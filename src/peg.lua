-- Generates a C header file and implementation from the given grammar
-- description.

--------------------------------------------------------------------------------

table.unpack = table.unpack or _G.unpack
_G.unpack = nil

setmetatable(_G, {
	__index = function(_, key)
		error("read from undefined global `" .. tostring(key) .. "`", 2)
	end,
	__newindex = function(_, key)
		error("write to undefined global `" .. tostring(key) .. "`", 2)
	end,
})

--------------------------------------------------------------------------------

local specialRepresentation = {
	["\a"] = [[\a]],
	["\b"] = [[\b]],
	["\f"] = [[\f]],
	["\n"] = [[\n]],
	["\r"] = [[\r]],
	["\t"] = [[\t]],
	["\v"] = [[\v]],
	["\\"] = [[\\]],
	["\""] = [[\"]],
	["\0"] = [[\0]],
}
for i = 0, 31 do
	local c = string.char(i)
	if not specialRepresentation[c] then
		local digit = tostring(i)
		specialRepresentation[c] = string.format("\\%03d", i)
	end
end
for i = 128, 255 do
	specialRepresentation[string.char(i)] = "\\" .. tostring(i)
end

-- RETURNS nothing
-- MODIFIES out by appending strings to it
local function showAdd(object, indent, out)
	if indent > 12 then
		table.insert(out, "...")
	elseif type(object) == "string" then
		-- Turn into a string literal
		table.insert(out, [["]])
		for character in object:gmatch "." do
			table.insert(out, specialRepresentation[character] or character)
		end
		table.insert(out, [["]])
	elseif type(object) == "table" then
		local internal = {}
		table.insert(out, "{")
		for key, value in pairs(object) do
			local line = {}
			table.insert(line, "\n" .. string.rep("\t", indent) .. "\t[")
			showAdd(key, indent + 1, line)
			table.insert(line, "] = ")
			if rawequal(key, "location") and type(value) == "table" then
				if value.file and value.from and value.to then
					table.insert(line, ("<%s:%d:%d-%d:%d>"):format(
						value.file.filename,
						value.from.line,
						value.from.column,
						value.to.line,
						value.to.column
					))
				else
					showAdd(value, indent + 1, line)
				end
			else
				showAdd(value, indent + 1, line)
			end
			table.insert(line, ",")
			table.insert(internal, table.concat(line))
		end
		table.sort(internal)
		for i = 1, #internal do
			if #internal > 1000 and i > 3 and i <= #internal - 3 then
				local el = "\n" .. string.rep("\t", indent) .. "...;"
				if out[#out] ~= el then
					table.insert(out, el)
				end
			else
				local line = internal[i]
				table.insert(out, line)
			end
		end
		table.insert(out, "\n" .. string.rep("\t", indent) .. "}")
	else
		table.insert(out, tostring(object))
	end
end

-- RETURNS a nearly-valid Lua expression literal representing the
-- (acyclic) Lua value
local function show(value)
	local out = {}
	showAdd(value, 0, out)
	return table.concat(out)
end

--------------------------------------------------------------------------------

local PREFIX = "AST"

if #arg ~= 3 then
	io.stderr:write("Usage:\n\tlua peg.lua <grammar.peg> <outdir> <outname>")
	os.exit(1)
end

local grammarFileName = arg[1]
local grammarFile, err = io.open(grammarFileName, "rb")
if not grammarFile then
	io.stderr:write(err)
	os.exit(1)
end

local outdir = arg[2]
local outname = arg[3]
local outCFileName = outdir .. "/" .. outname .. ".c"
local outHFileName = outdir .. "/" .. outname .. ".h"

local outCFile, err = io.open(outCFileName, "wb")
if not outCFile then
	io.stderr:write(err)
	os.exit(1)
end

local outHFile, err = io.open(outHFileName, "wb")
if not outHFile then
	io.sterr:write(err)
	os.exit(1)
end

--------------------------------------------------------------------------------

local function emit(list, ss)
	for _, line in ipairs(ss) do
		assert(type(line[1]) == "string")
		local status, result = pcall(string.format, table.unpack(line))
		if not status then
			error(result .. ": `" .. line[1] .. "`")
		end
		table.insert(list, result)
	end
end

local headerTypes = {}
local headerFunctions = {}
local bodyTypes = {}
local bodyFunctions = {}

--------------------------------------------------------------------------------

local function getCharFromEscaped(x)
	assert(type(x) == "string" and #x == 1)

	local map = {
		n = "\n",
		r = "\r",
		t = "\t",
		["\""] = "\"",
		["]"] = "]",
		["["] = "[",
		["\\"] = "\\",
		["/"] = "/",
		["-"] = "-",
		["("] = "(",
		[")"] = ")",
		["^"] = "^",
	}
	assert(map[x], "unhandled escape `" .. x .. "`")
	return map[x]
end

--------------------------------------------------------------------------------

local function parseRegex(regex)
	assert(type(regex) == "string")
	assert(regex:sub(1, 1) == "/" and regex:sub(-1) == "/" and 3 <= #regex)

	-- Handle escaping special characters.
	local body = regex:sub(2, -2)
	local chars = {}
	local escaped = false
	for c in body:gmatch "." do
		if escaped then
			table.insert(chars, {escaped = getCharFromEscaped(c):byte()})
			escaped = false
		else
			if c == "\\" then
				escaped = true
			else
				table.insert(chars, {regular = c})
			end
		end
	end
	assert(not escaped, "unclosed regex")

	-- Handle grouping into pieces by () and [].
	local CLOSER = {["("] = ")", ["["] = "]"}
	local group = {}
	local stack = {}
	for _, c in ipairs(chars) do
		if (c.regular == "(" or c.regular == "[") and group.opening ~= "[" then
			-- No sub-groups in `[]` character classes.
			table.insert(stack, group)
			table.insert(group, {opening = c.regular})
			group = group[#group]
		elseif c.regular and c.regular == CLOSER[group.opening] then
			assert(#stack ~= 0, "unmatched `" .. c.regular .. "`")
			group = table.remove(stack)
		else
			table.insert(group, c)
		end
	end
	assert(#stack == 0, "unmatched `" .. tostring(group.opening) .. "`")

	-- Interpret individual groups.
	local function convert(group)
		assert(group.opening == "(" or group.opening == "[")
		assert(#group ~= 0, "group cannot be empty")
		if group.opening == "(" then
			-- A set of branches.
			local branches = {{}}
			for _, e in ipairs(group) do
				if e.regular == "|" then
					table.insert(branches, {})
				elseif e.regular == "?" or e.regular == "*" then
					local last = table.remove(branches[#branches])
					assert(last, "dangling `" .. e.regular .. "`")
					table.insert(branches[#branches], {
						modifier = e.regular,
						element = last,
					})
				elseif e.opening then
					-- A sub-group.
					table.insert(branches[#branches], convert(e))
				else
					-- A single character.
					local byte = e.escaped or e.regular:byte()
					assert(type(byte) == "number")
					table.insert(branches[#branches], {
						class = {
							lone = true,
							inverted = false,
							byte
						},
					})
				end
			end
			return {branches = branches}
		else
			-- A character class.
			local class = {inverted = false}
			for i, e in ipairs(group) do
				if i == 1 and e.regular == "^" then
					class.inverted = true
				elseif e.regular == "-" then
					assert(type(class[#class]) == "number", "unexpected `-`")
					class[#class] = {class[#class]}
				else
					assert(e.escaped or e.regular, show(e))
					local byte = e.escaped or e.regular:byte()
					assert(type(byte) == "number")
					if type(class[#class]) == "table" and #class[#class] == 1 then
						-- Finish a range.
						table.insert(class[#class], byte)
					else
						table.insert(class, byte)
					end
				end
			end
			local final = class[#class]
			assert(type(final) == "number" or #final == 2, "class cannot end with `-`")
			return {class = class}
		end
	end

	-- The top level behaves like a parenthesized unit.
	group.opening = "("
	local tree = convert(group)
	return tree
end

local function compileRegex(targetName, regex)
	assert(type(targetName) == "string")
	assert(targetName:match "[a-zA-Z][a-zA-Z0-9_]*")
	assert(type(regex) == "string")

	local tree = parseRegex(regex)
	local signature = string.format("static int32_t regex_%s(Blob const* blob, int32_t offset)", targetName)
	table.insert(bodyTypes, signature .. ";")
	table.insert(bodyTypes, "")

	local helperIndex = 0
	local helpers = {}

	-- Writes one or more functions to the `helper` table.
	-- RETURNS the name of the root helper function for this tree.
	local function compileTree(tree)
		assert(type(tree) == "table")
		helperIndex = helperIndex + 1
		local name = string.format("regex_%s_helper_%d", targetName, helperIndex)
		local opening = string.format("static inline int32_t %s(Blob const* blob, int32_t offset) {", name)

		if tree.branches then
			local subs = {}
			for i, branch in ipairs(tree.branches) do
				subs[i] = compileTree(branch)
			end

			if #subs == 1 then
				-- Shortcut.
				table.insert(helpers, opening)
				table.insert(helpers, string.format("\treturn %s(blob, offset);", subs[1]))
				table.insert(helpers, "}")
				table.insert(helpers, "")
				return name
			end

			-- Parse the first one that matches (zero or more bytes).
			table.insert(helpers, opening)
			table.insert(helpers, "\tint32_t component;")
			for _, sub in ipairs(subs) do
				table.insert(helpers, string.format("\tcomponent = %s(blob, offset);", sub))
				table.insert(helpers, "\tif (0 <= component) {")
				table.insert(helpers, "\t\treturn component;")
				table.insert(helpers, "\t}")
			end
			table.insert(helpers, "\treturn -1;")
			table.insert(helpers, "}")
			table.insert(helpers, "")

			return name
		elseif #tree ~= 0 then
			local subs = {}
			for i, element in ipairs(tree) do
				subs[i] = compileTree(element)
			end

			if #subs == 1 then
				-- Shortcut.
				table.insert(helpers, opening)
				table.insert(helpers, string.format("\treturn %s(blob, offset);", subs[1]))
				table.insert(helpers, "}")
				table.insert(helpers, "")
				return name
			end

			-- Parse a sequence.
			table.insert(helpers, opening)
			table.insert(helpers, "\tint32_t consumed = 0;")
			table.insert(helpers, "\tint32_t component;")
			for _, sub in ipairs(subs) do
				table.insert(helpers, string.format("\tcomponent = %s(blob, offset + consumed);", sub))
				table.insert(helpers, "\tif (component < 0) {")
				table.insert(helpers, "\t\treturn -1;")
				table.insert(helpers, "\t}")
				table.insert(helpers, "\tconsumed += component;")
			end
			table.insert(helpers, "\treturn consumed;")
			table.insert(helpers, "}")
			table.insert(helpers, "")
			
			return name
		elseif tree.class then
			assert(type(tree.class.inverted) == "boolean")
			assert(#tree.class ~= 0)

			table.insert(helpers, opening)
			table.insert(helpers, "\tif (blob->size <= offset) {")
			table.insert(helpers, "\t\treturn -1;")
			table.insert(helpers, "\t}")

			-- Parse a character class.
			local matching = tree.class.inverted and -1 or 1
			table.insert(helpers, "\t// matching " .. tostring(tree.class.inverted))
			table.insert(helpers, "\tint c = blob->data[offset];")
			for i, range in ipairs(tree.class) do
				local lead = i == 1 and "if" or "} else if"
				if type(range) == "number" then
					table.insert(helpers, string.format("\t%s (c == %d) {", lead, range))
					table.insert(helpers, string.format("\t\treturn %d;", matching))
				else
					assert(#range == 2)
					table.insert(helpers, string.format("\t%s (%d <= c && c <= %d) {", lead, range[1], range[2]))
					table.insert(helpers, string.format("\t\treturn %d;", matching))
				end
			end
			table.insert(helpers, "\t}")
			table.insert(helpers, string.format("\treturn %d;", -matching))
			table.insert(helpers, "}")
			table.insert(helpers, "")

			return name
		elseif tree.modifier then
			assert(tree.modifier == "*", "TODO: handle other modifiers")

			local sub = compileTree(tree.element)
			table.insert(helpers, opening)
			table.insert(helpers, "\tint32_t consumed = 0;")
			table.insert(helpers, "\twhile (1) {")
			table.insert(helpers, string.format("\t\tint32_t component = %s(blob, offset + consumed);", sub))
			table.insert(helpers, "\t\tif (component <= 0) {")
			table.insert(helpers, "\t\t\treturn consumed;")
			table.insert(helpers, "\t\t}")
			table.insert(helpers, "\t\tconsumed += component;")
			table.insert(helpers, "\t}")
			table.insert(helpers, "}")
			table.insert(helpers, "")

			return name
		end

		error("unhandled tree")
	end

	local root = compileTree(tree)

	table.insert(bodyFunctions, table.concat(helpers, "\n"))
	table.insert(bodyFunctions, signature .. " {")
	table.insert(bodyFunctions, string.format("\treturn %s(blob, offset);", root))
	table.insert(bodyFunctions, "}")
	table.insert(bodyFunctions, "")
end

local function compileAST(name)
	-- Forward declare type.
	emit(headerTypes, {
		{"typedef struct {"},
		{"\t%sParse* parse;", PREFIX},
		{"\tint32_t index;"},
		{"} %s;", name},
		{""},
	})

	-- A parser returns the number of tokens it consumed (-1 for failure; 0 or more means success).
	-- The index to construct the AST is the Parse_ptr immediately after parsing, ie, the index
	-- of the slot immediately *following* the object.
	-- An error is written to error if a syntax error is detected.
	local parseArgs = PREFIX .. "Parse* parse, int32_t from, Error* error"
	local parseSignature = string.format("static int32_t %s_parse(%s)", name, parseArgs)
	return parseSignature
end

-- Compiles choice trees.
local function compileChoice(choice)
	assert(#choice.fields ~= 0)

	local parseSignature = compileAST(choice.name)
	table.insert(bodyTypes, string.format("%s;", parseSignature))
	table.insert(bodyTypes, "")

	-- Generate internal parser.
	emit(bodyFunctions, {
		{"%s {", parseSignature},
		{"\tint32_t consumed;"},
	})
	for i, field in ipairs(choice.fields) do
		emit(bodyFunctions, {
			{"\tconsumed = %s_parse(parse, from, error);", field.typeName},
			{"\tif (consumed >= 0) {"},
			{"\t\t%sParse_push(parse, %d);", PREFIX, i},
			{"\t\treturn consumed;"},
			{"\t}"},
		})
	end
	emit(bodyFunctions, {
		{"\treturn -1;"},
		{"}"},
		{""},
	})
	-- Generate getters.
	for i, field in ipairs(choice.fields) do
		emit(headerFunctions, {
			{"inline int %s_is_%s(%s ast) {", choice.name, field.name, choice.name},
			{"\treturn ast.parse->data[ast.index - 1] == %d;", i},
			{"}"},
			{""},
		})

		emit(headerFunctions, {
			{"inline %s %s_%s(%s ast) {", field.typeName, choice.name, field.name, choice.name},
			{"\treturn (%s){.parse=ast.parse, .index=ast.index - 1};", field.typeName},
			{"}"},
			{""},
		})
	end
end

-- Compiles sequence trees.
local function compileSequence(sequence)
	assert(type(sequence.name) == "string")
	assert(#sequence.fields ~= 0)

	local parseSignature = compileAST(sequence.name)
	table.insert(bodyTypes, string.format("%s;", parseSignature))
	table.insert(bodyTypes, "")

	-- Generate internal parser.
	emit(bodyFunctions, {
		{"\%s {", parseSignature},
		{"\tint32_t origin = %sParse_ptr(parse);", PREFIX},
		{"\tint32_t consumed = 0;"},
		{""},
	})

	-- Parse the fields.
	for i, field in ipairs(sequence.fields) do
		if field.modifier then
			-- Parse into a linked list, then follow the links back to make
			-- a contiguous array.
			-- [# # #][a:-1][# # #][b:&a][# # #][c:&b][&c][&b][arrayi:&a]
			local maxCond = field.modifier.mark == "?" and string.format("count%d < 1", i) or "1"
			local minCond = field.modifier.mark == "+" and string.format("count%d < 1", i) or "0"
			emit(bodyFunctions, {
				{"\tint32_t count%d = 0;", i},
				{"\tint32_t linked%d = -1;", i},
				{"\twhile (%s) {", maxCond},
				{"\t\tint32_t c = %s_parse(parse, from + consumed, error);", field.typeName},
				{"\t\tif (c < 1) {"},
				{"\t\t\tbreak;"},
				{"\t\t}"},
				{"\t\tlinked%d = %sParse_push(parse, linked%d);", i, PREFIX, i},
				{"\t\tcount%d++;", i},
				{"\t\tconsumed += c;"},
				{"\t}"},
				{"\tif (%s) {", minCond},
				{"\t\tgoto fail;"},
				{"\t}"},
				{"\tfor (int32_t k = 0; k < count%d; k++) {", i},
				{"\t\t%sParse_push(parse, linked%d);", PREFIX, i},
				{"\t\tlinked%d = parse->data[linked%d];", i, i},
				{"\t}"},
				{"\tint32_t array%d = %sParse_ptr(parse) - 1;", i, PREFIX},
				{""},
			})
		else
			emit(bodyFunctions, {
				{"\tint32_t consumed%d = %s_parse(parse, from + consumed, error);", i, field.typeName, i},
				{"\tif (consumed%d < 0) {", i},
				{"\t\tgoto fail;"},
				{"\t}"},
				{"\tint32_t result%d = %sParse_ptr(parse);", i, PREFIX},
				{"\tconsumed += consumed%d;", i},
				{""},
			})
		end
	end

	-- Emit the field pointers.
	local treeSize = 0
	for i, field in ipairs(sequence.fields) do
		if field.modifier then
			-- Follow the linked list
			emit(bodyFunctions, {
				{"\t%sParse_push(parse, count%d);", PREFIX, i},
				{"\t%sParse_push(parse, array%d);", PREFIX, i},
			})
			treeSize = treeSize + 2
		else
			emit(bodyFunctions, {
				{"\t%sParse_push(parse, result%d);", PREFIX, i},
			})
			treeSize = treeSize + 1
		end
	end

	emit(bodyFunctions, {
		{"\treturn consumed;"},
		{"fail:"},
		{"\t%sParse_reset(parse, origin);", PREFIX},
		{"\treturn -1;"},
		{"}"},
		{""},
	})

	-- Generate getters.
	local parseOffset = 0
	for i, field in ipairs(sequence.fields) do
		assert(type(field.typeName) == "string")

		if field.modifier and field.modifier.mark:match "[+*?]" then
			if field.name ~= "_" then
				emit(headerFunctions, {
					{"inline int32_t %s_%s_count(%s ast) {", sequence.name, field.name, sequence.name},
					{"\tif (ast.index < 0) {"},
					{"\t\treturn 0;"},
					{"\t}"},
					{"\treturn ast.parse->data[ast.index - %d + %d];", treeSize, parseOffset},
					{"}"},
					{""},
				})
				emit(headerFunctions, {
					{"inline %s %s_%s(%s ast, int32_t i) {", field.typeName, sequence.name, field.name, sequence.name},
					{"\tassert(0 <= ast.index);"},
					{"\tint32_t size = ast.parse->data[ast.index - %d + %d];", treeSize, parseOffset},
					{"\tassert(0 <= i && i < size);"},
					{"\tint32_t array = ast.parse->data[ast.index - %d + %d];", treeSize, parseOffset + 1},
					{"\treturn (%s){.parse=ast.parse, .index=ast.parse->data[array + size - i]};", field.typeName},
					{"}"},
					{""},
				})
			end
			
			-- One for the length and one for the array pointer.
			parseOffset = parseOffset + 2
		elseif not field.modifier then
			if field.name ~= "_" then
				emit(headerFunctions, {
					{"inline %s %s_%s(%s ast) {", field.typeName, sequence.name, field.name, sequence.name},
					{"\tassert(0 <= ast.index);"},
					{"\treturn (%s){.parse=ast.parse, .index=ast.parse->data[ast.index - %d + %d]};", field.typeName, treeSize, parseOffset},
					{"}"},
					{""},
				})
			end

			parseOffset = parseOffset + 1
		else
			error "unhandled"
		end
	end
end

--------------------------------------------------------------------------------

local regexDefinitions = {}
local tokenOrder = {}
local astDefinitions = {}

local tokenTags = {}
local keywords = {}

-- Parse AST and token definitions from the grammar file.
local lines = {}
local count = 0
for line in grammarFile:lines() do
	count = count + 1
	if line:sub(1, 1) == "#" then
		-- Ignore comments
	elseif line:find "%S" then
		table.insert(lines, {count = count, text = line})
	end
end

local owner = false
for _, line in ipairs(lines) do
	local location = string.format("%s:%d", grammarFileName, line.count)

	local text = line.text
	local tokenDef = {text:match "^\t([-]?)([a-z_]+)%s*:=%s*(/.*/)$"}
	local astDef = {text:match "^\t(%$?)([A-Z][A-Za-z]+)%s*:=$"}
	local continuation = {text:match "^\t\t([|]?)([a-z_]+):%s*(.*)$"}

	if tokenDef[1] then
		-- Defining a token using a regex-like syntax.
		owner = false
		local name = tokenDef[2]
		table.insert(tokenTags, {
			enum = "T_" .. name:upper(),
			matcher = "regex_" .. name,
			keep = tokenDef[1] ~= "-",
		})

		if regexDefinitions[name] then
			io.stderr:write(string.format("Token `%s` already defined at %s redefined at %s.\n", name, regexDefinitions[name].location, location))
			io.stderr:flush()
			os.exit(1)
		end

		regexDefinitions[name] = {
			regex = tokenDef[3],
			whitespace = tokenDef[1] ~= "",
			location = location,
		}
		table.insert(tokenOrder, name)
	elseif astDef[1] then
		-- Defining an AST type, either as a struct or a union.
		owner = astDef[2]
		if astDefinitions[owner] then
			io.stderr:write(string.format("AST `%s` already defined at %s redefined at %s.\n", owner, astDefinitions[owner].location, location))
			io.stderr:flush()
			os.exit(1)
		end

		astDefinitions[owner] = {
			public = astDef[1] ~= "",
			mode = false,
			list = {},
			location = location,
			typeName = PREFIX .. owner,
		}
	elseif continuation[1] then
		if not owner then
			io.stderr:write("Expected a new definition at " .. location .. ".\n")
			io.stderr:flush()
			os.exit(1)
		end

		local mode = continuation[1] == "" and "seq" or "choice"
		if not astDefinitions[owner].mode then
			astDefinitions[owner].mode = mode
		elseif astDefinitions[owner].mode ~= mode then
			io.stderr:write("Inconsistent AST mode at " .. locaiton .. ".\n")
			io.stderr:flush()
			os.exit(1)
		end

		local name = continuation[2]
		table.insert(astDefinitions[owner].list, {
			name = name,
			def = continuation[3],
			location = location,
		})
	else
		io.stderr:write("Bad syntax at " .. location .. ": `" .. text .. "`\n")
		io.stderr:flush()
		os.exit(1)
	end
end

local function getKeywordParser(str)
	assert(type(str) == "string")
	assert(str:sub(1, 1) == "\"" and 3 <= #str)
	
	local cname = str:sub(2, -2):gsub("[^a-zA-Z0-9]+", function(x)
		return "d" .. x:byte() .. "b_"
	end)
	if not keywords[str:sub(2, -2)] then
		keywords[str:sub(2, -2)] = true
		table.insert(tokenTags, 1, {
			keyword = true,
			literal = str,
			enum = "K_" .. cname,
		})
	end

	return "K_" .. cname
end

-- Compile AST parsers.
for name, def in pairs(astDefinitions) do
	local fields = {}
	local fieldMap = {}
	for _, field in ipairs(def.list) do
		if field.name ~= "_" and fieldMap[field.name] then
			io.stderr:write("Field `" .. field.name .. "` first defined at " .. fieldMap[field.name] .. " redefined at " .. field.location .. ".\n")
			io.stderr:flush()
			os.exit(1)
		end
		fieldMap[field.name] = field.location

		local body = false
		local modifier = false
		if field.def:sub(-1):match "[*+?]" then
			-- A repetition modifier.
			if field.def:sub(-2, -2) == "~" and field.def:sub(-1):match "[+*]" then
				-- A "comma" modifier that separates repetitions.
				local sepBegin = field.def:find "(\".*\")~[+*]$"
				if not sepBegin then
					io.stderr:write("Bad separated repetition syntax at " .. field.location .. ".\n")
					io.stderr:flush()
					os.exit(1)
				end

				modifier = {
					mark = field.def:sub(-1),
					separator = getKeywordParser(field.def:sub(sepBegin, -3)),
				}
				body = field.def:sub(1, sepBegin - 1)
			else
				modifier = {
					mark = field.def:sub(-1),
					separator = false,
				}
				body = field.def:sub(1, -2)
			end
		else
			body = field.def
		end
		
		if body:sub(1, 1) == "\"" then
			-- Keyword token.
			assert(body:match "^\"[^\"]+\"$")

			table.insert(fields, {
				name = field.name,
				type = "token",
				typeName = PREFIX .. "_" .. getKeywordParser(body),
				modifier = modifier,
				enum = getKeywordParser(body),
			})
		else
			-- Token or AST.
			if not body:match "^[a-zA-Z][_a-zA-Z0-9]*$" then
				io.stderr:write("Bad syntax at " .. field.location .. ".\n")
				io.stderr:flush()
				os.exit(1)
			end

			if body:sub(1, 1):upper() == body:sub(1, 1) then
				-- AST.
				if not astDefinitions[body] then
					io.stderr:write("No AST called `" .. body .. "` has been defined, but it's used at " .. field.location .. ".\n")
					io.stderr:flush()
					os.exit(1)
				end

				table.insert(fields, {
					name = field.name,
					typeName = PREFIX .. body,
					type = "ast",
					modifier = modifier,
					f = PREFIX .. body .. "_parse",
				})
			else
				-- Token.
				if not regexDefinitions[body] then
					io.stderr:write("No token called `" .. body .. "` has been defined, but it's used at " .. field.location .. ".\n")
					io.stderr:flush()
					os.exit(1)
				end

				local enum = "T_" .. body:upper()
				table.insert(fields, {
					name = field.name,
					typeName = PREFIX .. "_" .. enum,
					type = "token",
					modifier = modifier,
					enum = enum
				})
			end
		end
	end

	if def.mode == "seq" then
		compileSequence {
			name = PREFIX .. name,
			fields = fields,
		}
	elseif def.mode == "choice" then
		compileChoice {
			name = PREFIX .. name,
			fields = fields,
		}
	else
		error "unhandled"
	end
end

-- Compile regex parsers.
for name, def in pairs(regexDefinitions) do
	compileRegex(name, def.regex)
end

-- Emit "AST" types for individual tokens.
for _, token in ipairs(tokenTags) do
	local treeName = string.format("%s_%s", PREFIX, token.enum)
	emit(headerTypes, {
		{"typedef struct {"},
		{"\t%sParse* parse;", PREFIX},
		{"\tint32_t index;"},
		{"} %s;", treeName},
		{""},
	})

	emit(bodyTypes, {
		{"static inline int32_t %s_parse(%sParse* parse, int32_t from, Error* error) {", treeName, PREFIX},
		{"\t(void)error;"},
		{"\tif (from < parse->token_count && parse->token_tags[from] == %s) {", token.enum},
		{"\t\treturn 1;"},
		{"\t}"},
		{"\treturn -1;"},
		{"}"},
		{""},
	})
end

-- Publish any public parsers.
emit(bodyFunctions, {
	{"static %sParse* %sParse_new(Blob const* blob, Error* error);", PREFIX, PREFIX},
	{""},
})
for name, def in pairs(astDefinitions) do
	if def.public then
		local signature = string.format("%s %s_from_blob(Blob const* blob)", def.typeName, def.typeName)
		emit(headerFunctions, {
			{"%s;", signature},
			{""},
		})

		--[[
	Error tokenError = (Error){0};
	ASTParse* parse = ASTParse_new(blob, &tokenError);
	if (parse == NULL) {
		Error_render_colorful(&tokenError, stderr);
		exit(1);
	}
		]]

		emit(bodyFunctions, {
			{"%s {", signature},
			{"\tError error = (Error){0};"},
			{"\t%sParse* parse = %sParse_new(blob, &error);", PREFIX, PREFIX},
			{"\tif (parse == NULL) {"},
			{"\t\treturn (%s){NULL, -1};", def.typeName},
			{"\t}"},
			{"\tint32_t consumed = %s_parse(parse, 0, &error);", def.typeName},
			{"\tif (consumed < 0) {"},
			{"\t\treturn (%s){NULL, -1};", def.typeName},
			{"\t}"},
			{"\tif (consumed < parse->token_count) {"},
			{"\t\tError_text(&error, \"There is unexpected content\");"},
			{"\t\tError_at_location(&error, (Location){blob, parse->token_offsets[consumed], parse->token_offsets[consumed] + parse->token_lengths[consumed]});"},
			{"\t\treturn (%s){NULL, -1};", def.typeName},
			{"\t}"},
			{"\treturn (%s){.parse=parse, .index=parse->size};", def.typeName},
			{"}"},
			{""},
		})
	end
end

--------------------------------------------------------------------------------

-- Write out the files.
local guard = "_" .. outHFileName:upper():gsub("[^A-Za-z0-9]", "_")
outHFile:write("// THIS FILE WAS GENERATED BY peg.lua.\n")
outHFile:write("#ifndef " .. guard .. "\n")
outHFile:write("#define " .. guard .. "\n")
outHFile:write([==[
#include "../parser.h"

typedef struct {
	Blob const* blob;

	// Token data.
	int32_t token_count;
	int32_t* token_tags;
	int32_t* token_offsets;
	int32_t* token_lengths;

	// AST data.
	int32_t* data;
	int32_t size;
	int32_t capacity;
} ]==] .. PREFIX .. [==[Parse;

]==])


outCFile:write("// THIS FILE WAS GENERATED BY peg.lua.\n")
outCFile:write(string.format("#include \"%s.h\"\n\n", outname))
outCFile:write("#include \"assert.h\"\n")
outCFile:write("#include \"stdint.h\"\n")
outCFile:write("#include \"stdlib.h\"\n")
outCFile:write("#include \"string.h\"\n\n")
outCFile:write("typedef enum {\n\tTAG_INVALID,\n")
for _, tag in ipairs(tokenTags) do
	outCFile:write("\t" .. tag.enum .. ",\n")
end
outCFile:write("} TokenTag;\n\n")
outCFile:write((([==[
static int32_t $Parse_ptr($Parse* parse) {
	return parse -> size;
}

static void $Parse_reset($Parse* parse, int32_t to) {
	parse->size = to;
}

static int32_t $Parse_push($Parse* parse, int32_t value) {
	if (parse->size == parse->capacity) {
		// Reallocate.
		int32_t* newdata = (int32_t*)malloc(4 * parse->capacity * sizeof(int32_t));
		memcpy(newdata, parse->data, sizeof(int32_t) * parse->capacity);
		free(parse->data);
		parse->data = newdata;
	}

	parse->data[parse->size] = value;
	parse->size++;
	return parse->size - 1;
}

]==]):gsub("%$", PREFIX)))

outCFile:write("\n")

for _, line in ipairs(headerTypes) do
	outHFile:write(line)
	outHFile:write("\n")
end

for _, line in ipairs(headerFunctions) do
	outHFile:write(line)
	outHFile:write("\n")
end

for _, line in ipairs(bodyTypes) do
	outCFile:write(line)
	outCFile:write("\n")
end

outCFile:write(string.rep("/", 80) .. "\n\n")

for _, line in ipairs(bodyFunctions) do
	outCFile:write(line)
	outCFile:write("\n")
end

outCFile:write((([==[
static $Parse* $Parse_new(Blob const* blob, Error* error) {
	$Parse* parse = malloc(sizeof($Parse));
	if (parse == NULL) {
		return NULL;
	}
	parse->blob = blob;
	parse->size = 0;
	parse->capacity = 1024;
	parse->data = (int32_t*)malloc(sizeof(int32_t) * parse->capacity);

	// Tokenize
	int32_t offset = 0;
	parse->token_count = 0;
	int32_t token_capacity = 1;
	while (offset < blob->size) {
		token_capacity *= 2;
		int32_t* tags = (int32_t*)malloc(sizeof(int32_t) * token_capacity);
		int32_t* offsets = (int32_t*)malloc(sizeof(int32_t) * token_capacity);
		int32_t* lengths = (int32_t*)malloc(sizeof(int32_t) * token_capacity);
		if (tags == NULL || offsets == NULL || lengths == NULL) {
			free(tags);
			free(offsets);
			free(lengths);
			free(parse);
			return NULL;
		}

		if (parse->token_count != 0) {
			memcpy(tags, parse->token_tags, sizeof(int32_t) * parse->token_count);
			memcpy(offsets, parse->token_offsets, sizeof(int32_t) * parse->token_count);
			memcpy(lengths, parse->token_lengths, sizeof(int32_t) * parse->token_count);
			free(parse->token_tags);
			free(parse->token_offsets);
			free(parse->token_lengths);
		}
		parse->token_tags = tags;
		parse->token_offsets = offsets;
		parse->token_lengths = lengths;

		// Take the earliest, longest-matching token.
		while (parse->token_count < token_capacity) {
			if (offset == blob->size) {
				break;
			}

			int32_t length = 0;
			int keep;
			TokenTag tag;
			int32_t component;
]==]):gsub("%$", PREFIX)))

for _, tag in ipairs(tokenTags) do
	if not tag.keyword then
		outCFile:write(string.format("\t\t\tcomponent = %s(blob, offset);\n", tag.matcher))
		outCFile:write("\t\t\tif (length < component) {\n")
		outCFile:write(string.format("\t\t\t\tkeep = %d;\n", tag.keep and 1 or 0))
		outCFile:write("\t\t\t\tlength = component;\n")
		outCFile:write(string.format("\t\t\t\ttag = %s;\n", tag.enum))
		outCFile:write("\t\t\t}\n")
	end
end

outCFile:write("\n\t\t\t// Switch generic tags to specific keywords.\n")
for _, tag in ipairs(tokenTags) do
	if tag.keyword then
		outCFile:write(string.format("\t\t\tif (length == %d && memcmp(blob->data + offset, %s, %d) == 0) {\n", #tag.literal - 2, tag.literal, #tag.literal - 2))
		outCFile:write(string.format("\t\t\t\ttag = %s;\n", tag.enum))
		outCFile:write("\t\t\t}\n")
	end
end

outCFile:write [==[

			if (length == 0) {
				// Report a bad token error.
				Error_text(error, "There is no valid token to parse");
				int32_t end = offset + 1;
				if (blob->size < end) {
					end = blob->size;
				}
				Error_at_location(error, (Location){blob, offset, end});
				return NULL;
			}

			if (keep) {
				// Don't record whitespace, comments, etc in the token stream.
				tags[parse->token_count] = tag;
				lengths[parse->token_count] = length;
				offsets[parse->token_count] = offset;
				parse->token_count++;
			}

			offset += length;
		}
	}
	return parse;
}
]==]

-- Flush and close them.
outHFile:write("#endif\n")
outHFile:close()
outCFile:close()
