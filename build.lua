-- build.lua
-- A build and test script for Smol's compiler.

-- # PREREQUISITES
-- a) A version of Lua. Lua 5.1, Lua 5.2, Lua 5.3, and LuaJIT (based on 5.1/5.2)
--    are all supported.
-- b) A C compiler. Currently `gcc` and `clang` (TODO) are supported.
-- c) A "standard" shell. Windows Command Prompt, Windows Powershell,
--    and Ubuntu Bash are all supported.

-- # HOW TO BUILD SMOL
-- In your shell, run
--     lua build.lua
-- This will run the default `build` target. You could instead explicitly
-- execute
--     lua build.lua build
-- This will build Smol, and produce a file, `smolc`, in the current working
-- directory.
-- If building is successful, you can run tests.
-- You can run the `clean` target to wipe away any generated files.
-- If you've gotten your local build into a bad state, try running this.
--     lua build.lua clean

-- # HOW TO RUN SMOLC'S TESTS
-- Run tests by invoking the `test` target.
--     lua build.lua test
-- This will invoke smolc on each of the test cases in the tests-positive and
-- tests-negative directories.
-- The output of smolc is compared to the known-correct output for each test.
-- The test script will conclude by stating how many tests passed and how many
-- tests failed.

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

local function fail(message, ...)
	io.stderr:write(message:format(...) .. "\n")
	io.stderr:flush()
	os.exit(1)
end

--------------------------------------------------------------------------------

local shell = {}

-- RETURNS the string `str` wrapped in quotes so that it counts as a single
-- argument in the shell.
function shell.quoted(str)
	assert(type(str) == "string")

	return string.format("\"%s\"", str:gsub("\"", "\\\""))
end

-- RUNS an arbitrary program with the given arguments. Arguments are quoted
-- so that they become individual words (but globbing, etc. is not currently
-- escaped!)
-- RETURNS true when the command reports success, otherwise false.
function shell.arbitrary(program, arguments)
	assert(type(program) == "string")
	assert(type(arguments) == "table")

	local cmd = {program}
	for _, a in ipairs(arguments) do
		table.insert(cmd, shell.quoted(a))
	end

	-- In Lua 5.1, os.execute returns a status code (0 usually means success).
	-- In Lua 5.2, os.execute returns success as a boolean.
	local result = os.execute(table.concat(cmd, " "))
	return result == 0 or result == true
end

-- RUNS gcc with the given arguments.
-- SEE shell.arbitrary.
function shell.gcc(arguments)
	assert(type(arguments) == "table")

	return shell.arbitrary("gcc", arguments)
end

-- RUNS Lua with the given arguments.
-- SEE shell.arbitrary.
function shell.lua(arguments)
	assert(type(arguments) == "table")

	return shell.arbitrary("lua", arguments)
end

-- RUNS a shell command to delete the given file.
function shell.delete(file)
	assert(type(file) == "string")

	return shell.arbitrary("rm", {file})
end

function shell.list(directory)
	assert(type(directory) == "string")

	error "TODO"
end

--------------------------------------------------------------------------------

local targets = {}

function targets.build()
	-- (0) Make sure the environment is sane.
	if not shell.gcc {"--version"} then
		io.stderr:write("Could not invoke `gcc`.\n")
		return 1
	elseif not shell.lua {"-v"} then
		io.stderr:write("Could not invoke `lua`\n.")
		return 1
	end

	-- (1) Compile the grammar.
	if not shell.lua {"src/peg.lua", "src/grammar/smol.peg", "src/c/gen", "grammar"} then
		return 1
	end

	-- (2) Run the C compiler.
	shell.gcc {"-pedantic", "-O0", "-g3", "-std=c99", "-Wall", "-Wextra", "src/c/gen/grammar.c", "src/c/parser.c", "src/c/main.c", "-o", "smolc"}
	return 0
end

function targets.clean()
	shell.delete("src/c/gen/grammar.c")
	shell.delete("src/c/gen/grammar.h")
	shell.delete("smolc")
end

function targets.test()
	error "TODO: test target"
end

--------------------------------------------------------------------------------

local arguments = arg
local target = arg[1]
if not target then
	target = "build"
end

if not targets[target] then
	local available = {}
	for k in pairs(targets) do
		table.insert(available, k)
	end
	table.sort(available)
	available = "\t" .. table.concat(available, "\n\t")
	fail("No such target `%s`.\nAvailable targets:\n%s", target, available)
end

local code = targets[target]()
io.stderr:flush()
os.exit(code)
