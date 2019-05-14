// parser.c
#include "parser.h"

#include "assert.h"
#include "stdlib.h"
#include "string.h"

#include "stdio.h"

#define TAB_SIZE 8

Blob* Blob_from_file(FILE* file, char const* name) {
	size_t capacity = 4; // * 1024;

	char* data = (char*)malloc(capacity);
	if (data == NULL) {
		return NULL;
	}
	char* target = data;

	while (1) {
		size_t consumed = fread(target, 1, capacity - (target - data), file);
		target += consumed;
		if (feof(file)) {
			Blob* blob = malloc(sizeof(Blob));
			if (blob == NULL) {
				free(data);
				return NULL;
			}
			blob->data = data;
			blob->size = target - data;
			blob->name = name;
			return blob;
		} else if (consumed == 0) {
			size_t new_capacity = 2 * capacity;
			char* new_data = (char*)malloc(new_capacity);
			if (new_data == NULL) {
				free(data);
				return NULL;
			}

			size_t size = target - data;
			memcpy(new_data, data, size);
			free(data);
			data = new_data;
			capacity = new_capacity;
			target = data + size;
		}
	}
}

Blob* Blob_from_c_str(char const* data, char const* name) {
	Blob* blob = malloc(sizeof(blob));
	if (blob == NULL) {
		return NULL;
	}
	blob->size = strlen(data);
	char* copy = malloc(1 + blob->size);
	if (copy == NULL) {
		free(blob);
		return NULL;
	}
	memcpy(copy, data, blob->size + 1);
	blob->data = copy;
	blob->name = name;
	return blob;
}

void Blob_destroy(Blob* blob) {
	free(blob->data);
	free(blob);
}

Location Location_span(Location begin, Location end) {
	assert(begin.blob == end.blob);
	assert(begin.begin <= end.end);
	return (Location){
	    .blob = begin.blob,
	    .begin = begin.begin,
	    .end = end.end,
	};
}

typedef enum {
	EF_NONE = 0,
	EF_TEXT = 1,
	EF_CODE = 2,
	EF_LOC_ERR = 3,
	EF_LOC_REF = 4,
} ErrorFormat;

void Error_text(Error* error, char const* text) {
	error->modes[error->count] = EF_TEXT;
	int32_t length = strlen(text);
	assert(length < 1024);
	error->lengths[error->count] = length;
	memcpy(&error->messages[error->count], text, length);

	error->count++;
}

void Error_at_location(Error* error, Location location) {
	error->modes[error->count] = EF_LOC_ERR;
	error->locations[error->count] = location;

	error->count++;
}

void Error_refer_at_location(Error* error, Location location) {
	error->modes[error->count] = EF_LOC_REF;
	error->locations[error->count] = location;

	error->count++;
}

typedef enum {
	ANSI_REGULAR = 0,
	ANSI_BOLD = 1,
	ANSI_BLUE = 34,
	ANSI_RED = 31,
	ANSI_CYAN = 36,
	ANSI_GRAY = 30,
} ANSIColor;

static void ANSI_write_color(FILE* file, ANSIColor color) {
	fprintf(file, "\x1B[1m\x1B[%dm", (int)color);
}

static void Location_print_position(Location const* location, FILE* file) {
	int startLine = -1;
	int startColumn = -1;
	int endLine = 1;
	int endColumn = 1;

	for (int i = 0; i < location->end; i++) {
		if (i == location->begin) {
			startLine = endLine;
			startColumn = endColumn;
		}

		char c = location->blob->data[i];
		if (c == '\n') {
			endLine++;
			endColumn = 1;
		} else if (c == '\t') {
			endColumn += TAB_SIZE;
			endColumn -= endColumn % TAB_SIZE;
		} else {
			endColumn++;
		}
	}

	fprintf(file, "%s:%d:%d-%d:%d", location->blob->name, startLine,
	        startColumn, endLine, endColumn);
}

// Write output that looks like this:
//          3 | class Out {
//          4 |     foreign static println!(message String) Unit;
//            |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//          5 | }
static void Location_print_excerpt(Location const* location, FILE* file) {
	const int LINE_NUM_WIDTH = 10;

	int32_t line = 0;
	int32_t lineBegin = -1;
	int32_t lineEnd = -1;
	for (int32_t i = 0; i < location->blob->size; i++) {
		if (i == location->begin) {
			lineBegin = line;
		}
		if (i == location->end) {
			lineEnd = line;
		}
		if (location->blob->data[i] == '\n') {
			line++;
		}
	}

	line = 0;
	int32_t column = 0;
	int returned = 1;
	int needsCarets = 0;
	int caretLow = -1;
	int caretHigh = -1;
	for (int32_t i = 0; i <= location->blob->size; i++) {
		int drawing = lineBegin - 1 <= line && line <= lineEnd + 1;
		if (returned || i == location->blob->size) {
			if (needsCarets) {
				fprintf(file, "%*s | ", LINE_NUM_WIDTH, "");
				fprintf(file, "%*s", caretLow, "");
				ANSI_write_color(file, ANSI_RED);
				for (int k = caretLow; k <= caretHigh; k++) {
					fputc('^', file);
				}
				ANSI_write_color(file, ANSI_REGULAR);
				fputc('\n', file);
			}
			if (drawing) {
				fprintf(file, "%*d | ", LINE_NUM_WIDTH, line + 1);
			}
			returned = 0;
			needsCarets = 0;
			caretLow = -1;
			caretHigh = -1;
		}

		if (i == location->blob->size) {
			break;
		}

		char c = location->blob->data[i];
		if (c == '\n') {
			column = 0;
			line++;
			returned = 1;
			if (drawing) {
				fputc('\n', file);
			}
		} else if (c == '\t') {
			int32_t was = column;
			column = column + TAB_SIZE - column % TAB_SIZE;
			int32_t width = column - was;
			if (drawing) {
				fprintf(file, "%*s", width, "");
			}
		} else if (c == '\r') {
			// Do nothing.
		} else {
			if (drawing) {
				char draw = ('!' <= c && c <= '~') ? c : ' ';
				fputc(draw, file);
				if (draw != ' ' && location->begin <= i && i < location->end) {
					if (!needsCarets) {
						caretLow = column;
					}
					caretHigh = column;
					needsCarets = 1;
				}
			}
			column++;
		}
	}
}

void Error_render_colorful(Error* error, FILE* file) {
	ANSI_write_color(file, ANSI_RED);
	fputs("ERROR: ", file);
	ANSI_write_color(file, ANSI_REGULAR);
	assert(0 < error->count);
	for (int i = 0; i < error->count; i++) {
		ErrorFormat mode = error->modes[i];
		if (mode == EF_TEXT) {
			fwrite(&error->messages[i], 1, error->lengths[i], file);
		} else if (mode == EF_LOC_ERR || mode == EF_LOC_REF) {
			Location* location = &error->locations[i];
			fputs(" at ", file);
			ANSI_write_color(file, ANSI_RED);
			Location_print_position(location, file);
			ANSI_write_color(file, ANSI_REGULAR);
			fputs(":\n", file);
			Location_print_excerpt(location, file);
		} else {
			fprintf(stderr, "`%d`?", mode);
			assert(0 && "unhandled ErrorFormat.");
		}
	}
	fflush(file);
}

int Error_has_problem(Error const* error) { return error->count; }
