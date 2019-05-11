// parser.h

#ifndef _PARSER_H
#define _PARSER_H

#include "assert.h"
#include "stdint.h"
#include "stdio.h"

// Blob is the body of a source file.
typedef struct {
	int32_t size;
	char* data;
	char const* name;
} Blob;

// `name` must be null terminated C-string, and outlive the returned object.
// RETURNS a new Blob with the contents of the given file.
// RETURNS `NULL` if an allocation failure occurs (best effort).
Blob* Blob_from_file(FILE* file, char const* name);

// `name` must be a null-terminated C-string, and outlive the returned object.
// `data` must be a null-terminated C-string. It need not outlive this function.
Blob* Blob_from_c_str(char const* data, char const* name);

// Releases the memory held by the blob. Does not release the blob's name.
void Blob_destroy(Blob* blob);

typedef struct {
	Blob const* blob;
	int32_t begin;
	int32_t end;
} Location;

Location Location_span(Location begin, Location end);

#define ERROR_MAX_FIELDS 32

// Create an empty Error by `(Error){0}`.
typedef struct {
	int8_t count;
	Location locations[ERROR_MAX_FIELDS];
	char messages[ERROR_MAX_FIELDS][1024];
	int32_t lengths[ERROR_MAX_FIELDS];

	// 0: none. 1: text. 2: code. 3: error location. 4: reference location.
	int modes[ERROR_MAX_FIELDS];
} Error;

// Appends the given text to the error message.
// `text` must be a null-terminated c-string. It need not outlive this call.
void Error_text(Error* error, char const* text);

// Appends a multiline description beginning with " at ..." to the error.
// Indicates an invalid construct.
// When a use and a definition conflict, the
// use is the error and the definition is the reference.
void Error_at_location(Error* error, Location location);

// Appends a multiline description beginning with " at ..." to the error.
// Indicates a place of interest which isn't necessarily invalid.
// When a use and a definition conflict, the
// use is the error and the definition is the reference.
void Error_refer_at_location(Error* error, Location location);

void Error_render_colorful(Error* error, FILE* file);

// RETURNS 0 when the error has not been written to.
// Otherwise returns a positive integer.
int Error_has_problem(Error const* error);

#endif
