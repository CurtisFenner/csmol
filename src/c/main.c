#include "stdio.h"
#include "stdlib.h"

#include "gen/grammar.h"
#include "parser.h"

int main(int argc, char** argv) {
	if (argc < 2) {
		fprintf(stderr, "USAGE:\n\t%s <sourcefile>\n", argv[0]);
		fflush(stderr);
		exit(1);
	}

	FILE* file = fopen(argv[1], "rb");
	if (file == NULL) {
		fprintf(stderr, "Could not open file `%s`\n", argv[1]);
		fflush(stderr);
		exit(1);
	}

	Blob* blob = Blob_from_file(file, argv[1]);
	if (blob == NULL) {
		fprintf(stderr, "Could not allocate memory for file `%s`.\n", argv[1]);
		fflush(stderr);
		exit(1);
	}

	Error parseError = (Error){0};
	ASTSource source = ASTSource_from_blob(blob, &parseError);
	if (Error_has_problem(&parseError)) {
		Error_render_colorful(&parseError, stderr);
		fflush(stderr);
		exit(1);
	}

	printf("Successfully parsed!\n");
	printf("Index: %d\n", (int)source.index);
	(void)source;

	Blob_destroy(blob);

	return 0;
}
