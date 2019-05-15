#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "gen/grammar.h"
#include "parser.h"

static void print_global_usage(char const* program) {
	fprintf(stderr,
	        "USAGE:\n"
	        "\t%s parse <input .smol> <output .smol.ast>\n",
	        program);
	fflush(stderr);
}

static void print_parse_usage(char const* program) {
	fprintf(stderr,
	        "USAGE:\n"
	        "\t%s parse <input .smol> <output .smol.ast>\n",
	        program);
	fflush(stderr);
}

static int main_parse(int argc, char** argv) {
	if (argc < 4) {
		print_parse_usage(argc == 0 ? "smolc" : argv[0]);
		return EXIT_FAILURE;
	}

	char const* filename = argv[2];
	FILE* file = fopen(filename, "rb");
	if (file == NULL) {
		fprintf(stderr, "Could not open file `%s`\n", filename);
		fflush(stderr);
		exit(1);
	}

	Blob* blob = Blob_from_file(file, filename);
	if (blob == NULL) {
		fprintf(stderr, "Could not allocate memory for file `%s`.\n", filename);
		fflush(stderr);
		fclose(file);
		exit(1);
	}

	Error parseError = (Error){0};
	ASTSource source = ASTSource_from_blob(blob, &parseError);
	if (Error_has_problem(&parseError)) {
		Error_render_colorful(&parseError, stderr);
		fflush(stderr);
		fclose(file);
		exit(1);
	}

	printf("Successfully parsed!\n");
	printf("Index: %d\n", (int)source.index);
	(void)source;

	char const* astfile = argv[3];
	FILE* out = fopen(astfile, "wb");
	if (out == NULL) {
		fprintf(stderr, "Could not open file `%s`\n", astfile);
		fclose(file);
	}

	for (int32_t i = 0; i < source.parse->size; i++) {
		size_t w = fwrite(&source.parse->data[i], sizeof(int32_t), 1, out);
		assert(w == 1);
	}
	fflush(out);
	fclose(out);

	Blob_destroy(blob);
	return EXIT_SUCCESS;
}

int main(int argc, char** argv) {
	if (argc < 2) {
		print_global_usage(argc == 0 ? "smolc" : argv[0]);
		return EXIT_FAILURE;
	}

	char const* command = argv[1];
	if (strcmp(command, "parse") == 0) {
		return main_parse(argc, argv);
	}

	print_global_usage(argc == 0 ? "smolc" : argv[0]);
	return EXIT_FAILURE;
}
