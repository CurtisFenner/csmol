#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "gen/grammar.h"
#include "parser.h"

static void print_global_usage(char const* program) {
	fprintf(stderr,
	        "USAGE:\n"
	        "\t%s parse <input .smol> <output .smol.ast>\n"
	        "\t%s analyze <input .smol 1> <...> <input .smol n>\n",
	        program, program);
	fflush(stderr);
}

static void print_parse_usage(char const* program) {
	fprintf(stderr,
	        "USAGE:\n"
	        "\t%s parse <input .smol> <output .smol.ast>\n",
	        program);
	fflush(stderr);
}

static void print_analyze_usage(char const* program) {
	fprintf(stderr,
	        "USAGE:\n"
	        "\t%s analyze <input .smol 1> <...> <input .smol n>\n",
	        program);
	fflush(stderr);
}

static void print(ASTLexeme lexeme) {
	fwrite(lexeme.str, 1, lexeme.length, stdout);
}

static int main_analyze(int argc, char** argv) {
	if (argc < 3) {
		print_analyze_usage(argc == 0 ? "smolc" : argv[0]);
		return EXIT_FAILURE;
	}

	int fileCount = argc - 2;
	ASTSource* sources = (ASTSource*)malloc(sizeof(ASTSource) * fileCount);
	for (int i = 0; i < fileCount; i++) {
		char const* filename = argv[i + 2];
		FILE* file = fopen(filename, "rb");
		if (file == NULL) {
			fprintf(stderr, "Could not open file `%s`\n", filename);
			fflush(stderr);
			return EXIT_FAILURE;
		}

		Blob* blob = Blob_from_file(file, filename);
		fclose(file);
		if (blob == NULL) {
			fprintf(stderr, "Could not allocate memory for file `%s`.\n",
			        filename);
			fflush(stderr);
			return EXIT_FAILURE;
		}

		Error parseError = (Error){0};
		sources[i] = ASTSource_from_blob(blob, &parseError);
		if (Error_has_problem(&parseError)) {
			Error_render_colorful(&parseError, stderr);
			fflush(stderr);
			fclose(file);
			return EXIT_FAILURE;
		}
	}

	for (int i = 0; i < fileCount; i++) {
		// TODO: Compile source.
	}

	for (int i = 0; i < fileCount; i++) {
		// TODO: Free sources[i].
	}
	free(sources);
	return EXIT_SUCCESS;
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
		return EXIT_FAILURE;
	}

	Blob* blob = Blob_from_file(file, filename);
	fclose(file);
	if (blob == NULL) {
		fprintf(stderr, "Could not allocate memory for file `%s`.\n", filename);
		fflush(stderr);
		return EXIT_FAILURE;
	}

	Error parseError = (Error){0};
	ASTSource source = ASTSource_from_blob(blob, &parseError);
	if (Error_has_problem(&parseError)) {
		Error_render_colorful(&parseError, stderr);
		fflush(stderr);
		fclose(file);
		return EXIT_FAILURE;
	}

	printf("Successfully parsed!\n");
	printf("Index: %d\n", (int)source.index);
	(void)source;

	char const* astfile = argv[3];
	FILE* out = fopen(astfile, "wb");
	if (out == NULL) {
		fprintf(stderr, "Could not open file `%s`\n", astfile);
		fclose(file);
		return EXIT_FAILURE;
	}

	for (int32_t i = 0; i < source.parse->ast_size; i++) {
		fprintf(out, "%d\n", (int)source.parse->ast_data[i]);
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
	} else if (strcmp(command, "analyze") == 0) {
		return main_analyze(argc, argv);
	}

	print_global_usage(argc == 0 ? "smolc" : argv[0]);
	return EXIT_FAILURE;
}
