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
	return 0;
}
