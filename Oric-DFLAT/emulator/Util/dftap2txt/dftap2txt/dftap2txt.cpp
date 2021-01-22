/* dftap2txt converter tool by 6502Nerd */
/* version 0.1 D. Miah 01/2020 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __linux__
#include <assert.h>
#include <errno.h>
#define strcpy_s strcpy
#define errno_t int
errno_t fopen_s(FILE **f, const char *name, const char *mode) {
	errno_t ret = 0;
	assert(f);
	*f = fopen(name, mode);
	/* Can't be sure about 1-to-1 mapping of errno and MS' errno_t */
	if (!*f)
		ret = errno;
	return ret;
}
#endif

FILE *in, *out;
char fname[100];
int block = 0;								/* Count in to the block */
bool stripLine = false;

int init(int argc, char *argv[])
{
	if (argc < 3) return 1;

	/* Line numbers to be stripped? */
	if (strcmp(argv[1], "-l") == 0) stripLine = true;

	printf("Reading %s\n", argv[argc - 2]);
	int err = fopen_s(&in, argv[argc - 2], "rb");
	printf("Error = %d\n", err);
	if (in == NULL) {
		printf("Cannot open %s file\n\n", argv[1]);
		return 1;
	}

	printf("Writing %s\n", argv[argc - 1]);
	err = fopen_s(&out, argv[argc - 1], "wb");
	printf("Error = %d\n", err);
	if (out == NULL) {
		printf("Cannot create %s file\n\n", argv[2]);
		return 1;
	}
	return 0;
}

unsigned char getdatabyte()
{
	unsigned char valin;

	/* At each new block, skip 2 byte block number */
	if (block == 0) {
		valin = fgetc(in);
		valin = fgetc(in);
	}

	valin = fgetc(in);
	block++;
	if (block > 255) block = 0;
	return valin;
}


int main(int argc, char *argv[])
{
	int i, s;
	unsigned char valin;
	char line[200], temp[200];

	if (init(argc, argv)) {
		printf("Usage: %s [-l] <source tap file> <destination text file>\n", argv[0]);
		printf("Source file must have .tap extension\n");
		printf("(v0.1)\n");
		exit(1);
	}

	if (!feof(in)) {
		/* Jump over sync and #$# of TAP file header */
		do {
			valin = fgetc(in);
		} while (!feof(in) && (valin != 0x24));

		/* 9 byte header - skip */
		for (i = 0; i < 9; i++) valin = fgetc(in);

		/* Jump over filename and zero terminator */
		do {
			valin = fgetc(in);
		} while (!feof(in) && (valin != 0));

		block = 0;
		/* Get lines of text from 256 byte blocks, each preceded by block # */
		while (!feof(in)) {
			i = 0;								/* Count of line characters */
			do {
				valin = getdatabyte();			/* Get a data byte */
				line[i++] = valin;				/* Add to current line */
			} while (valin != 0x0d);			/* Until CR detected */
			line[i] = 0;						/* Add zero terminator */
			if ((i > 1) && stripLine) {			/* Strip line number from line? */
				/* Skip past the first space */
				for (s = 0; line[s] != ' '; s++);
				s++;
				strcpy_s(temp, &line[s]);
				strcpy_s(line, temp);
				i -= s;
			}
			fputs(line, out);					/* Write line */
			if (i == 1) break;					/* Empty line means done */
		}
	}
	fclose(in);
	fclose(out);
	printf("dftap2txt 0.1  (c) 01/2021 --- File converted.\n");
}
