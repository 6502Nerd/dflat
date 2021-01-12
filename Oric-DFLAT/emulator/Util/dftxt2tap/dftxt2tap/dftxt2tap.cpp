/* dftxt2tap converter tool by 6502Nerd */
/* version 0.1 D. Miah 01/2020 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

FILE *in, *out;
char fname[100], line[200];
int lineNo=0, blockCount=0, blockNum=0;

int init(int argc, char *argv[])
{
	int i;

	if (argc < 3) return 1;

	/* Line numbers prepended */
	if (strncmp(argv[1], "-l", 2) == 0) {
		lineNo = atoi(&argv[1][2]);
		if (lineNo == 0) lineNo = 10;
	}

	printf("Reading %s\n", argv[argc - 2]);
	int err = fopen_s(&in, argv[argc - 2], "rb");
	printf("Error = %d\n", err);
	if (in == NULL) {
		printf("Cannot open %s file\n\n", argv[1]);
		return 1;
	}

	/* Get filename portion of destination, must be <16 chars */
	for (i = strlen(argv[argc-1]); (i > 1) && (argv[argc-1][i] != '\\'); i--);
	if (argv[argc-1][i] == '\\') i++;
	strcpy_s(fname, &argv[argc-1][i]);
	if (strlen(fname) > 15) {
		printf("Destination filename too long.\n");
		return 1;
	}

	printf("Writing %s\n", argv[argc - 1]);
	err = fopen_s(&out, argv[argc - 1], "wb");
	printf("Error = %d\n", err);
	if (out == NULL) {
		printf("Cannot create %s file\n\n", argv[argc-1]);
		return 1;
	}
	return 0;
}

void gettxtline()
{
	unsigned char valin;
	int i = 0;
	char lineStr[10];
	char temp[250];

	do {
		valin = fgetc(in);
		if (valin == 0xff) valin = 0x0d;
		if (valin != 0x0a) line[i++] = valin;
	} while (valin != 0x0d);
	line[i] = 0;
	if ((i > 1) && (lineNo!=0)) {
		if (atoi(line) == 0) {
			_itoa_s(lineNo, lineStr, 10);
			lineNo += 10;
			strcpy_s(temp, lineStr);
			strcat_s(temp, " ");
			strcat_s(temp, line);
			strcpy_s(line, temp);
		}
	}
}

void puttapdata(unsigned char c) {
	if (blockCount == 0) {
		fputc(blockNum & 0xff, out);					/* Low byte of block number */
		fputc(blockNum >> 8, out);						/* High byte of block number */
		blockNum++;
	}
	fputc(c, out);
	blockCount++;
	if (blockCount > 255) blockCount = 0;				/* Block count wraps from 255 to 0 */
}

int main(int argc, char *argv[])
{
	int i;

	if (init(argc, argv)) {
		printf("Usage: %s [-l[start]] <source text file> <destination tap file>\n", argv[0]);
		printf("Destination file must have .tap extension\n");
		printf("(v0.1)\n");
		exit(1);
	}

	while (!feof(in)) {
		/* Put out TAP file header */
		for (i = 0; i <4; i++) fputc(0x16, out);		/* 4 bytes of sync char 0x16 */
		fputc(0x24, out);								/* '$' for start of fname */

		for (i = 0; i < 9; i++) fputc(0xff, out);		/* 9 byte header - future expansion */

		fputs(fname, out);								/* Copy filename */
		fputc(0, out);									/* zero terminator for fname */

		/* Put out all text file as TAP body */
		/* in 256 byte blocks, preceeded by block number */
		while (!feof(in)) {
			gettxtline();								/* Line from text file, line no added if needed */
			for (i=0; i < (int)strlen(line); i++)			/* Write the line out */
				puttapdata(line[i]);
		}
		puttapdata(0xd);								/* Final CR needed for dflat */
		while (blockCount != 0) puttapdata(0x0d);		/* Fill final block with CR */
	}

	fclose(in);
	fclose(out);

	printf("dftxt2tap 0.1  (c) 01/2021 --- File converted.\n");
}
