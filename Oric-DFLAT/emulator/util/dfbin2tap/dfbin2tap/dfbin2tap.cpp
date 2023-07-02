/* dfbin2tap converter tool by 6502Nerd 			*/
/* Converts a raw image file to a format that can   */
/* be loaded using the bload function in Orid dflat.*/
/* To convert files, use the OSDK pictconv.exe		*/
/* utility with option -o2 e.g.						*/
/* pictconv -o2 -f6 source.jpg dflatimg.bin			*/
/* read the pictconv instructions for more picture	*/
/* options, but I like -f6 for static displays		*/
/* version 0.1 D. Miah 03/2023 						*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(__linux__) || defined(__APPLE__)
#include <assert.h>
#include <errno.h>
#define pathchr '/'
#define strcpy_s strcpy
#define strcat_s strcat
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
}
#else
#define pathchr '\\'
#endif

FILE *in, *out;
char fname[100], line[200];
int lineNo=0, blockCount=0, blockNum=0, inSize=0;

int init(int argc, char *argv[])
{
	size_t i;

	if (argc != 3) return 1;

	printf("Reading %s\n", argv[argc - 2]);
	int err = fopen_s(&in, argv[argc - 2], "rb");
	printf("Error = %d\n", err);
	if (in == NULL) {
		printf("Cannot open %s file\n\n", argv[1]);
		return 1;
	}

	/* Find the size of the input raw file */
	fseek(in, 0L, SEEK_END);
	inSize = ftell(in);
	fseek(in, 0L, SEEK_SET);

	/* Get filename portion of destination, must be <16 chars */
	for (i = strlen(argv[argc-1]); (i > 1) && (argv[argc-1][i] != pathchr); i--);
	if (argv[argc-1][i] == pathchr) i++;
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
	size_t i;

	if (init(argc, argv)) {
		printf("Usage: %s <source binary file> <destination tap file>\n", argv[0]);
		printf("Destination file must have .tap extension\n");
		printf("(v0.1)\n");
		exit(1);
	}

	while (!feof(in)) {
		unsigned char c;
		
		/* Put out TAP file header */
		for (i = 0; i <4; i++) fputc(0x16, out);		/* 4 bytes of sync char 0x16 */
		fputc(0x24, out);								/* '$' for start of fname */

		for (i = 0; i < 9; i++) fputc(0xff, out);		/* 9 byte header - future expansion */

		fputs(fname, out);								/* Copy filename */
		fputc(0, out);									/* zero terminator for fname */
		
		puttapdata(0x00);								/* Destination address 0xa000 low byte*/
		puttapdata(0xa0);								/* Destination address 0xa000 high byte*/
		puttapdata(inSize&255);							/* Put out low byte of size */
		puttapdata(inSize/256);							/* Put out high byte of size */
		/* Put out all text file as TAP body */
		/* in 256 byte blocks, preceeded by block number */
		while (!feof(in)) {
			c=fgetc(in);								/* Get a byte of raw data */
			puttapdata(c);								/* Write the byte out */				
		}
		while (blockCount != 0) puttapdata(0x00);		/* Fill final block with nul */
	}

	fclose(in);
	fclose(out);

	printf("dfbin2tap 0.1  (c) 03/2023 --- File converted.\n");
}
