/* tap2wav tool by F.Frances*/
/* version 1.1 adapted by Simon & F.Frances 08/2007*/
/* adapted for dflat by D. Miah 01/2020 */

#include <stdlib.h>
#include <stdio.h>

#if defined(__linux__) || defined(__APPLE__)
#include <stdint.h>
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
typedef uint32_t U32;
#else
/* use windows custom types */
typedef unsigned __int32 U32;
#endif

#define FCC(a, b, c, d) ((U32)(a)|((U32)(b)<<8)|((U32)(c)<<16)|((U32)(d)<<24))

FILE *in, *out;
int file_size;
int speed = 11025;					/* Default speed is 11KHz */
int silence = 0;
int gap = 3600;						/* Default block gap is 3600 bits of 0 ~ 1.0seconds */

/* wav file header format */
struct {
	int sig;
	int riff_size;
	int typesig;
	int fmtsig;
	int fmtsize;
	short tag;
	short channels;
	int freq;
	int bytes_per_sec;
	short byte_per_sample;
	short bits_per_sample;
	int samplesig;
	int datalength;
} sample_riff = {
	FCC('R','I','F','F'),0,
	FCC('W','A','V','E'),
	FCC('F','M','T',' '),16,1,1,0,0,1,8,
	FCC('D','A','T','A'),0
};

/* emit required wave form */
void emit_level(int size)
{
	static int current_level = 0xC0;
	int i;
	current_level = 256 - current_level;
	for (i = 0; i < size; i++) fputc(current_level, out);
	file_size += size;
}

/* emit silence */
void emit_silence()
{
	static int current_level = 0x80;
	int i, size;
	size = 15000;
	current_level = 256 - current_level;
	for (i = 0; i < size; i++) fputc(current_level, out);
	file_size += size;
}

/* emit a single bit */
void emit_bit(int bit)
{
	switch (speed) {
	case 4800:
		emit_level(1);
		if (bit) emit_level(1);
		else emit_level(2);
		break;
	case 8000:
		if (bit) {
			emit_level(1);
			emit_level(2);
		}
		else {
			emit_level(2);
			emit_level(3);
		}
		break;
	case 11025:
		if (bit) {
			emit_level(2);
			emit_level(2);
		}
		else {
			emit_level(3);
			emit_level(4);
		}
		break;
	}
}

/* emit a single byte */
void emit_byte(int val)
{
	int i;
	emit_bit(1);							/* 2 start bits are 1 */
	emit_bit(1);
	for (i = 0; i < 8; i++, val >>= 1) {	/* 8 bits of data */
		emit_bit(val & 1);
	}
	emit_bit(0);							/* 1 stop bit is 0 */
}

int feed_parameters(char *param)
{
	switch (param[1]) {
	case 'a':
	case 'A':
		silence = 1;
		break;
	case 'b':
	case 'B':
		gap = 1;							/* Block gap short for binary mode */
		break;
	case 'd':
	case 'D':
		gap = atoi(param + 2);
		break;
	case '8':
		speed = 8000;
		break;
	case '1':
		if (atoi(param + 1) == 11) {
			speed = 11025;
		}
		else {
			printf("Bad value: %s\n", param + 1);
			return 1;
		}
		break;
	default:
		printf("Bad parameter: %s\n", param);
		return 1;
	}
	return 0;
}


int init(int argc, char *argv[])
{
	if (argc < 3) return 1;
	if (argv[1][0] == '-') {
		if (feed_parameters(argv[1])) return 1;
	}
	if (argv[2][0] == '-') {
		if (feed_parameters(argv[2])) return 1;
	}
	if ((argc > 3) && argv[3][0] == '-') {
		if (feed_parameters(argv[3])) return 1;
	}

	sample_riff.freq = sample_riff.bytes_per_sec = speed;
	printf("Reading %s\n", argv[argc - 2]);
	int err = fopen_s(&in, argv[argc - 2], "rb");
	printf("Error =%d\n", err);
	if (in == NULL) {
		printf("Cannot open %s file\n\n", argv[1]);
		return 1;
	}
	printf("Writing %s\n", argv[argc - 1]);
	err = fopen_s(&out, argv[argc - 1], "wb");
	printf("Error =%d\n", err);
	if (out == NULL) {
		printf("Cannot create %s file\n\n", argv[2]);
		return 1;
	}
	return 0;
}

int main(int argc, char *argv[])
{
	int i;
	unsigned char valin;

	if (init(argc, argv)) {
		printf("Usage: %s [ -b ][ -4 | -11 ] [ -a ] <.TAP file> <.WAV file>\n", argv[0]);
		printf("Options: -4  produces a  4.8 kHz WAV file\n");
		printf("         -11 produces a 11 kHz WAV file  (default is 4800 Hz)\n");
		printf("         -b  produces short block gaps for binary files\n");
		printf("         -dX produces gap of X bits between data blocks\n");
		printf("         -a  adds a 1.5 seconds silence at the end of the WAV file, as\n");
		printf("             some mp3 players seem to 'eat' the end of the file\n");
		printf("(v0.1)\n");
		exit(1);
	}

	printf("Writing samples.\n");
	fwrite(&sample_riff, 1, sizeof(sample_riff), out);

	while (!feof(in)) {
		if (fgetc(in) == 0x16) {						/* Skip the syncro bytes */
			do {
				valin = fgetc(in);
				if (feof(in)) break;
			} while (valin != 0x24);					/* Skip until '$' found */
			if (feof(in)) break;

			for (i = 0; i < 128; i++) emit_bit(0);		/* Put out 128 bits of 0 for sync */

			emit_byte('$');								/* Put out '$' */
			for (i = 0; i < 32; i++) emit_bit(0);		/* Put out 32 bits of delay */

			for (i = 0; i < 9; i++) {					/* Get 9 byte header - future expansion */
				valin = fgetc(in);
				emit_byte(valin);						/* Put with 32 bits delay */
				for (int s = 0; s < 32; s++) emit_bit(0);
			}
			do {										/* Put out name */
				valin = fgetc(in); emit_byte(valin);
				for (i = 0; i < 32; i++) emit_bit(0);	/* Put out 32 bits of delay */
			} while ((valin != 0) && (!feof(in)));

			while (!feof(in)) {
				for (i = 0; i < 128; i++) emit_bit(0);	/* Put out 128 bits of 0 before block */

				valin = fgetc(in);						/* Read block low byte */
				emit_byte(valin);
				valin = fgetc(in);						/* Read block high byte */
				emit_byte(valin);

				for (i = 0; i < 32; i++) emit_bit(0);	/* Put out 32 bits of 0 for delay after block # */

				for (i = 0; i < 256; i++) {				/* Write 256 byte block */
					valin = fgetc(in);
					emit_byte(valin);
				}

				for (i = 0; i < gap; i++) emit_bit(0);	/* Block dependent on binary or normal format */
			}
		}
	}
	fclose(in);

	if (silence == 1) {
		emit_silence();
	}
	sample_riff.datalength = file_size;
	sample_riff.riff_size = sample_riff.datalength + 8 + sample_riff.fmtsize + 12;
	fseek(out, 0, SEEK_SET);
	fwrite(&sample_riff, 1, sizeof(sample_riff), out);

	fclose(out);
	printf("dftap2wav 0.1  (c) 01/2021 --- File converted.\n");
}
