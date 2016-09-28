#include <stdio.h>
#include <stdlib.h>
#include "pds.h"

/*
 * force a grib file to have a new date
 *
 * usage: to fix limitations in grib
 *  ex. limited forecast hours
 *      limited time ranges
 *      --> cheat by changing the start time so that fhour is smaller
 *
 * v1.0 Wesley Ebisuzaki Y2K buggy
 * for monthly mean data
 */

/*
 * MSEEK = I/O buffer size
 * LEN_HEADER_PDS = size of section 0 and required PDS
 * note: search distance = (MSEEK-LEN_HEADER_PDS) bytes
 */

#define MSEEK 1024
#define LEN_HEADER_PDS	(8 + 28)

#ifndef min
#define min(a,b)  ((a) < (b) ? (a) : (b))
#endif

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
	unsigned char *buffer);


int main(int argc, char **argv) {

    unsigned char buffer[MSEEK];
    FILE *file;				/* oh what original names */
    int i, count, t1, t2;
    long len_grib, pos, date;
    unsigned char *message;
    struct PDS *pds;
    int century, year, month, day, hour;

    if (argc != 3) {
	fprintf(stderr,"%s: change dates on grib file\n", argv[0]);
	fprintf(stderr,"  usage: %s [grib file] yymmddhh\n", argv[0]);
	fprintf(stderr,"  or     %s [grib file] yyyymmddhh\n",argv[0]);
	fprintf(stderr,"    if yy is used -- assumed to be 19xx\n\n");
	fprintf(stderr,"    for instantaneous values: T1, T2 are set to zero\n");
	fprintf(stderr,"    for accum. T1=0, T2=T2-T1 mod 256\n");
	fprintf(stderr,"    for climo, T1 and T2 are not altered\n");
	return 8;
    }

    if ((file = fopen(argv[1],"r+")) == NULL) {
	fprintf(stderr,"Could not open file: %s\n", argv[1]);
	fprintf(stderr,"Failure of the Nth degree\n");
	return 9;
    }

    date = atol(argv[2]);
    if (date < 0) {
	fprintf(stderr,"illegal date %ld\n", date);
	return 10;
    }
    hour = date % 100;
    date /= 100;
    day = date % 100;
    date /= 100;
    month = date % 100;
    date /= 100;
    year = date % 100;

    /*
     * century 20:  1901->2000 
     * century 21:  2001->2100
     */

    century = date / 100;
    if (century == 0) century = 19;
    if (year != 0) {
			century++;
		}
		 else {
			year = 100;
		}

    if (hour >= 24 || day > 31 || month > 12 || day <= 0 || month <= 0) {
	fprintf(stderr,"illegal date %ld\n", date);
	return 10;
    }

    count = pos = 0;
    while ((message = seek_grib(file, &pos, &len_grib, buffer)) != NULL) {
	pds = (struct PDS *) (message + 8);

	pds->Century = century;
	pds->Year = year;
	pds->Month = month;
	pds->Day = day;
	pds->Hour = hour;
	switch (pds->TimeRange) {
	    case 0:
	    case 1:
	    case 10:
		pds->P1 = pds->P2 = 0;
		break;
	    case 2:
	    case 3:
	    case 113:
		pds->P1 = 0;
		pds->P2 = 1;
		pds->ForecastTimeUnit = 3;
		break;
	    case 4:
	    case 5:
		t1 = pds->P1;
		t2 = pds->P2;
		if (t2 < t1) t2 += 256;
		t2 = t2 - t1;
		pds->P1 = 0;
		pds->P2 = (unsigned char) (t2 % 256);
		break;
	    case 51:
	    case 114:
	    case 115:
	    case 116:
	    case 117:
	    case 118:
	    case 123:
	    case 124:
		break;
	    default:
		break;
	}

	/* finished updating grib header */
	/* lets write */
        if (fseek(file, pos, SEEK_SET) == -1) {
	    fprintf(stderr,"fatal error - i/o system\n");
	    return 12;
	}
	i = fwrite(message, sizeof (unsigned char), LEN_HEADER_PDS, file);
	if (i != LEN_HEADER_PDS) {
	    fprintf(stderr,"fatal error - write\n");
	    return 13;
	}
	pos += len_grib;
	count++;
    }
    fclose(file);
    fprintf(stderr,"number of records %d\n", count);
    return 0;
}

/*
 * find next grib header
 *
 * file = what do you think?
 * pos = initial position to start looking at  ( = 0 for 1st call)
 *       returns with position of next grib header (units=bytes)
 * len_grib = length of the grib record (bytes)
 * buffer[MSEEK] = buffer for reading/writing
 *
 * returns (char *) to start of GRIB header+PDS
 *         NULL if not found
 *
 * adapted from SKGB (Mark Iredell)
 *
 * v1.0 5/94 Wesley Ebisuzaki
 *
 */

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
	unsigned char *buffer) {

    int i, len;

    /* read data */
    if (fseek(file, *pos, SEEK_SET) == -1) {
	*len_grib = 0;
	return NULL;
    }

    i = fread(buffer, sizeof (unsigned char), MSEEK, file);

    len = min(i, MSEEK) - LEN_HEADER_PDS;

    for (i = 0; i < len; i++) {
	if (buffer[i] == 'G' && buffer[i+1] == 'R' && buffer[i+2] == 'I'
	    && buffer[i+3] == 'B' && buffer[i+7] == 1) {
		*pos = i + *pos;
		*len_grib = (buffer[i+4] << 16) + (buffer[i+5] << 8) + 
			buffer[i+6];
		return (buffer+i);
	}
    }
    *len_grib = 0;
    return NULL;
}
