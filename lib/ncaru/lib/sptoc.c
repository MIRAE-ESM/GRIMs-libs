
/* Copyright 1994 University Corporation for Atmospheric Research (UCAR).
** 
** Permission to use, copy, and modify this software and its documentation 
** for any non-commercial purpose is hereby granted without fee, provided 
** that the above copyright notice appear in all copies and that both that 
** copyright notice and this permission notice appear in supporting 
** documentation. UCAR makes no representations about the suitability of 
** this software for any purpose.   It is provided "as is" without express
** or implied warranty.
*/

/*  sptocf() and sptoci():  routines to convert IEEE  single
**   precision reals and integers (32 bits each) to CRAY (64 bit) format;
**	it is assumed that char == 8 bits on all machines.
**  Hack of D'Ambras ctospf code
**  Created by Dan Anderson in March of 92
*/
	
#define CBIAS		040000	/*  Cray f.p. exponent bias  */
#define MBIAS		126	/*  IEEE f.p. exponent bias  */
#define CSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */
#define LCDIF		sizeof(long) - sizeof(char)
	
/*
** Because the SUN Fortran compiler adds an underscore to Fortran entry
** points, we need to define the following wrappers to the user
** entry points.
**
** There probably is a cleaner way to manage this, however we are now
** stuck with this documented (and widely used) interface.
**
** By doing this, the interfaces are the same for SUNs and RS6000s
*/

extern void sptocf();
extern void sptoci();

#ifdef sun
void sptocf_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
        sptocf(in, out, n);
        return;
}

void sptoci_(in, out, n, zpad)
unsigned char   in[], out[];
int             *n;
int		*zpad;
{
        sptoci(in, out, n, zpad);
        return;
}
#endif

	
/*--------------------------------------------------------------------------*/
void sptocf(in, out, n)
unsigned char in[], out[];
int *n;
{
	register int 		i, joff;
	unsigned char 		*p, sign, Cb, *manp[3];
	long 			exp;
	static unsigned char  	minman = 0;
	
	/*   32 bit IEEE float representation:
	**	bits 0-22:	mantissa
	**	bits 23-30:	exponent
	**	bit 31:		sign
	*/

	/*  Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/
	
	for (i = 0; i < *n; i++) {
	    p = in+i*4;
	    joff = i * 8;
	
	    sign = *(p) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+1;	
	    manp[1] = p+2;
	    manp[2] = p+3;
	
	    exp = ((*(p+1) & 0x80)>>7) | ((*p & 0x7f) << 1);
	
	    Cb=0x80;
	    if (exp != 0) exp = exp - MBIAS + CBIAS;
	
	    if (exp <= 0) {   /* too small */
	      	exp = 0;
	      	Cb=0x00;
	      	manp[0] = &minman;
	      	manp[1] = &minman;
	      	manp[2] = &minman;
	
	    }
	
	    out[joff] = sign | (exp >> 8);
	    out[joff+1] = (exp & 0xff) ;
	    out[joff+2] = Cb |((*manp[0]) & 0x7f) ;
	    out[joff+3] =  *manp[1];
	    out[joff+4] =  *manp[2];
	    out[joff+5] =  0x00;
	    out[joff+6]=   0x00;
	    out[joff+7]=   0x00;
	}
}
	
	
/*--------------------------------------------------------------------------*/
void sptoci(in, out, n, zpad)
unsigned char in[], out[];
int *n, *zpad;
{
	register int 		i, j;
	register unsigned char 	*pin;
	
	for (i = 0; i < *n; i++) {
	
	    pin = in+(*zpad+1)*i*4;
	    j = i * 8;
	
	    out[j] = *(pin) & CSIGNMASK;	/*  sign bit  */
	
	    if( out[j] != 0x00) { 
		out[j] = 0xff; 
		out[j+1]  = 0xff;
		out[j+2]  = 0xff;
		out[j+3]  = 0xff;
	    }
	    else {
		out[j+1]  = 0x00;
		out[j+2]  = 0x00;
		out[j+3]  = 0x00;
	    }
	
	    out[j+4] = *pin;
	    out[j+5] = *(pin+1);
	    out[j+6] = *(pin+2);
	    out[j+7] = *(pin+3);
	}
}
