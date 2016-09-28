
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
/* 
** dctocf():  routine to convert  double IEEE to CRAY single precision reals.
**
**	it is assumed that char == 8 bits on all machines.
**
**  Created from a hack of D'Ambra's  C->IEEE code 
**  by Dan Anderson for rs6k 14 March 1992
** Code cleanup Peter Morreale January 1994.
*/
	
#define CBIAS		040000	/*  Cray f.p. exponent bias  */
#define MBIAS		1022	/*  IEEE f.p. exponent bias  */
#define CSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */
	
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

extern void dptocf();

#ifdef sun
void dptocf_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
	dptocf(in, out, n);
	return;
}
#endif

/*-------------------------------------------------------------------------*/
void dptocf(in, out, n)
unsigned char in[], out[];
int *n;
{
	register int 		i, j, joff;
	unsigned char 		*p, sign, *manp[6];
	long 			exp;
	static unsigned char 	minman = 0, and;
	
	/*  
	** Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/
	
	for (i = 0; i < *n; i++) {
	    p = in+i*8;
	    joff = i * 8;
	
	    sign = *(p) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+1;	
	    manp[1] = p+2;	
	    manp[2] = p+3;	
	    manp[3] = p+4;	
	    manp[4] = p+5;	
	    manp[5] = p+6;	
	    manp[6] = p+7;	
	

	    /*
	    ** Get the exponet
	    */
	    exp = ((*(p+1))>>4) | ((*p & 0x7f) << 4);
	    if (exp != 0) exp = exp - MBIAS + CBIAS;
	
	    /*  
	    ** If input is outside range representable on IEEE, set to
	    ** closest representable number. 
	    */
	    and=0xff;
	    if (exp <= 0) {  /* too small */
	        exp = 0;
	        /*  
	        ** Turn off all bits in mantissa 
	        */
	        for (j = 0; j < 7; j++) manp[j] = &minman;
	        and=0x00;
	    }
	
	    /*  
	    ** Pack it into 64 bit CRAY float representation:
	    **	bits 0-47:	mantissa
	    **	bits 48-62:	exponent
	    **	bit 63:		sign
	    */
	
	    out[joff] = sign | (exp >> 8);  /* sign+upper 7 bits of exponent */

	    out[joff+1] = ((exp & 0xff));
	    out[joff+2] = (0x80 & and)|((*manp[0] & 0xf)<<3)|((*manp[1])>>5);
	    out[joff+3] = ((*manp[1] & 0x1f) <<3) | ((*manp[2] ) >>5);
	    out[joff+4] = ((*manp[2] & 0x1f) <<3) | ((*manp[3] ) >>5);
	    out[joff+5] = ((*manp[3] & 0x1f) <<3) | ((*manp[4] ) >>5);
	    out[joff+6] = ((*manp[4] & 0x1f) <<3) | ((*manp[5] ) >>5);
	    out[joff+7] = ((*manp[5] & 0x1f) <<3) | ((*manp[6] ) >>5);
	}
}
	
	
	
