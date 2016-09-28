
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
**  ctodpf() and dctoie3i():  routines to convert Cray Fortran single
**      precision reals and integers (64 bits each) to IEEE  DP format;
**	it is assumed that char == 8 bits on all machines.
**
**  Written April 1987 by Chuck D'Ambra.
**  Last modified 15 August 1988 by CD.
**  Modified by Dan Anderson for rs6k 22 Feb 1992
**  Modified to unroll loops by PWM 14Nov93
*/

#define CBIAS		040000	/*  Cray f.p. exponent bias  */
#define MBIAS		1022	/*  IEEE f.p. exponent bias  */
#define CSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */
#define LCDIF		sizeof(double) - sizeof(char)

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

extern void ctodpf();

#ifdef sun
void ctodpf_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
        ctodpf(in, out, n);
        return;
}
#endif


/*------------------------------------------------------------------*/
void ctodpf(in, out, n)
unsigned char in[], out[];
int *n;
{
  	register int 		i, j, joff;
  	unsigned char 		*p, sign, *manp[6];
  	long 			exp;
  	static unsigned char 	maxman = 0xff, minman = 0;

	/*  Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/

  	for (i = 0; i < *n; i++) {
    	    p = in+i*8;
    	    joff = i * 8;

    	    sign = *(p) & CSIGNMASK;	/*  sign bit  */

    	    /*
    	    ** Point to mantissa...
    	    */
    	    manp[0] = p+2;
    	    manp[1] = p+3;
    	    manp[2] = p+4;
    	    manp[3] = p+5;
    	    manp[4] = p+6;
    	    manp[5] = p+7;


    	    exp = (*(p+1)) | ((*p & 0x7f) << 8);
    	    if (exp != 0) exp = exp - CBIAS + MBIAS;

	    /*  
	    ** If input is outside range representable on IEEE, set to
    	    **  closest representable number.  
	    */
    	    if (exp > 2046) {
      		exp = 2046;
      	    	/*  Set all bits in mantissa  */
      	    	for (j = 0; j < 6; j++)	manp[j] = &maxman; 
    	    }
    	    else if (exp <= 0) {
      	  	exp = 0;
      		/*  Turn off all bits in mantissa  */
      		for (j = 0; j < 6; j++)	manp[j] = &minman; 
    	    }

	    /*  Pack it into 64 bit IEEE float representation:
	    **	bits 0-51:	mantissa
	    **	bits 52-62:	exponent
	    **	bit 63:		sign
 	    */

     	    /* 
	    ** sign+upper 7 bits of exponent 
	    */
     	    out[joff] = sign | (exp >> 4); 

    	    /* next joff+1 get lower 4 bits of exponent plus first 5 bits 
     	    ** of cray mantissa....but throw away left normalized bit because 
     	    ** its not in IEEE representation..so it leaves 3bits of mp[0] 
     	    ** for next byte
     	    **/

    	    out[joff+1]=  ((exp      & 0xf) <<4) | ((*manp[0] >>3) & 0xf);
    	    out[joff+2] = ((*manp[0] & 0x7) <<5) | ((*manp[1] & 0xf8) >>3);
    	    out[joff+3] = ((*manp[1] & 0x7) <<5) | ((*manp[2] & 0xf8) >>3);
    	    out[joff+4] = ((*manp[2] & 0x7) <<5) | ((*manp[3] & 0xf8) >>3);
    	    out[joff+5] = ((*manp[3] & 0x7) <<5) | ((*manp[4] & 0xf8) >>3);
    	    out[joff+6] = ((*manp[4] & 0x7) <<5) | ((*manp[5] & 0xf8) >>3);
    	    out[joff+7] = ((*manp[5] & 0x7) <<5);
  	}
}
