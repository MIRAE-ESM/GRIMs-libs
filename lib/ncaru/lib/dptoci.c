
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

/*  dptoci()  routine to convert  padded double length IEEE integers to CRAY
 * single precision  integers;
 *	it is assumed that char == 8 bits on all machines.
 *  Created from a hack of D'Ambra's  C->IEEE code 
 *  by Dan Anderson for rs6k 14 March 1992
 */
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

extern void dptoci();

#ifdef sun
void dptoci_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
        dptoci(in, out, n);
        return;
}
#endif

/*--------------------------------------------------------------------------*/
void dptoci(in, out, n)
unsigned char in[], out[];
int *n;
{
  	register int 		i, j, joff;
  	register unsigned char 	*pin, uBs;

  	for (i = 0; i < *n; i++) {

    	    pin = in+i*8;
    	    joff = i * 8;

    	    out[joff]  = *(pin) & CSIGNMASK;	/*  sign bit  */
    	    if( out[joff]  != 0x00){ 
		out[joff]  = 0xff; 
		uBs = 0xff; 
	    }
	    else 
    	        uBs = 0x00;

    	    out[joff+1] = uBs;
    	    out[joff+2] = uBs;
    	    out[joff+3] = uBs;
    	    out[joff+4] = *pin;
    	    out[joff+5] = *(pin+1);
    	    out[joff+6] = *(pin+2);
    	    out[joff+7] = *(pin+3);
  	}
}
