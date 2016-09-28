#define CRAY_PRIVATE
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

#include <stdio.h>

#ifdef DEBUG
#include <stdio.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include "cray.h"
#include <unistd.h>
#include <memory.h>
#include <malloc.h>

/* 
** Fortran & C  Cray blocking routines
**
** HISTORY:
**      Original Cray routines written by Craig Ruff.
**	Initial port to the RS6000 by Greg Woods. (added Fortran wrappers)
**
**      22Oct93  - PWM  Cleaned up the source, and added backspace 
** 		   and unblocked I/O capabilities.
*/

static union pcw	zcw;
extern int 		errno;

static int		_blksize = -1;

static int		readblock();
static int		flushblock();
static int		position();

/*extern char		*malloc();*/
extern void		*malloc();


/*
** These routines are originally written for Fortran, therefore most all
** arguments are pointers.  C folks need to pass addresses!
**
** WARNING!!!!!
**
** These routines will not work correctly if the user has specified 
** the "intsize=8 option on the IBM xlf compiler.  In that case, the 
** address if pointing to a true 64 bit integer.
** They will work for  autodbl=dblpad since in that case, the ints are
** only *padded* with an extra 4 bytes.
*/

/*-------------------------------------------------------------------------*/
int crayblocks(size)
int	*size;
{
	/*
	** Set the number of blocks for a crayopen()
	** If the size is absurdly large, then the malloc for the 
	** buffer will fail.
	*/
	if (*size <= 0)   return(-1);

	_blksize = *size;
	return(0);

}
/*-------------------------------------------------------------------------*/
int crayopen(path, rwflags, mode, plen) 
char 	*path;
int  	*rwflags;
int	*mode;
int 	plen;
{
	int 		flags;
	CrayFile 	*cf;
	register char 	*p;
	int		unblkFlag;


	/*
	** Check for a trailing blank in the filename, 
	** set to '\0';
	*/
	for (p=path+plen-1; *p == ' '; p--); 
	if (*++p) *p=0;


	switch(*rwflags) {

	    /*
	    ** Blocked file flags
	    */

   	    case 0: 	/* read only */
      	   	flags = O_RDONLY;
		unblkFlag = 0;	/* False */
      		break;

   	    case 1: 	/* write only, create, truncate */
      		flags = O_WRONLY | O_CREAT | O_TRUNC;
		unblkFlag = 0;	/* False */
      		break;

   	    case 2: 	/* read/write, create, no truncate */
      		flags = O_RDWR | O_CREAT;
		unblkFlag = 0;	/* False */
      		break;

   	    case 3:
      		flags = O_APPEND | O_RDWR;
		unblkFlag = 0;	/* False */
      		break;

	    /*
	    ** Unblocked file flags
	    */

   	    case 10: 	/* read only */
      	   	flags = O_RDONLY;
		unblkFlag = 1;	/* True */
      		break;

   	    case 11: 	/* write only, create, truncate */
      		flags = O_WRONLY | O_CREAT | O_TRUNC;
		unblkFlag = 1;	/* True */
      		break;

   	    case 12: 	/* read/write, create, no truncate */
      		flags = O_RDWR | O_CREAT;
		unblkFlag = 1;	/* True */
      		break;

   	    case 13:
      		flags = O_APPEND | O_RDWR;
		unblkFlag = 1;	/* True */
      		break;

   	    default:
      		if (p-path != plen) *p=' ';  /* reset null byte to a blank.. */
      		return(CRAY_OPENERR);
	}
      
	/*
	** Open the file
	*/
	cf = _CrayOpen(path, flags, *mode, unblkFlag); 

	/* 
	** reset null byte to a blank.. 
	*/
	if (p-path != plen) *p=' ';  

	if (cf ==  NULL) return(CRAY_OPENERR);

	/* 
	** for append modes we must verify EOD && EOF and back up over them
	*/
	if (*rwflags == 3 || *rwflags == 13) {
	    if (position(cf) != 0)  {
		(void) _CrayClose(cf);
		return(CRAY_OPENERR);
	    }
	}

	return((int) cf);
}

/*--------------------------------------------------------------------------*/
int crayrew(cf)
CrayFile 	**cf; 
{

	return(_CrayRew(*cf));
}

/*--------------------------------------------------------------------------*/
int crayread( cf, buf, size, conv) 
CrayFile 	**cf;
int 		*size;
int		*conv;
char 		*buf; 
{
	int 		errcode;
	int 		cwords;
	int 		zpad;

	/*
	** Handle the special case of no conversion here.  
	** Read directly into the user's buffer.
	*/
	if (*conv == 0) { 
	    errcode = _CrayRead(*cf, buf, *size*CRAY_WORD_SIZE);
	    return((errcode < 0) ? errcode : errcode/CRAY_WORD_SIZE);
	}

	/*
	** For all other conversion cases, allocate a buffer for the 
	** read data so that the converted data can be placed in the
	** user's buffer...
	*/

	/* 
	** We only allocate the buffer once, however we do need to check
	** reallocating the buffer....  This buffer is released in
	** crayclose.
	*/
	if (*size*CRAY_WORD_SIZE  > (*cf)->cbufSize) {
	    if ((*cf)->cbuf) {
		(void)free((*cf)->cbuf);
		(*cf)->cbufSize = 0;
	    }

	    (*cf)->cbuf = malloc(*size*CRAY_WORD_SIZE);
	    if ((*cf)->cbuf  == (char *) NULL) return(0);
	    (*cf)->cbufSize = *size*CRAY_WORD_SIZE;
	}

	/*
	** Set the  memory to zeros...
	** We *have* to do this to prevent unused bits from 
	** changing word values.
	*/
	(*cf)->cbuf = memset((*cf)->cbuf, 0L, (*cf)->cbufSize);

	/*
	** Read data into the conversion buffer.
	** Yup, in this case, we are (in essence) double buffering the 
	** file.   If positive, then errcode is the number of bytes read
	*/
 	if ((errcode = _CrayRead(*cf, (*cf)->cbuf, *size*CRAY_WORD_SIZE)) < 0){
		return(errcode);
	}

	cwords = errcode/CRAY_WORD_SIZE;

	switch(*conv) {

   	    case 1: /* convert to single precision floats (32bit) */
      		ctospf((*cf)->cbuf,buf,&cwords);
      		break;

   	    case 2: /* convert to double precision floats (64bit) */
      		ctodpf((*cf)->cbuf,buf,&cwords);
      		break;

   	    case 3: /* convert to int (32bit) */
      		zpad=0;
      		ctospi((*cf)->cbuf,buf,&cwords,&zpad);
      		break;

   	    case 4: /* convert to "zero padded" (double length) ints */
      		zpad=1;
      		ctospi((*cf)->cbuf,buf,&cwords,&zpad);
      		break;
	}

	return(cwords);
}

/*--------------------------------------------------------------------------*/
int craywrite( cf, buf, size, conv)
CrayFile 	**cf;
int		*size;
int		*conv; 
unsigned char 	*buf; 
{

	int 		rval;
	int 		zpad;

	/*
	** For no conversion, place in the user's buffer...
	*/
	if (*conv == 0) {
	    rval =  _CrayWrite( *cf, buf, *size*CRAY_WORD_SIZE);
	    return((rval == 0) ? *size : rval);
	}

	/* 
	** We only allocate the buffer once, however we do need to check
	** reallocating the buffer....  This buffer is released in
	** crayclose.
	*/
	if (*size*CRAY_WORD_SIZE  > (*cf)->cbufSize) {
	    if ((*cf)->cbuf) {
		(void)free((*cf)->cbuf);
		(*cf)->cbufSize = 0;
	    }

	    (*cf)->cbuf = malloc(*size*CRAY_WORD_SIZE);
	    if ((*cf)->cbuf  == (char *) NULL) return(0);
	    (*cf)->cbufSize = *size*CRAY_WORD_SIZE;
	}

	/*
	** Set the  memory to zeros...
	*/
	(*cf)->cbuf = memset((*cf)->cbuf, 0L, (long) (*cf)->cbufSize);

   	switch (*conv) {
      	    case 1: /* convert IEEE float (32bit) to CRAY float */
	 	sptocf(buf,(*cf)->cbuf,size);
	 	break;

       	    case 2:  /* convert IEEE float (64bit) to CRAY float */
	 	dptocf(buf,(*cf)->cbuf,size);
	 	break;

      	    case 3:  /* 32 bit int to CRAY int */
	 	zpad=0;
	 	sptoci(buf,(*cf)->cbuf,size,&zpad);
	 	break;
		
      	    case 4:   /* 32bit double padded int to CRAY int */
	 	zpad=1;
	 	sptoci(buf,(*cf)->cbuf,size,&zpad);
	 	break;

      	    default:
	 	return(0);
   	}

	rval = _CrayWrite(*cf, (*cf)->cbuf, *size*CRAY_WORD_SIZE);
	return((rval == 0) ? *size : rval);
}

/*--------------------------------------------------------------------------*/
int crayweof(cf) 
CrayFile	**cf;
{
	/*
	** Write a Cray EOF to the file 
	*/
	return(_CrayWEOF(*cf));
}

/*--------------------------------------------------------------------------*/
int crayweod(cf)
CrayFile	**cf;
{
	/*
	** Write a Cray EOD to the file, users should *never* 
	** call this.  It's only here for historical purposes
	*/
	return(_CrayWEOD(*cf));
}

/*--------------------------------------------------------------------------*/
int crayclose(cf)
CrayFile	**cf;
{
	/*
	** Close the file
	*/
	return(_CrayClose(*cf));
}

/*--------------------------------------------------------------------------*/
int crayback(cf)
CrayFile	**cf;
{
	/*
	** Backspace
	*/
	return(_CrayBack(*cf));
}


/*#############################################################################

  CRAY dataset manipulation routines

  Originally written by Craig Ruff for MASnet. 
  Enhanced by Peter W. Morreale  

##############################################################################*/

/*--------------------------------------------------------------------------*/
CrayFile *_CrayOpen(name, flag, mode, unblkFlag)
char	*name;
int	flag;
int	mode;
int	unblkFlag;
{
	int		fd;
	CrayFile	*cf;
extern int errno;

	if ((fd = open(name, flag, mode)) < 0) {
#ifdef DEBUG
                printf("Cannot open %s, errno %d\n",name,errno);
		perror(name);
#endif DEBUG
		return(NULL);
	}

	if ((cf = CrayFdopen(fd, flag, unblkFlag)) == NULL) {
		close(fd);
#ifdef DEBUG
                printf("Cannot fdopen %s, errno %d\n",name,errno);
                perror(name);
#endif
                return(NULL);
        }

	cf->oflags 	= flag;
	cf->unblkFlag 	= unblkFlag;
	cf->last_op 	= C_INIT;
	cf->cbuf	=  (char *) NULL;
	cf->cbufSize 	= 0;

	return(cf);
}

/*-----------------------------------------------------------------------*/
static 
void _CrayInit(cf, flag)
CrayFile	*cf;
int		flag;
{
	/*
	** Init the cf struct members.
	*/

	cf->endcw = (union pcw *) &cf->block[CRAY_BLCK_SIZE];
	cf->endblk = (union pcw *) &cf->block[CRAY_BLCK_SIZE*cf->numBlocks];
	cf->blockn = 0;
	cf->bufblockn = 0;
	cf->reod = 0;
	cf->weod = 0;
	cf->weof = 0;
	cf->pri = 0;
	cf->pfi = 0;
	if (flag == O_RDONLY) {
		cf->cw = cf->endblk;

	} else {
		cf->cw = (union pcw *) cf->block;
		*cf->cw = zcw;
#ifdef STRICT
		cf->cw->u.m = M_BCW;
		SET_PCW_FWI(cf->cw, 0);
#endif STRICT
	}

}
/*--------------------------------------------------------------------------*/
CrayFile *
CrayFdopen(fd, flag, unblkFlag)
int	fd;
int	flag;
int	unblkFlag;
{
	register CrayFile	*cf;
	extern char		*getenv();

	if ((cf = (CrayFile *) malloc(sizeof(CrayFile))) == NULL) {
#ifdef DEBUG
		perror("CrayFdopen: malloc");
#endif DEBUG
		return((CrayFile *) NULL);
	}

	/*
	** Set the block size for this blocked buffer.
	*/
	if (_blksize == -1) {
  	    cf->numBlocks = 1;
	    if ((cf->block = getenv("CRAYBLOCKS")) != (char *) NULL) {
	       cf->numBlocks = atoi(cf->block);
	       if (cf->numBlocks == 0) cf->numBlocks = 1;
	    }
	}
	else {
	    cf->numBlocks = _blksize;
	}
	    

	/*
	** Only allocate a buffer if the open is for blocked files...
	*/
	if (unblkFlag == 0) {
	    cf->block = malloc(cf->numBlocks*CRAY_BLCK_SIZE);
	    if (cf->block == (char *) NULL) {
	        (void)free(cf);
	        return((CrayFile *) NULL);
	    }
	}

	cf->crayfd = fd;
	if (unblkFlag == 0) _CrayInit(cf, flag);
	return(cf);
}


/*--------------------------------------------------------------------------*/
int
_CrayRead(cf, buf, size)
register CrayFile	*cf;
char			*buf;
int			size;
{
	int		len;
	int		n;
	char		*p = buf;
	struct rcw	rcw;
	struct bcw	bcw;



	/*
	** If this is an unblocked read, then we can ignore 
	** all the blocked I/O settings and merely read to fill 
	** in the given buffer.
	*/
	if (cf->unblkFlag == 1) {
	   /*
	   ** Read must a multiple of 4096 (512 word blocks...)
	   */
	   if ((size/CRAY_WORD_SIZE)%512) return(CRAY_UNBLKERR);

	   if ((len = read(cf->crayfd, buf, size))!=size) return(CRAY_UNBLKERR);
	   cf->last_op = C_READ;
	   return(len);
	}



	/*
	** If we are here, then this is a blocked read....
	** Check to make sure we have not reached the EOD
	*/
	if (cf->reod)  return(CRAY_EOD);

	
	/*
	**  Read in a library buffer if necessary.
	*/
	if (cf->last_op == C_INIT || cf->last_op == C_REWND) {
	    if ((n = readblock(cf)) != 0) return(n);
	    cf->cw++;
	}

	/*
	** Now read until the end of the next record.
	*/


	while (1) {

	    /*
	    ** Copy the data requested...
	    */
	    len = cf->fwi*CRAY_WORD_SIZE;
	    len = len > size ? size : len;
	    if (size > 0 && len > 0) {
		    memcpy(p, (char *) cf->cw, len);
		    p += len;
		    size -= len;
	    }


	    /*
	    ** Move forward in the library buffer.  If necessary
	    ** read in a new block.
	    */
	    cf->cw += cf->fwi;
	    if (cf->cw >= cf->endcw) {
		if (cf->cw >= cf->endblk) {
		    if ((n = readblock(cf)) != 0) return(n);
		}
		else {
		    cf->endcw += 512;
		}
	    }


	    /*
	    ** Parse the control word.
	    */
	    switch (cf->cw->u.m) {
		case M_BCW:
		    UNPACK_BCW(&cf->cw->block, &bcw);
		    cf->blockn = bcw.bn;
		    cf->fwi = bcw.fwi;
		    cf->cw++;
		    break;

		case M_EOR:
		    UNPACK_RCW(&cf->cw->record, &rcw);
		    cf->fwi = rcw.fwi;
		    cf->pri = rcw.pri;
		    goto DONE;

		case M_EOF:
		    UNPACK_RCW(&cf->cw->record, &rcw);
		    cf->fwi = rcw.fwi;
		    cf->cw++;
		    return(CRAY_EOF);

		case M_EOD:
		    cf->reod = 1;
		    return(CRAY_EOD);

		default:
		    return(CRAY_BADCRAY);
	    }
	}

	DONE:

	p -= rcw.ubc / 8;
	cf->last_op = C_READ;  /* set the last operation flag */
	cf->cw++;
	return(p - buf);
}

/*--------------------------------------------------------------------------*/
static int
readblock(cf)
register CrayFile	*cf;
{
	struct bcw	bcw;
	int		n;

	/*
	** Read in a new buffer.
	*/
	n = read(cf->crayfd, cf->block, cf->numBlocks*CRAY_BLCK_SIZE);
	if (n < 0) return(CRAY_DISKERR);

	/*
	** set the pointers accordingly.
	*/
	cf->cw = (union pcw *) cf->block;
	cf->endcw = (union pcw *) &cf->block[CRAY_BLCK_SIZE];

	/*
	** Unpack the BCW...
	*/
	if (cf->cw->u.m != M_BCW)  return(CRAY_BADCRAY);
	UNPACK_BCW(&cf->cw->block, &bcw);
	cf->bufblockn = bcw.bn;
	cf->blockn    = bcw.bn;
	cf->fwi       = bcw.fwi;

	return(0);
}

/*--------------------------------------------------------------------------*/
int _CrayWrite(cf, buf, size)
CrayFile	*cf;
char		*buf;
int		size;
{
	int		bytes;
	int		left;
	int		len;
	int		ubc;
	int		n;
	struct rcw	rcw;
	struct bcw	bcw;


	/*
	** If this is an unblocked write, then we can ignore 
	** all the blocked I/O settings and merely write the data
	*/
	if (cf->unblkFlag == 1) {
	   /*
	   ** Writes must a multiple of 4096 (512 word blocks...)
	   */
	   if ((size%CRAY_BLCK_SIZE)) return(CRAY_UNBLKERR);

	   if ((len = write(cf->crayfd,buf,size))!=size) return(CRAY_DISKERR);

	   cf->last_op = C_WRITE;
	   return(0);
	}

	/*
	** If we are here, then this is a blocked write....
	*/
	len = (size + CRAY_WORD_SIZE - 1) & ~(CRAY_WORD_SIZE - 1);
	ubc = (len - size) * CRAY_WORD_SIZE;
	len /= CRAY_WORD_SIZE;
	cf->weof = 0;
	cf->pri = 0;

	/*
	** reset some pointers based on the last operation 
	*/
	if (cf->last_op == C_READ || cf->last_op == C_BCKSP) 
	    cf->cw--; 			/* move back to the RCW */
	else if (cf->last_op == C_REWND)
	    _CrayInit(cf, cf->oflags);	/* re-init for writing */

	
	do {
		cf->fwi = len;
		left = cf->endcw - cf->cw - 1;
		if (len > left)
			cf->fwi = left;

		SET_PCW_FWI(cf->cw, cf->fwi);
		cf->cw++;
		bytes = cf->fwi * CRAY_WORD_SIZE;
		if (bytes > 0) {
			memcpy((char *) cf->cw, buf, bytes);
			cf->cw += cf->fwi;
			len -= cf->fwi;
			buf += bytes;
			size -= bytes;
		}

		/* 
		** If out of room in the buffer, flush it.
		*/
		if (cf->cw >= cf->endblk)
			if ((n = flushblock(cf)) < 0) return(n);

		/*
		** Otherwise, add a block control word and continues on.
		*/
		if (cf->cw >= cf->endcw)  {
		    cf->endcw += 512; 
		    cf->blockn++;
		    *cf->cw = zcw;

		    bcw.bdf = 0;
		    bcw.fwi = 0;
		    bcw.m = M_BCW;
		    bcw.bn = cf->blockn;
		    PACK_BCW(&bcw, &cf->cw->block);

		    cf->pfi++;
		    cf->pri++;
		}


	} while (size > 0);

	if (cf->cw == (union pcw *) cf->block) {
#ifdef STRICT
		SET_PCW_FWI(cf->cw, 0);
#endif STRICT
		cf->cw++;

	} else {
		/*
		 * Zero the unused bits in the end of a record.
		 * Bug: if we just called flushblock, the place
		 *	we should have zeroed was just written
		 *	to disk!  Thus, we shouldn't do it again
		 *	now or we'll over write something before
		 *	the buffer in memory.  That's why we're
		 *	in the else branch.
		 */
		buf = (char *) cf->cw;
		len = ubc / 8;
		while (--len >= 0)
			*--buf = '\0';
	}

	*(cf->cw) = zcw;
#ifdef STRICT
	rcw.tran = 0;
	rcw.bdf = 0;
	rcw.srs = 0;
	rcw.fwi = 0;
#endif STRICT
	rcw.m = M_EOR;
	rcw.ubc = ubc;
	rcw.pri = cf->pri;
	rcw.pfi = cf->pfi;
	PACK_RCW(&rcw, &cf->cw->record);
	cf->pri = 0;
	cf->last_op = C_WRITE;
	cf->weof = cf->weod = 0;  /* clear the eof and eod flags. */

	return(0);
}

/*--------------------------------------------------------------------------*/
static int
flushblock(cf)
register CrayFile	*cf;
{
	struct bcw	bcw;
	int		len;

	/*
	** Always truncate the file when flushing to handle O_RDWR cases.
	** We also need to move the fd to the proper block within the
	** file since a readblock may have taken place...
	*/
	len = lseek(cf->crayfd, (long) cf->bufblockn*CRAY_WORD_SIZE*512, SEEK_SET);
	if (len < 0) return(CRAY_DISKERR);
	if (ftruncate(cf->crayfd, len) < 0) return(CRAY_DISKERR);


	/*
	** Now write only those blocks which are "dirty"
	*/
	len = (cf->endcw - (union pcw *) cf->block) * CRAY_WORD_SIZE;
	if (write(cf->crayfd, cf->block, len) != len) {
#ifdef DEBUG
		perror("Block write!");
#endif
		return(CRAY_DISKERR);
	}
	cf->cw = (union pcw *) cf->block;
	cf->endcw = (union pcw *) &cf->block[CRAY_BLCK_SIZE];
	cf->blockn++;
	cf->bufblockn = cf->blockn;
	*cf->cw = zcw;
#ifdef STRICT
	bcw.bdf = 0;
	bcw.fwi = 0;
	bcw.m = M_BCW;
#endif STRICT
	bcw.bn = cf->blockn;
	PACK_BCW(&bcw, &cf->cw->block);
	cf->pfi++;
	cf->pri++;
	return(0);
}
/*--------------------------------------------------------------------------*/
int
_CrayWEOF(cf)
register CrayFile	*cf;
{
	struct rcw	rcw;
	int		n;

	/*
	** If this is an unblocked file, we can't do this.
	*/
	if (cf->unblkFlag == 1) return(CRAY_UNBLKERR);

#ifdef STRICT
	SET_PCW_FWI(cf->cw, 0);
#endif STRICT
	cf->cw++;

	if (cf->cw >= cf->endblk) {
		if ((n = flushblock(cf)) < 0)
			return(n);

#ifdef STRICT
		SET_PCW_FWI(cf->cw, 0);
#endif STRICT
		cf->cw++;
	}

	*cf->cw = zcw;
#ifdef STRICT
	rcw.tran = 0;
	rcw.bdf = 0;
	rcw.srs = 0;
	rcw.fwi = 0;
#endif STRICT
	rcw.m = M_EOF;
	rcw.ubc = 0;
	rcw.pri = 0;
	rcw.pfi = cf->pfi;
	PACK_RCW(&rcw, &cf->cw->record);
	cf->pfi = 0;
	cf->weof = 1;
	cf->last_op = C_WRITE;
	return(0);
}

/*--------------------------------------------------------------------------*/
int
_CrayWEOD(cf)
register CrayFile	*cf;
{
	struct rcw	rcw;
	int		n;

	/*
	** If this is an unblocked file, we can't do this.
	*/
	if (cf->unblkFlag == 1) return(CRAY_UNBLKERR);


	if (!cf->weof)
		if ((n = _CrayWEOF(cf)) < 0)
			return(n);

#ifdef STRICT
	SET_PCW_FWI(cf->cw, 0);
#endif STRICT
	cf->cw++;

	if (cf->cw >= cf->endblk) {
		if ((n = flushblock(cf)) < 0)
			return(n);

#ifdef STRICT
		SET_PCW_FWI(cf->cw, 0);
#endif STRICT
		cf->cw++;
	}

	*cf->cw = zcw;
#ifdef STRICT
	rcw.ubc = 0;
	rcw.tran = 0;
	rcw.bdf = 0;
	rcw.srs = 0;
	rcw.fwi = 0;
	rcw.pri = 0;
	rcw.pfi = 0;
	rcw.m = M_EOD;
	PACK_RCW(&rcw, &cf->cw->record);
#else !STRICT
	cf->cw->record.m = M_EOD;
#endif STRICT
	cf->weod = 1;
	cf->last_op = C_WRITE;
	return(0);
}

/*--------------------------------------------------------------------------*/
int _CrayBack(cf)
CrayFile	*cf;
{
	long		offset;
	union  pcw	*endrec;
	struct rcw	rcw;
	struct bcw	bcw;
	int		bufblockn;
	int		blockn;
	int		pri;
	int		rc;
	int		len;
	int		flushed;



	endrec = cf->cw;
	bufblockn = cf->bufblockn;
	blockn = cf->blockn;
	flushed = 0;


	switch(cf->last_op) {
	    case C_INIT:	/* just return... no-op yet */
	    case C_REWND:
		return(0);
	
	    case C_READ:
		pri = cf->pri;
		endrec -= 2;
		break;

	    case C_BCKSP:
		endrec++;
		UNPACK_RCW(&endrec->record, &rcw);
		pri = rcw.pri;

		/*
		**  If pri == 0, then  the end of this record is in this
		**  block, set endrec to point to the end of the last word
		**  in the record  here.  If we have a pri, then we set the
		**  endrec below.
		*/
		if (pri == 0) endrec--;  
		break;
	
	    case C_WRITE:
		/*
		** If the file was opened O_WRONLY, then this is 
		** an illegal operation.  The reason is that we will 
		** probably have to use readblock() and that will fail.
		*/
		if (cf->oflags & O_WRONLY) return(CRAY_WRONLY);

		/*
		** Unpack the RCW to get the pri.
		*/
		if (cf->cw->u.m != M_EOR)  return(CRAY_BADCRAY);
		UNPACK_RCW(&cf->cw->record, &rcw);
		pri = rcw.pri;

		endrec--;  

		/*
		** If there isn't enough room in the buffer for the 
		** following EOF and EOF, then we'll flush the buffer.
		** Set flushed here so that we will re-read the block.
		*/
		if (cf->endblk - cf->cw < 2) flushed = 1;

                /*
                ** Write a eof and eod
                */

                if ((rc = _CrayWEOD(cf)) < 0)  return(rc);

                /*
                ** flush if necessary
                */
                if (cf->cw > (union pcw *) cf->block) {
		    len = (cf->endcw - (union pcw *) cf->block)*CRAY_WORD_SIZE;
		    if (write(cf->crayfd, cf->block, len) != len) 
			return(CRAY_DISKERR);
		    if (ftruncate(cf->crayfd, (cf->blockn+1)*4096) != 0)
			return(CRAY_DISKERR);
                }

		break;
	
	}

	/*
	** Determine whether 
	** we need to read in a new lib buffer.
	*/
	if (blockn - pri < bufblockn || flushed) {  
	    /* 
	    **  Yup, the starting block for this record is not 
	    **  in the current buffer, or, we previously did a 
	    **  write and we had the flush the buffer as per above.
	    **  Re-read a new buffer 
	    */
	    offset = (blockn - pri) * CRAY_BLCK_SIZE;
	    if (lseek(cf->crayfd, offset, SEEK_SET) < 0) return(CRAY_WRONLY);
	    if ((rc = readblock(cf)) != 0) return(rc);
	    cf->cw++;
	    /*
	    ** Reset endrec only if this buffer was not flushed 
	    */
	    if (!flushed) endrec = cf->endcw;
	}
	else { 	
	    /* 
	    ** nope, the starting block is in this buffer.
	    ** Just set the pointers accordingly
	    */
	    cf->cw = (union pcw *) cf->block + ((blockn-pri)-bufblockn)*512;
	    cf->endcw = cf->cw + 512;

	    /*
	    ** If we have a pri, then set endrec to the end of the block.
	    */
	    if (pri) endrec = cf->endcw;

	    /*
	    ** This must be a BCW now.
	    */
	    if (cf->cw->u.m != M_BCW)  return(CRAY_BADCRAY);

            UNPACK_BCW(&cf->cw->block, &bcw);
	    cf->blockn = bcw.bn;
	    cf->fwi = bcw.fwi;
	    cf->cw++;
	}

	/*
	** Traverse the block to find the last control word.
	** This is the start of the previous record
	*/
	while((cf->cw + cf->fwi) < endrec) {
	    cf->cw += cf->fwi;
	    UNPACK_RCW(&cf->cw->record, &rcw);
	    if (rcw.m != M_EOR && rcw.m != M_EOF) return(-1);
	    cf->fwi = rcw.fwi;
	    cf->pri = rcw.pri;
	    cf->pfi = rcw.pfi;
	    cf->cw++;
	}

	cf->reod = 0;
	cf->last_op = C_BCKSP;

	return(0);
	    

}

/*--------------------------------------------------------------------------*/
int
_CrayClose(cf)
register CrayFile	*cf;
{
	int	n = 0;

	/*
	** We only perform the control word and block flushing if 
	** this is a blocked file....
	*/
	if (cf->unblkFlag == 0)  {
	    switch (cf->last_op) {

		case C_INIT:    /* no-op */
		case C_REWND:
		case C_READ:
		    break;

		case C_WRITE:
	            if (!cf->weof) n = _CrayWEOF(cf);
	            if ((n == 0) && !cf->weod) n = _CrayWEOD(cf);
	            if ((n == 0) && (cf->cw > (union pcw *) cf->block))
		        if (flushblock(cf))  return(CRAY_DISKERR);
		    break;
		    
		case C_BCKSP:
		    /*
		    ** Don't write an EOF and EOD here, _CrayBack already did.
		    */
	            if (cf->cw > (union pcw *) cf->block)
		        if (flushblock(cf))  return(CRAY_DISKERR);
		    break;
	    }
    	}

	/*
	** Free the I/O buffer and the conversion buffer
	*/
	if (cf->unblkFlag == 0) (void)free(cf->block);
	if (cf->cbuf) (void)free(cf->cbuf);

	(void)close(cf->crayfd);
	(void)free(cf);

	return(n);
}

/*--------------------------------------------------------------------------*/
int _CrayRew(cf)
CrayFile 	*cf; 
{
	int	rc;


	switch(cf->last_op) {
	    case C_INIT:	/* just return... */
	    case C_REWND:
		return(0);
	
	    case C_READ:
	    case C_BCKSP:
		break;
	
	    case C_WRITE:
		/*
		** Write a eof and eod
		*/
		if (cf->weod == 0) {
		    if ((rc = _CrayWEOD(cf)) < 0)  return(rc);
		}

		/*
		** flush if necessary
		*/
		if ((cf->cw - cf->endcw) != (CRAY_WORD_SIZE*511)) {
		    if ((rc = flushblock(cf)) < 0) return(rc);
		}
		break;
	
	}

	/*
	** seek to the beginning.
	*/
	if (lseek(cf->crayfd, 0L, SEEK_SET)) return(-1);

	/*
	** Re-initialize the cf struct.
	*/
	_CrayInit(cf, cf->oflags);
	cf->last_op = C_REWND;

	return(0);
}

/*------------------------------------------------------------------------*/
static
int position(cf)
CrayFile 	*cf;
{
	/*
	** Position a file before the last EOF (blocked file) or 
	** at the end (unblocked file) for appending
	*/
	long		offset;
	int		rc;
	struct rcw	rcw;

	/*
	** Set the offset and seek to either the last block in the file,
	** or the end of the file (unblocked files).
	*/
	if (cf->unblkFlag == 1) 
	    offset = 0L;
	else
	    offset = -4096L;
	
	rc = lseek(cf->crayfd, offset, SEEK_END);
	if (rc < 0 || cf->unblkFlag == 1) return(rc);

	/*
	** Read in the last block of the file.
	*/
	if ((rc = readblock(cf)) < 0) return(rc);
	cf->cw++;

	/*
	** Traverse the block until we find the EOD
	*/
	while((cf->cw + cf->fwi) < cf->endcw) {
	    cf->cw += cf->fwi;
	    UNPACK_RCW(&cf->cw->record, &rcw);

	    /*
	    ** If this is a EOD, break out of the loop
	    */
	    if (rcw.m == M_EOD)  break;

	    if (rcw.m != M_EOR && rcw.m != M_EOF) return(-1);
	    cf->fwi = rcw.fwi;
	    cf->cw++;
	}

	/*
	** Position before the EOF mark.
	*/
	cf->cw -= 2;

	return(0);
}

/*-----------------------------------------------------------------------*/
void _CrayCfDump(cf)
CrayFile	*cf;
{
	/*
	** For debugging purposes only 
	*/

	(void)printf("Dump of cf struct:\n\n");
	(void)printf("\tunblkFlag = %d\n", cf->unblkFlag);
	(void)printf("\tlast_op   = ");
	switch(cf->last_op) {
	    case C_INIT:
		(void)printf("C_INIT\n");
		break;
	    case C_READ:
		(void)printf("C_READ\n");
		break;
	    case C_WRITE:
		(void)printf("C_WRITE\n");
		break;
	    case C_BCKSP:
		(void)printf("C_BCKSP\n");
		break;
	    case C_REWND:
		(void)printf("C_REWND\n");
		break;

	    default:
		(void)printf("*** UNKNOWN ***\n");
		break;
	}
	(void)printf("\tblockn    = %d\n", cf->blockn);
	(void)printf("\tbufblockn = %d\n", cf->bufblockn);
	(void)printf("\tcrayfd    = %d\n", cf->crayfd);
	(void)printf("\treod      = %d\n", cf->reod);
	(void)printf("\tweod      = %d\n", cf->weod);
	(void)printf("\tweof      = %d\n", cf->weof);
	(void)printf("\tfwi       = %d\n", cf->fwi);
	(void)printf("\tpfi       = %d\n", cf->pfi);
	(void)printf("\tpri       = %d\n", cf->pri);
	(void)printf("\tcw        = %d words from block\n", 
		     ((char *) cf->cw -  cf->block) / CRAY_WORD_SIZE);
	(void)printf("\tendcw     = %d words from cw, %d words from block\n", 
		     cf->endcw - cf->cw,
		     ((char *) cf->endcw - cf->block) / CRAY_WORD_SIZE);
	(void)printf("\tendblk    = %d words from endcw, %d words from block\n",
		     cf->endblk - cf->endcw, 
		     ((char *) cf->endblk - cf->block) / CRAY_WORD_SIZE);
	(void)printf("\tnumBlocks = %d\n", cf->numBlocks);
	(void)printf("\tblock     = %s\n", cf->block  ? "(defined)" : "(NULL)");
	(void)printf("\tcbuf      = %s\n", cf->cbuf  ? "(defined)" : "(NULL)");
	(void)printf("\tcbufSize  = %d\n", cf->cbufSize);
}

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

#ifdef sun 

int crayblocks_(size)
int *size;
{
        return(crayblocks(size));
}

int crayopen_(path, rwflags, mode, plen) 
char    *path;
int     *rwflags;
int     *mode;
int     plen;
{
        return(crayopen(path, rwflags, mode, plen));
}

int crayrew_(cf)
CrayFile **cf;
{
        return(crayrew(cf));
}

int crayread_( cf, buf, size, conv) 
CrayFile        **cf;
char            *buf;
int             *size;
int             *conv;
{
        return(crayread(cf, buf, size, conv));
}

int craywrite_( cf, buf, size, conv)
CrayFile        **cf;
char            *buf;
int             *size;
int             *conv;
{
        return(craywrite(cf, buf, size, conv));
}

int crayweof_(cf)
CrayFile **cf;
{
        return(crayweof(cf));
}

int crayweod_(cf)
CrayFile **cf;
{
        return(crayweod(cf));
}

int crayback_(cf)
CrayFile **cf;
{
        return(crayback(cf));
}

int crayclose_(cf)
CrayFile **cf;
{
        return(crayclose(cf));
}

#endif

