#include <machine.h>
#include <sys/file.h>
#include <stdio.h>
sfopen_ (idev,iflag,path,nc)
/*   Open UNIX file (Fortran-callable) for buffered I/O using system routines.
*
*   From F77:
*
* Call SFOPEN (IDEV,IFLAG,PATH)
*
* Integer*4 IDEV
*   UNIX file descriptor (returned) (-1 if open failed)
* Integer*4 IFLAG
*   0 open for reading
*   1 create for writing
*   2 create for reading and writing at start of file
*   3 append: open for writing at end of file, or create for writing
* Character*(*) PATH
*   Address of null-terminated file name (=path name)
*   nc Number of characters in string PATH
*   this is not explicitly in the calling argument list;
*   Fortran adds string lengths to the end automatically
*/
	char *path;
	long *iflag;
	long *idev;
	int nc;
{
	char *type;
	FILE *stream;
	long offset;
	if (*iflag == 0) type = "r";
	if (*iflag == 1) type = "w";
	if (*iflag == 2) type = "w+";
	if (*iflag == 3) type = "a";
	stream = fopen (path,type);
	if (stream == NULL)
	   *idev = -1; /* set device to -1 if open failed*/
	else
	   *idev = (long)stream;
	return;
}
sfput_ (idev,nbytes,buf,nwrit)
/*   Write to UNIX file (Fortran-callable) using buffered I/O.  
*
*   From F77:
*
* Call SFPUT( IDEV,NBYTES,BUF,NWRIT)
*
* Integer*4 IDEV
*   UNIX file descriptor (input, returned from SFOPEN)
* Integer*4 NBYTES
*   Number of bytes to write (input)
* (Integer,Real) BUF
*   Address of buffer array (values written to file)
* Integer*4 NREAD
*   Number of bytes written (returned, should=nbytes)
*/
	char *buf;
	long *nbytes,*nwrit;
	long *idev;
{
	FILE *stream;
	int nw,nitems,size;
	nitems = *nbytes ;
	size = 1;
	stream = (FILE*)*idev;
	nw = fwrite(buf,size,nitems,stream);
	*nwrit = nw;
	return;
}
sfget_ (idev,nbytes,buf,nread)
/*   Read from UNIX file (Fortran-callable) using buffered I/O.
*   From F77:
* Call SFGET (IDEV,NBYTES,BUF,NREAD)
* Integer*4 IDEV
*   UNIX file descriptor (input, returned from SFOPEN)
* Integer*4 NBYTES
*   Number of bytes to read (input)
* (Integer,Real) BUF
*   Address of buffer array (values returned)
* Integer*4 NREAD
*   Number of bytes read (returned, should=nbytes)
*/
	char *buf;
	long *nbytes,*nread;
	long *idev;
{
	FILE *stream;
	int nr,nitems,size;
	nitems = *nbytes ;
	size = 1;
	stream = (FILE*)*idev;
	nr = fread(buf,size,nitems,stream);
	*nread = nr;
	return;
}
sfclose_ (idev,ier)
/*   Close UNIX file opened for buffered I/O (Fortran-callable)
*   From F77:
* Call SFCLOSE (IDEV,IER)
* Integer*4 IDEV
*   UNIX file descriptor (input, returned by SFOPEN)
* Integer*4 IER
*   Error flag (0 => OK) (returned)
*/
	long *idev;
	long *ier;
{
	FILE *stream;
	long ptr;
	stream = (FILE*)*idev;
	ptr=fclose(stream);
	*ier=ptr;
	return;
}
