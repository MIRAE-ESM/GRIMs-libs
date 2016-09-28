#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

main(int ac,char **av)
{
	int		ifd,ofd;
	char	   oname[256],fname[256];
	int		nhead,rec_size=0,swap;
	mode_t	mod=0644;

	if (ac<3)
	{
		printf("Usage : %s [-s] num_head filename [rec_size]\n",av[0]);
		exit(1);
	}
   if (strcmp(av[1],"-s")==0)
   {
      nhead=atoi(av[2]);
      strcpy(fname,av[3]);
	   if (ac==5)
		   rec_size=atoi(av[4]);
      swap=1;
   }
   else
   {
      nhead=atoi(av[1]);
      strcpy(fname,av[2]);
	   if (ac==4)
		   rec_size=atoi(av[3]);
      swap=0;
   }
	if ((ifd=open(fname,O_RDONLY))<0)
	{
		printf("Can't open %s file\n",fname);
		exit(1);
	}
	strcpy(oname,fname);
	strcat(oname,".ieee");
	if ((ofd=open(oname,O_WRONLY|O_CREAT|O_TRUNC,mod))<0)
	{
		printf("Can't open %s file\n",oname);
		exit(1);
	}
	if (scan_file(ifd,nhead,ofd,rec_size,swap)<0) exit(1);
	close(ifd);
	close(ofd);
	printf("out file is %s\n",oname);
	exit(0);
}

int scan_file(ifd,nhead,ofd,rec_size,swap)
int		ifd,ofd;
int		nhead,rec_size,swap;
{
	int	   dum,len;
	float    *f4;
	double   *f8;
	int      skip,i,l,nelm,nvar,recl;

	for (skip=0; skip<nhead; skip++)
	{
		if (read(ifd,&len,sizeof(int))!=sizeof(int)) break;
      if (swap) byteswap(&len,4);
		lseek(ifd,(off_t)len,SEEK_CUR);
		if (read(ifd,&dum,sizeof(int))!=sizeof(int)) break;
      if (swap) byteswap(&dum,4);
		if (dum!=len)
		{
			printf("rec_size %d != dum %d \n",len,dum);
			return(-1);
		}
		printf("skip %d rec\n",len);
	}
	if (rec_size==0)
	{
		if ((read(ifd,&len,sizeof(int)))!=sizeof(int))
		{
			printf("error get rec size\n");
			exit(1);
		}
      if (swap) byteswap(&len,4);
		lseek(ifd,(off_t)(-sizeof(int)),SEEK_CUR);
      rec_size=len;
	}

	recl=rec_size/8;
	f4=(float *)malloc(rec_size);
   printf("rec_size, recl=%d %d\n",rec_size, recl);

	while (read(ifd,&len,sizeof(int))==sizeof(int))
	{
      if (swap) byteswap(&len,4);
		f8=(double *)malloc(len);
		read(ifd,f8,len);
		if (read(ifd,&dum,sizeof(int))!=sizeof(int)) break;
      if (swap) byteswap(&dum,4);

		if (dum!=len)
		{
			printf("rec_size %d != dum %d \n",len,dum);
			return(-1);
		}
		nvar=len/(recl*8);

		for (l=0; l<nvar; l++)
		{
			for (i=0; i<recl; i++) 
			{
            if (swap) byteswap(&f8[i+l*recl],8);
				f4[i]=f8[i+l*recl];
				/*
				if (i%20==0) printf("data=%f\n",f4[i]);
				*/
			}
			write(ofd,f4,recl*sizeof(float));
	printf("%d rec %d %d %d written\n",len,nvar, recl, recl*sizeof(float));
		}
		free(f8);
	}
	free(f4);
	return(0);
}

int      byteswap(src,siz)
unsigned char  *src;
int            siz;
{
   int tmp[2];
   unsigned char  *ptr;

   ptr = (unsigned char *)&tmp;

   if (siz==4)
   {
      ptr[0]=src[3];
      ptr[1]=src[2];
      ptr[2]=src[1];
      ptr[3]=src[0];
   }
   else if (siz==8)
   {
      ptr[0]=src[7];
      ptr[1]=src[6];
      ptr[2]=src[5];
      ptr[3]=src[4];
      ptr[4]=src[3];
      ptr[5]=src[2];
      ptr[6]=src[1];
      ptr[7]=src[0];
   }
   memcpy(src,ptr,(size_t)siz);

   return(0);
}

