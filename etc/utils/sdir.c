#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

main(int ac,char **av)
{
	FILE	*ifp;
	char	*ifile,*opt;
	int		swap=0,ifix=0;

	if (ac<2)
	{
		printf("Usage : %s [-s] filename\n",av[0]);
		exit(1);
	}

	ifile=av[1];
	if (av[1][0]=='-')
	{
      opt=av[1];
      while (*++opt != '\0')
      {
         switch (*opt)
         {
            case 's' :
               swap=1;
               break;
            case 'f' :
               ifix=1;
               break;
            default:
			      printf("Usage : %s [-s[f]] filename\n",av[0]);
			      exit(1);
		   }
      }
		ifile=av[2];
	}
		
	if ((ifp=fopen(ifile,"r"))==NULL)
	{
		printf("Can't open %s file\n",ifile);
		exit(1);
	}
	if (scan_file(ifp,swap,ifix)<0)
		exit(1);
	fclose(ifp);
	exit(0);
}

scan_file(ifp,swap,ifix)
FILE	   *ifp;
int		swap,ifix;
{
	int	rec_size,dum;
	int cnt=0,save;

	while (!feof(ifp))
	{
		if (fread(&rec_size,sizeof(int),1,ifp)==0) break;
		cnt++;
		if (swap) rec_size=byteswap(&rec_size);
		fseek(ifp,(long)rec_size,SEEK_CUR);
		if (fread(&dum,sizeof(int),1,ifp)==0) break;
		if (swap) dum=byteswap(&dum);
		if (dum!=rec_size)
		{
			printf("rec_size != dum\n",rec_size,dum);
			return(-1);
		}
printf("rec_size=%d\n",rec_size);
      if (ifix==1)
      {
		   if (cnt==3) save=rec_size;
		   if (cnt>3 && rec_size != save)
		   {
			   printf("wrong rec size %d %d\n",save,rec_size);
			   return(-1);
		   }
      }
	}
	return(0);
}

int		byteswap(src)
unsigned char	*src;
{
	int tmp;
	unsigned char	*ptr;

	ptr = (unsigned char *)&tmp;

	ptr[0]=src[3];
	ptr[1]=src[2];
	ptr[2]=src[1];
	ptr[3]=src[0];

	return(tmp);
}
