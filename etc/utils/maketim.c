#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <string.h>

extern int daylight;
extern long timezone;
extern char *tzname[];

main(int ac,char **av)
{
	time_t		t,nt;
	struct tm	*nst,st,*localtime();
	int			i,len,dum,ismin,sub;
	int			ttt[5],dtt[5];
	char		diff_str[40];
	int			dptr,only_hour,hdiff,dhour;

	if (ac < 3)
	{
		fprintf(stderr,"Usage : %s YYYYMMDDhhmm [-h/d] diff(YYYYMMDDhhmm)\n",av[0]);	
		exit(1);
	}
	for (i=0; i<5; i++)
	{
		ttt[i]=0;
		dtt[i]=0;
	}
	len=strlen(av[1]);
	if (len<10 || len>12)
	{
		fprintf(stderr,"Too short start time\n");
		fprintf(stderr,"Usage : %s YYYYMMDDhhmm [-h/d] diff(YYYYMMDDhhmm)\n",av[0]);
		exit(1);
	}
	else if (len==10)
		ismin=0;
	else ismin=1;

	hdiff=0;
	if (av[2][0]=='-' && !(av[2][1]=='h' || av[2][1]=='d' || isdigit(av[2][1])))
	{
		fprintf(stderr,"Invalid option\n");
		fprintf(stderr,"Usage : %s YYYYMMDDhhmm [-h/d] diff(YYYYMMDDhhmm)\n",av[0]);
		exit(1);
	}
	else if (strncmp(av[2],"-h",2)==0)
	{
		dptr=3;
		only_hour=1;
	}
	else if (strncmp(av[2],"-d",2)==0)
	{
		dptr=3;
		only_hour=1;
		hdiff=1;
	}
	else
	{
		dptr=2;
		only_hour=0;
	}
	if (strlen(av[dptr])<=0)
	{
		fprintf(stderr,"Too short diffrence time\n");
		fprintf(stderr,"Usage : %s YYYYMMDDhhmm [-h] diff(YYYYMMDDhhmm)\n",av[0]);
		exit(1);
	}
	switch (av[dptr][0])
	{
		case '-':
			sub=1;
			strcpy(diff_str,av[dptr]+1);
			break;
		case '+':
			sub=0;
			strcpy(diff_str,av[dptr]+1);
			break;
		default:
			sub=0;
			strcpy(diff_str,av[dptr]);
			break;
	}

	if (ismin==1)
		dum=sscanf(av[1],"%4d%2d%2d%2d%2d",&ttt[0],&ttt[1],&ttt[2],&ttt[3],&ttt[4]);
	else
	{
		dum=sscanf(av[1],"%4d%2d%2d%2d",&ttt[0],&ttt[1],&ttt[2],&ttt[3]);
		ttt[4]=0;
		dtt[4]=0;
		dum++;
	}
	if (dum!=5)
	{
		fprintf(stderr,"read start time failed\n");
		fprintf(stderr,"Usage : %s YYYYMMDDhhmm [-h/d] diff(YYYYMMDDhhmm)\n",av[0]);
		exit(1);
	}
	if (ttt[0]>=2038)
	{
		fprintf(stderr,"This program could not be run with year>=2038\n");
		fprintf(stderr,"Please check Your O/S and modify sources of this\n");
		exit(1);
	}

	len=strlen(diff_str);
	if (len%2 != 0)
	{
		len++;
		strcpy(diff_str+20,diff_str);
		strcpy(diff_str,"0");
		strcat(diff_str,diff_str+20);
	}

	/*
	 * Get diff. time
	 */
	if (hdiff)
	{
		sscanf(diff_str,"%4d%2d%2d%2d",&dtt[0],&dtt[1],&dtt[2],&dtt[3]);
		dtt[4]=0;
	}
	else if (only_hour)
	{
		diff_str[len]='\0';
		sscanf(diff_str,"%d#",&dtt[3]);
	}
	else
	{
		for (i=0; i<len/2; i++)
		{
			sscanf(diff_str+len-2*(i+1),"%2d",&dum);
			if (ismin)
			{
				if (i==5) dtt[0]=dtt[0]+dum*100;
				else dtt[4-i]=dum;
			}
			else
			{
				if (i==4) dtt[0]=dtt[0]+dum*100;
				else dtt[3-i]=dum;
			}
		}
	}
/*
	for (i=0; i<5; i++)
		fprintf(stderr,"diff[%d]=%d\n",i,dtt[i]);

	for (i=0; i<5; i++)
		fprintf(stderr,"time[%d]=%d\n",i,ttt[i]);
*/

	if (hdiff!=1 && (dtt[0]!=0 || dtt[1]!=0))
	{
		fprintf(stderr,"This program still does not support for month or year\n");
		fprintf(stderr,"  Sorry \n");
		exit(1);
	}
	tzset();
	st.tm_year=ttt[0]-1900;
	st.tm_mon =ttt[1]-1;
	st.tm_mday=ttt[2];
	st.tm_hour=ttt[3];
	st.tm_min =ttt[4];
	st.tm_sec =0;
	st.tm_isdst =1;			/* 1=daylight, 0=no daylight, -1=up to system */

	t=mktime(&st);
	if (hdiff)
	{
		st.tm_year=dtt[0]-1900;
		st.tm_mon =dtt[1]-1;
		st.tm_mday=dtt[2];
		st.tm_hour=dtt[3];
		st.tm_min =dtt[4];
		st.tm_sec =0;
		st.tm_isdst =1;			/* 1=daylight, 0=no daylight, -1=up to system */
  
		nt=mktime(&st);
        if(nt>t)
          dhour=(nt-t)/3600;
        else
          dhour=(t-nt)/3600;
/* temporary option for 24-h differences*/
        if (dtt[3]-ttt[3]==0)
        {
          if(dhour%24==1)  dhour=dhour-1;
          if(dhour%24==23) dhour=dhour+1;
        }
/* end of temporary option */

		fprintf(stdout,"%d\n",dhour);
	}
	else
	{
		nt=60*(dtt[4]+60*(dtt[3]+24*dtt[2]));
		if (sub) t=t-nt;
		else t=t+nt;
		nst=localtime(&t);

		if (ismin)
			fprintf(stdout,"%d%02d%02d%02d%02d\n",
				nst->tm_year+1900, nst->tm_mon+1, nst->tm_mday, nst->tm_hour, nst->tm_min);
		else
			fprintf(stdout,"%d%02d%02d%02d\n",
				nst->tm_year+1900, nst->tm_mon+1, nst->tm_mday, nst->tm_hour);
	}
	exit(0);
}
