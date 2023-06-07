#define _GNU_SOURCE         /* See feature_test_macros(7) */

#include <stdio.h>
#include <string.h>
#include "mex.h"

int to_int(char x)
{
  if (x >='0' && x <= '9')
    { return x-'0'; }

  if (x >= 'a' && x <= 'f')
    { return x-'a'+10; }

  return -1;
}

void
mexFunction (int nlhs, mxArray *plhs[],
             int nrhs, const mxArray *prhs[])
{
  mwSize m, n;
  mwIndex i, j;
  mxChar *pi, *po;
  char *pre_result, *result;
  double *arg;
  int junk, k;
  int src, dst, digit;
  int dot_found;
  
  if (nrhs != 1)
    {
      mexErrMsgTxt ("One and only one argument");
    }
  
  if (! mxIsDouble (prhs[0]))
    {
      mexErrMsgTxt ("ARG1 must be a double");
    }

  if (mxGetM(prhs[0]) != 1 || mxGetN(prhs[0]) != 1)
    {
      mexErrMsgTxt ("ARG1 must be a scalar");
    }

  arg = mxGetPr(prhs[0]);

  junk=asprintf(&pre_result, "%a", *arg);

  if (junk < 0)
    {
      mexErrMsgTxt ("Error converting the value (?)");
    }

  if (pre_result[0] != '0' || pre_result[1] != 'x')
    {
      mexErrMsgTxt ("bad format");
    }
    
  result = malloc(sizeof(char)*strlen(pre_result)*4);


  result[0] = '2';
  result[1] = '#';

  dst = 2;
  dot_found = 0;
  
  for (src = 2; pre_result[src] != 'p'; src++)
    {
      if (pre_result[src]=='.')
	{
	  result[dst] = '.';
	  dst++;
	  dot_found = 1;
	}
      else
	{
	  digit = to_int(pre_result[src]);

	  for (k=0; k<4; k++)
	    {
	      result[dst] = (digit & 0x8) ? '1' : '0';
	      dst ++;

	      digit = (digit & 7) << 1;
	    }
	}
    }

  src++;

  if (dot_found == 0)
    {
      result[dst]='.'; dst++;
      result[dst]='0'; dst++;
    }
  
  result[dst]='#'; dst++;
  result[dst]='e'; dst++;

  for ( ; pre_result[src]; src++)
    {
      result[dst] = pre_result[src];
      dst++;
    }

  result[dst]='\0';
  
  plhs[0] = mxCreateString(result);

  //  mexPrintf("[%s]\n", pre_result);
  
  free(result);
  free(pre_result);
  
}
