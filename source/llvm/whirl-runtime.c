//--------------------------------------------
// WHIRL PROGRAMMING LANGUAGE LLVM COMPILER
// by: BigZaphod sean@fifthace.com
// http://www.bigzaphod.org/whirl/
//
// License: Public Domain
// May 12, 2009
//--------------------------------------------
#include <stdio.h>
#include <stdlib.h>

int WhirlPrintInt(int i)
{
  printf("%d",i);
  return 0;
}

int WhirlPrintASCII(int i)
{
  printf("%c",i);
  return 0;
}

int WhirlReadInt(int i)
{
  // read integer
  char buf[100];
  int c = 0;
  while( c < sizeof(buf)-1 )
  { 
    buf[c] = getchar();
    c++;
    buf[c] = 0;
    
    if( buf[c-1] == '\n' )
      break;
  }
  // swallow, just in case.
  if( c == sizeof(buf) )
    while( getchar() != '\n' );
  
  return atoi( buf );
}

int WhirlReadASCII(int i)
{
  // read character
  int c = getchar();
  while( getchar() != '\n' );
  return c;
}
