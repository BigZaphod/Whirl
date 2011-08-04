#include/* by Kang Seonghoon <tokigun@gmail.com> */<stdio.h>
FILE*f;int P[99999],*d=P,*p=P,U[99999],*u=U,q,s,t,r[2],v[2],
w[2];int main(int i,char**I){if(i-2)return!puts("TokigunStu\
dio Whirl Interpreter by Kang Seonghoon <tokigun@gmail.com>"
);if(f=**++I-45||1[*I]?fopen(*I,"r"):stdin){while((*d=fgetc(
f))>=0  )*d/2-24  ?0  :(*d  ++      -=      48)  ;for(;p<d;q
=!*p++  &&!q)if(  *p  )v[s  ]=(v  [s]-  r[s  ]+  13)%12;else
#define  O(  n)  ;;}        else  /**/      if(  !(v[s]-n)){
{r[s]^=  2;  if  (q)  {t=w  [s];  if(s  ){;  if  (0){O(1)t=*
u;O(2)*u        =t;O  (3)t  +=*u  O(4)  t*=  *u  O(5)t/=*u;O
(6)t=0;O(  7)  t=t<*  u;O(  8)      t=  t>*  u;       O(9)t=
t==*u;O(10)t=!t;O(11)t=-t;}O(1)return 0;O(2)t=1;O(3)t=0;O(4)
t=*u;O(5)*u=t;O(6)p+=t-1;O(7)u+=t;O(8)t=*u&&t;O(9)p+=*u?t-1:
0;O(10)t?printf("%d",*u):scanf("%d",u);O(11)*u=t?putchar(*u)
:getchar();}w[s]=t;s=!s;if(p<P)p=P;}}fclose(f);}else printf(
"File Not Found: %s\n",*I);return!f;}/*20050617rev3tokigun*/




/*
TokigunStudio Whirl Interpreter, revision 3 (2005-06-17)
Copyright (c) 2005, Kang Seonghoon (Tokigun).
This program is distributable under the GNU LGPL.

Quick Documentation:
whirl               to view copyright,
whirl <filename>    to execute <filename>,
whirl -             to input the code and execute.
*/
