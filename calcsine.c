#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <math.h>


int main()
{
  FILE *f = fopen("sinetbl","wb");
  
  for (int ai = 0; ai < 512; ai++)
  {
    double angle = (ai/512.0)*(M_PI*2);
    
    double sine = sin(angle) * (0xfffe/2.0);
    int16_t s = round(sine);
    
    fputc((s>>8),f);
    fputc((s&0xff),f);
  }
  
  fclose(f);
}