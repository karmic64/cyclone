#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>


#define WIDTH 320
#define HEIGHT 224

#define TILEWIDTH (WIDTH/8)
#define TILEHEIGHT (HEIGHT/8)


uint8_t image[HEIGHT][WIDTH];
uint8_t palette[0x40*3];

uint16_t tilemap[TILEHEIGHT][TILEWIDTH];
uint8_t tiles[TILEHEIGHT*TILEWIDTH][0x30];
unsigned tilecnt = 0;


int main()
{
  FILE *f;
  f = fopen("image.data","rb");
  fread(image,1,sizeof(image),f);
  fclose(f);
  f = fopen("image.data.pal","rb");
  fread(palette,1,sizeof(palette),f);
  fclose(f);
  
  for (unsigned y = 0; y < TILEHEIGHT; y++)
  {
    for (unsigned x = 0; x < TILEWIDTH; x++)
    {
      uint8_t tilebuf[0x30];
      memset(tilebuf,0,sizeof(tilebuf));
      for (unsigned yf = 0; yf < 8; yf++)
      {
        uint8_t *tr = &tilebuf[yf*6];
        uint8_t row[6];
        memset(row,0,sizeof(row));
        for (unsigned xf = 0; xf < 8; xf++)
        {
          uint8_t color = image[y*8 + yf][x*8 + xf] & 0x3f;
          switch (xf)
          {
            case 0:
              row[5] |= color;
              break;
            case 1:
              row[4] |= color >> 2;
              row[5] |= color << 6;
              break;
            case 2:
              row[3] |= color >> 4;
              row[4] |= color << 4;
              break;
            case 3:
              row[3] |= color << 2;
              break;
            case 4:
              row[2] |= color;
              break;
            case 5:
              row[1] |= color >> 2;
              row[2] |= color << 6;
              break;
            case 6:
              row[0] |= color >> 4;
              row[1] |= color << 4;
              break;
            case 7:
              row[0] |= color << 2;
              break;
          }
        }
        tr[0] = row[4];
        tr[1] = row[5];
        tr[2] = row[2];
        tr[3] = row[3];
        tr[4] = row[0];
        tr[5] = row[1];
      }
      
      size_t found;
      for (found = 0; found < tilecnt; found++)
      {
        if (!memcmp(&tiles[found],tilebuf,0x30)) break;
      }
      tilemap[y][x] = found;
      if (found == tilecnt) /* new */
      {
        memcpy(&tiles[tilecnt++],tilebuf,0x30);
      }
      
    }
  }
  
  
  
  f = fopen("../secret-tiles","wb");
  fwrite(tiles,0x30,tilecnt,f);
  fclose(f);
  
  f = fopen("../secret-tilemap","wb");
  uint16_t *tm = &tilemap;
  for (size_t i = 0; i < TILEHEIGHT*TILEWIDTH; i++)
  {
    fputc(tm[i] >> 8,f);
    fputc(tm[i]&0xff,f);
  }
  fclose(f);
  
  f = fopen("../secret-palette","wb");
  for (size_t i = 0; i < 64; i++)
  {
    fputc(palette[i*3 + 1],f);
    fputc(palette[i*3 + 0],f);
    fputc(0,f);
    fputc(palette[i*3 + 2],f);
  }
  fclose(f);
}