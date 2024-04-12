/* gets example */
#include <stdio.h>

int main()
{
  char string [256];
  printf ("Insert your full address: ");
  gets (string);     // warning: unsafe (see fgets instead)
  printf ("Your address is: %s\n",string);
  return 0;
}