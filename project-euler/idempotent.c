// Project euler problem 407
#include <stdio.h>

long idempotent(long count) 
{
  long i, max, tmp; 
  for (i = 0; i < count; i++) {
    tmp = (i * i) % count;
    if (tmp > max)
      max = tmp;
  }
  return max;
}

int main() 
{
  long sum, i, limit;
  scanf("%ld", &limit);   
  for (i = 1; i < limit; i++) {
    sum += idempotent(i);
  }
  printf("%ld\n", sum);
  return 0;
}
