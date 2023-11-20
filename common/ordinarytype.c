#include <stdio.h>
#include <stddef.h>
int main()
{
    printf("size_t:%d\n",sizeof(size_t));
    printf("ptrdiff_t:%d\n",sizeof(ptrdiff_t));
    printf("int:%d\n",sizeof(int));
    printf("unsigned:%d\n",sizeof(unsigned));
    printf("long:%d\n",sizeof(long));
    printf("long long:%d\n",sizeof(long long));
    printf("float:%d\n",sizeof(float));
    printf("double:%d\n",sizeof(double));
    printf("char *:%d\n",sizeof(char *));
    printf("int *:%d\n",sizeof(int *));
    printf("long *:%d\n",sizeof(long *));
    printf("float *:%d\n",sizeof(float *));
    printf("double *:%d\n",sizeof(double *));
    return 0;
    /* gcc 64 bit cpu 
size_t:8
ptrdiff_t:8
int:4
unsigned:4
long:8
long long:8
float:4
double:8
char *:8
int *:8
long *:8
float *:8
double *:8

    */
}