#include <stdio.h>

extern char **environ;

int main(int argc, char *argv[])
{
    puts("test stdout");
    printf("argc %d\n", argc);
    for (int i = 0; i < argc; i++)
    {
        printf("argv[%d]: %s\n", i, argv[i]);
    }
    char **s = environ;
    for (; *s; s++)
    {
        printf("env: %s\n", *s);
    }
    printf("stdin: ");
    int c;
    while ((c = getchar()) != EOF)
    {
        putchar(c);
    }
    fprintf(stderr, "test stderr\n");
}
