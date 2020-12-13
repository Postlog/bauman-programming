#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void display_prefixes(char *s)
{
    int t = 0, s_len = strlen(s),
    	p[s_len];
    p[0] = 0;

    for(int i = 1; i < s_len; i++)
    {
        while(t > 0 && s[t] != s[i])
            t = p[t-1];
  
        if (s[t] == s[i]) t++;
        p[i] = t;

        int k = i + 1;
        if(p[i] > 0 && (k % (k - p[i])) == 0)
        	printf("%i %i\n", k, (int)(k / (k - p[i])));
    }
}

int main(int argc, char **argv)
{
    char *s = argv[1];
    
    display_prefixes(s);
    return 0;
}