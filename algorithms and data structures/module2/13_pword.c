#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int prefix(char *s, int s_len, int *pi)
{
    int t = 0;
    pi[0] = 0;
    for(int i = 1; i < s_len; i++)
    {
        while(t > 0 && s[t] != s[i])
            t = pi[t-1];

        if(s[t] == s[i]) t++;
        pi[i] = t;
    }
}

int prefixes_KMP(char *s, char *T){
    int s_len = strlen(s),
        t_len = strlen(T);
    int pi[s_len];
    prefix(s, s_len, pi);

    int t = 0;
    for(int i = 0; i < t_len; i++)
    {
        while(t > 0 && s[t] != T[i])
            t = pi[t-1];

        if(s[t] == T[i]) t++;
        
        if(t == 0) return 0; 

        if(t == strlen(s))
            t = pi[t-1];
    }

    return 1;
}

int main(int argc,char **argv)
{  
    char *S = argv[1], *T = argv[2];
    if(prefixes_KMP(S, T)) printf("yes\n");
    else printf("no\n");
    return 0;
}
