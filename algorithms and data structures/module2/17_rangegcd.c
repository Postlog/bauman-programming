#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int gcd(int a, int b)
{
    int t;
    a = abs(a);
    b = abs(b);
    while (b > 0) {
        a %= b;
        t = a;
        a = b;
        b = t;
    }
    return a;
}

void compute_lgithms(int m, int size,int *lg)
{
    int j = 0;
    for(int i = 1; i <= m; i++)
        for(;j < (1 << i); j++)
            lg[j] = i - 1;
}


int sparse_table_query(int **ST, int l, int r, int *lg)
{
    int j = lg[r - l + 1];
    return gcd(ST[l][j], ST[r - (1 << j) + 1][j]);
}

void sparse_table_build(int *v, int n, int *lg,int ***pST)
{
    int m = lg[n] + 1,
    	**ST = (int**)malloc(sizeof(int *) * n);
    for (int i = 0; i < n; i++)
        ST[i] = (int*)malloc(sizeof(int) * m);
    
    for(int i = 0; i < n; i++) ST[i][0] = v[i];

    for(int i = 1; i < m; i++)
        for(int j = 0; j <= n - (1 << i); j++)
            ST[j][i] = gcd(ST[j][i-1], ST[j + (1 << (i - 1))][i - 1]);
    *pST = ST;
}

int main(){
    int n;
    scanf("%i", &n);
    int nums[n];

    for(int i = 0; i < n; i++) scanf("%i", &nums[i]);

    int m = log2(n) + 1,
    	lg_size = (1 << (m + 1)),
    	lg[lg_size];

    compute_lgithms(m, lg_size, lg);
    int **ST;
    sparse_table_build(nums, n, lg, &ST);

    int c;
    scanf("%i", &c);
    for(int i = 0; i < c; i++)
    {
    	int l, r;
        scanf("%i %i", &l, &r);
		printf("%d\n", sparse_table_query(ST, l, r, lg));
    } 
    
    for (int i = 0; i < n; i++)
        free(ST[i]);
    free(ST);
    return 0;
}