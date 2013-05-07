#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define BITS_PER_CHAR 8
#define BITS_PER_INT (sizeof(unsigned int)*BITS_PER_CHAR)

#define SIZE_OF_BITS(num) (((((num)-1)/BITS_PER_INT)+1)*4)

#define GET_BIT(bits, pos) (bits[(pos)/BITS_PER_INT] & (1 << ((pos) % BITS_PER_INT)))
#define SET_BIT(bits, pos) (bits[(pos)/BITS_PER_INT] |= (1 << ((pos) % BITS_PER_INT)))
#define CLEAR_BIT(bits, pos) (bits[(pos)/BITS_PER_INT] &= ~(1 << ((pos) % BITS_PER_INT)))

unsigned int populate_prime_bits(unsigned int *prime_bits, unsigned int num)
{
    unsigned int step;
    unsigned long long int work = 0;
    unsigned int num_primes = 0;
    
    memset(prime_bits, 0xFF, SIZE_OF_BITS(num));
    if (num < 2)
        return 0;
    
    CLEAR_BIT(prime_bits, 0);
    CLEAR_BIT(prime_bits, 1);
    
    step = 2;
    while (step < num)
    {
        unsigned int i;
        num_primes++;
        
        for (i = step*2; i < num; i += step)
        {
            CLEAR_BIT(prime_bits, i);
        }
        work += num/step;
        
        do
        {
            step++;
            work++;
        }
        while (step < num && !GET_BIT(prime_bits, step));
    }
    
    printf("work %lld\n", work);
    return num_primes;
}

void print_bits(unsigned int *bits, unsigned int num)
{
    unsigned int i;
    
    const char *sep = "";
    
    for (i = 0; i < num; i++)
    {
        if (GET_BIT(bits, i))
        {
            printf("%s%d", sep, i);
            sep = " ";
        }
    }
}

unsigned int construct_list(unsigned int *list, unsigned int expected, unsigned int *bits, unsigned int num)
{
    unsigned int next = 0;
    unsigned int i;
    
    for (i = 0; i < num; i++)
    {
        if (GET_BIT(bits, i))
        {
            list[next] = i;
            next++;
            if (next >= expected)
                break;
        }
    }
    
    return next;
}

void print_list(unsigned int *list, unsigned int num)
{
    unsigned int i;
    
    const char *sep = "";
    
    for (i = 0; i < num; i++)
    {
        printf("%s%d", sep, list[i]);
        sep = " ";
    }
}

void find_longest_sum(unsigned int *bits, unsigned int num, unsigned int *primes, unsigned int num_primes)
{
    unsigned int best_sum = 0;
    unsigned int best_len = 0;
    
    unsigned int start;
    for (start = 0; start < num_primes; start++)
    {
        unsigned int sum = 0;
        unsigned int len = 0;
        unsigned int i;
        
        for (i = start; i < num_primes; i++)
        {
            sum += primes[i];
            len++;
            if (sum >= num)
                break;
            
            if (GET_BIT(bits, sum))
            {
                if (len > best_len)
                {
                    best_sum = sum;
                    best_len = len;
                }
            }
        }
    }
    
    printf("best %d %d\n", best_sum, best_len);
}

void print_time()
{
    float seconds = clock() / (float) CLOCKS_PER_SEC;
    printf("time %1.3f\n", seconds);
}

int main(int argc, char *argv[])
{
    unsigned int max = 1000000000;
    unsigned int *prime_bits;
    unsigned int num_primes;
    unsigned int *primes;
    
    prime_bits = malloc(SIZE_OF_BITS(max));

    print_time();
    num_primes = populate_prime_bits(prime_bits, max);
    printf("num_primes %d\n", num_primes);
    print_time();
    
    //print_bits(prime_bits, max);
    //printf("\n");
    
    primes = malloc(sizeof(unsigned int) * num_primes);
    construct_list(primes, num_primes, prime_bits, max);
    print_time();

    //print_list(primes, num_primes);
    //printf("\n");
    
    find_longest_sum(prime_bits, max, primes, num_primes);
    print_time();
    
    free(primes);
    free(prime_bits);
    
    return 0;
}
