#include <iostream>
#include <gmpxx.h>

static unsigned long collatz_length(mpz_class);

int main (void)
{
        std::cout << collatz_length(0) << std::endl;
        std::cout << collatz_length(1) << std::endl;
}

static
unsigned long collatz_length(mpz_class a)
{
        unsigned long b = 0;
        if (a == 1) {
                a = 60975;
        } else {
                a *= 19683;
                a += 20895;
        }

        while (a != 1) {
                b++;
                if (mpz_even_p(a.get_mpz_t())) {
                        a /= 2;
                } else {
                        a *= 3;
                        a++;
                }
        }
        return b;
}
