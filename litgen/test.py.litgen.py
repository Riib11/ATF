"""

       title: Literate Python 
    subtitle: A use case for the Literate<Generic> implementation of ATF 
      author: Henry Blanchette 
        data: January 7th, 2019 

abstract: This is a test document for prototyping my idea for language-generically implementing literate programming style. Typically documentation formats require restrictive commands for the actually documentation code, and allow little freedom about its ultimate formatting. This is useful for standard formats for hackage, javadocs, etc., but not very good for beautiful, literate programs. ATF is a good solution because it allows more syntax freedom and simplicity as well easy exportation to many typesetting formats such as HTML and LaTex. 

"""


def factorial_recursive(n):
    assert (n >= 0)
    return 0 if (n == 0) else n * factorial(n - 1)



#
# fold_left(f: B->A->B, x0: B, xs: [A]) : B
#
def fold_left(f, x0, xs):
    for x in xs: x0 = f(x0, x)
    return x0

def factorial_folding(n):
    return fold_left(int.__mul__, 1, range(1, n))



#
# fold_right(f: A->B->B, x0: B, xs: [A]) : B
#
def fold_right(f, x0, xs):
    for x in xs: x0 = f(x, x0)
    return x0


