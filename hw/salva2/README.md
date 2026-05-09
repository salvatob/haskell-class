# Salvat Parser

Zadání bylo podstatně složitější než jsem čekal (indentace v tokenizátoru), nicméně jsem splinl alespoň minimální zadání...

Přikládám tedy pár zajímavějších fíčur co jsem udělal:

Umí to parsovat věci jako floaty, stringy, uzávorkované výrazy...


"""python
x=.2
h="Hello World!"
c='p'
r = 2*(a+b)
"""


"""js
x <- 0.2;
h <- "Hello World!";
c <- 'p';
r <- 2 * a + b;
"""


Tady jsem vyhrabal a trošičku uúravil nejaký úkol z PRG1.
"""python
def all_parenthesizations(n, left, right, current_variation):
    if left - right < n:
        current_variation_append(")" * n)
        print(str_join(current_variation))
        current_variation_pop()
        return 0

    current_variation_append("(")
    all_parenthesizations(n - 1, left + 1, right, current_variation)
    current_variation_pop()
    if left > right:
        current_variation_append(")")
        all_parenthesizations(n - 1, left, right + 1, current_variation)
        current_variation_pop()


all_parenthesizations(int(input()) * 2)
"""

"""js
function all_parenthesizations(n, left, right, current_variation) {
    if (left - right < n) {
        current_variation_append(")" * n);
        print(str_join(current_variation));
        current_variation_pop();
        return 0;
    }

    current_variation_append("(");
    all_parenthesizations(n - 1, left + 1, right, current_variation);
    current_variation_pop();
    if (left > right) {
        current_variation_append(")");
        all_parenthesizations(n - 1, left, right + 1, current_variation);
        current_variation_pop();
    }

}

all_parenthesizations(int(input()) * 2);
"""