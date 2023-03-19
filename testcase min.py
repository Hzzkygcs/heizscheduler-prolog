import Prolog
from random import randint
from typing import Callable, Optional

prolog = Prolog.Prolog("min_valen.pl")

for i in range(400):
    print(i)
    n = randint(1, 30)
    arr = [randint(-300, 300) for _i in range(n)]

    query = f"min({arr}, Result)"
    res = prolog.query(query)
    if len(res) != 1:
        print(query)
        raise RuntimeError("Hasilnya ga sebanyak tepat satu")
    actual = res[0]['Result']
    expected = min(arr)
    if expected != actual:
        print(query)
        print('exp', expected)
        print('act', actual)
        raise RuntimeError("Hasilnya ga sebanyak tepat satu")

