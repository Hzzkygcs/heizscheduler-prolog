import Prolog
from random import randint
from typing import Callable, Optional

prolog = Prolog.Prolog("inner_nuel.pl")

res = prolog.query_raw(" X=[1,2,3,454,56,6,6,76,7,8,8,8]")
print(res)
