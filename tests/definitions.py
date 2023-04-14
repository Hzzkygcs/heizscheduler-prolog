import os

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import PrologCallable, PrologOperator, define_prolog_callable, define_prolog_operator

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = os.path.abspath("../prolog.pl")
TIME_CONFLICT_PL_IO = os.path.abspath("../time_conflict.pl")
HZZ_TIME_PL_IO = os.path.abspath("../hzztime.pl")

dont_care = 0  # arbitrary value

X = define_prolog_callable("X")
Y = define_prolog_callable("Y")
Z = define_prolog_callable("Z")
Result = define_prolog_callable("Result")

time = define_prolog_callable("time")
hari = define_prolog_callable("hari")
add_time = define_prolog_callable("add_time")
subtract_time = define_prolog_callable("subtract_time")
convert_from_minutes = define_prolog_callable("convert_from_minutes")

range = define_prolog_callable("range")


class datetime(define_prolog_operator('datetime', ":")):
    def __init__(self, day: int, hour: int, minute: int):
        super().__init__(day, hour, minute)


class have_time(define_prolog_callable('have_time')):
    def __init__(self, npm: int, is_preferred: int, start: datetime, end: datetime):
        super().__init__(npm, is_preferred, start, end)


class all_npm(define_prolog_callable("all_npm")):
    def __init__(self, npm: int):
        super().__init__(npm)


available = define_prolog_callable("available")
time_in_range = define_prolog_callable("time_in_range")
duration = define_prolog_callable("duration")
time_point = define_prolog_operator("time_point", ":")
time_range = define_prolog_operator("time_range", "-")



def define_tokenizer_regex(hzz_prolog: HzzProlog):
    hzz_prolog.add_new_regex(150, "\d+:\d+:\d+")
    return hzz_prolog
