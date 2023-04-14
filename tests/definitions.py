import os

import deprecation

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import PrologCallable, PrologOperator, define_parameterized_predicate, \
    define_prolog_operator, \
    PrologList, define_variable

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = os.path.abspath("../prolog.pl")
TIME_CONFLICT_PL_IO = os.path.abspath("../time_conflict.pl")
HZZ_TIME_PL_IO = os.path.abspath("../hzztime.pl")

dont_care = 0  # arbitrary value

X = define_variable("X")
Y = define_variable("Y")
Z = define_variable("Z")
Result = define_variable("Result")

time = define_parameterized_predicate("time")
hari = define_parameterized_predicate("hari")
add_time = define_parameterized_predicate("add_time")
subtract_time = define_parameterized_predicate("subtract_time")
convert_from_minutes = define_parameterized_predicate("convert_from_minutes")

range = define_parameterized_predicate("range")


class datetime(define_prolog_operator('datetime', ":")):
    def __init__(self, day: int, hour: int, minute: int):
        super().__init__(day, hour, minute)


class have_time(define_parameterized_predicate('have_time')):
    def __init__(self, npm: int, is_preferred: int, start: datetime, end: datetime):
        super().__init__(npm, is_preferred, start, end)


class all_npm(define_parameterized_predicate("all_npm")):
    def __init__(self, npm: int):
        super().__init__(npm)


available = define_parameterized_predicate("available")
time_in_range = define_parameterized_predicate("time_in_range")
duration = define_parameterized_predicate("duration")


@deprecation.deprecated("time range using - operator is deprecated")
class time_range(define_prolog_operator("time_range", "-")):
    def __init__(self, start, end, *args):
        super().__init__(start, end)

class time_point(define_prolog_operator("time_point", ":")):
    def __init__(self, day: int, hour: int, minute: int=None):
        args = [day, hour]
        if minute is not None:
            args.append(minute)
        super().__init__(*args)


def define_tokenizer_regex(hzz_prolog: HzzProlog):  # special cases like time_point
    hzz_prolog.add_new_regex(150, "\d+:\d+:\d+")
    return hzz_prolog


