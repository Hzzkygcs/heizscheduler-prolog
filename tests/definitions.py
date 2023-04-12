import os

from HzzProlog.HzzProlog import HzzProlog
from HzzProlog.PrologCallable import PrologCallable, PrologOperator

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = os.path.abspath("../prolog.pl")
TIME_CONFLICT_PL_IO = os.path.abspath("../time_conflict.pl")
HZZ_TIME_PL_IO = os.path.abspath("../hzztime.pl")

dont_care = 0  # arbitrary value

X = PrologCallable("X")
Y = PrologCallable("Y")
Z = PrologCallable("Z")
Result = PrologCallable("Result")

time = PrologCallable("time")
hari = PrologCallable("hari")
add_time = PrologCallable("add_time")
subtract_time = PrologCallable("subtract_time")
convert_from_minutes = PrologCallable("convert_from_minutes")

range = PrologCallable("range")
datetime = PrologOperator(":")

def have_time(npm, is_preferred, start, end):
    func = PrologCallable("have_time")
    return func(npm, is_preferred, start, end)

def all_npm(npm):
    func = PrologCallable("all_npm")
    return func(npm)

available = PrologCallable("available")


time_in_range = PrologCallable("time_in_range")
duration = PrologCallable("duration")
time_point = PrologOperator(":")
time_range = PrologOperator("-")


def define_tokenizer_regex(hzz_prolog: HzzProlog):
    hzz_prolog.add_new_regex(150, "\d+:\d+:\d+")
    return hzz_prolog