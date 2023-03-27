from HzzProlog.PrologCallable import PrologCallable, PrologOperator

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = open("../prolog.pl")
TIME_CONFLICT_PL_IO = open("../time_conflict.pl")
HZZ_TIME_PL_IO = open("../hzztime.pl")


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


available = PrologCallable("available")
time_in_range = PrologCallable("time_in_range")
duration = PrologCallable("duration")
time_point = PrologOperator(":")
time_range = PrologOperator("-")
