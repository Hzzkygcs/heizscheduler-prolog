from HzzProlog.PrologCallable import PrologCallable, PrologOperator

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = open("../prolog.pl")


X = PrologCallable("X")
Y = PrologCallable("Y")
Z = PrologCallable("Z")

time = PrologCallable("time")
hari = PrologCallable("hari")
available = PrologCallable("available")
time_in_range = PrologCallable("time_in_range")
time_point = PrologOperator(":")
