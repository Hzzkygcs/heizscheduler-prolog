from HzzProlog.PrologCallable import PrologCallable

MAIN_PROLOG_FILE = "prolog.pl"
MAIN_PROLOG_FILE_IO = open("../prolog.pl")


X = PrologCallable("X")
Y = PrologCallable("Y")
Z = PrologCallable("Z")

time = PrologCallable("time")
hari = PrologCallable("hari")
available = PrologCallable("available")