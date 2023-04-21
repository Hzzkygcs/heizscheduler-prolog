from HzzProlog.PrologCallable import define_variable
from random import choice

X = define_variable("X")
Y = define_variable("Y")
Z = define_variable("Z")
Result = define_variable("Result")

# randomized to proof that the value is truly neglegible when this variable is used
dont_care = choice(["dontcare", "dont_care", "idontcare", "whatever", "letItBeAnything"])
