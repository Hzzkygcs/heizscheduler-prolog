from random import choice

from HzzProlog.PrologCallable import define_variable

_ = define_variable("_")
X = define_variable("X")
Y = define_variable("Y")
Z = define_variable("Z")
Result = define_variable("Result")

X1 = define_variable("X1")
Y1 = define_variable("Y1")

# randomized to proof that the value is truly neglegible when this variable is used
dont_care = choice(["dontcare", "dont_care", "idontcare", "whatever", "letItBeAnything"])
