from HzzProlog.PrologCallable import define_parameterized_predicate, Variable


class member(define_parameterized_predicate("member")):
    def __init__(self, member: Variable, lst: list):
        super().__init__(member, lst)