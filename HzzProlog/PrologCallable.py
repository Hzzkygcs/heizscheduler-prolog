import abc
from typing import Any


class BasePrologCallable(abc.ABC):
    pass


class PrologCallable(BasePrologCallable):
    def __init__(self, name):
        self.name = name

    def __call__(self, *args: Any):
        ret = f"{self.name}"
        if len(args) == 0:
            return ret
        args = map(str, args)
        ret += "(" + (", ".join(args)) + ")"
        return ret

    def __str__(self):
        return str(self.name)
    def __repr__(self):
        return repr(self.name)
    def __hash__(self):
        return hash(str(self))
    def __eq__(self, other):
        return str(self) == other


class PrologOperator(BasePrologCallable):
    def __init__(self, operator_separator: str, opening="", enclosing=""):
        self.separator = operator_separator
        self.opening = opening
        self.enclosing = enclosing

    def __call__(self, *args: list[str]):
        assert len(args) > 0
        args = map(str, args)
        return self.opening + (self.separator.join(args)) + self.enclosing


def new_class_init(self, *args):
    self.args = args

def new_class_repr(self) -> str:
    return self.__call__(*self.args)

def new_class_eq(self, other) -> bool:
    return str(self) == str(other)

def define_prolog_callable(name):

    return type(name, (PrologCallable,), {
        'name': name,
        '__init__': new_class_init,
        '__str__': new_class_repr,
        '__repr__': new_class_repr,
        '__eq__': new_class_eq,
    })


def define_prolog_operator(name, operator_separator: str, opening="", enclosing=""):
    return type(name, (PrologOperator,), {
        'separator': operator_separator,
        'opening': opening,
        'enclosing': enclosing,
        '__init__': new_class_init,
        '__str__': new_class_repr,
        '__repr__': new_class_repr,
        '__eq__': new_class_eq,
    })

