import abc
from typing import Any, TypeVar, Generic


class BasePrologCallable(abc.ABC):
    pass


class Processed:
    def __init__(self, value: str):
        self.string = value
    def __str__(self):
        return str(self.string)
    def __repr__(self):
        return str(self.string)
    def __contains__(self, item):
        return item in self.string

    # methods to imitate/delegate the str object
    def __eq__(self, other):
        return str(self) == str(other)
    def __getitem__(self, i):
        return self.string.__getitem__(i)
    def __iter__(self):
        return iter(self.string)
    def __len__(self):
        return len(self.string)
    def __getattr__(self, name):
        return getattr(self.string, name)


class PrologCallable(BasePrologCallable):
    def __init__(self, name):
        self.name = name

    def __call__(self, *args: Any):
        ret = f"{self.name}"
        if len(args) == 0:
            return Processed(ret)
        args = map(stringify, args)
        ret += "(" + (", ".join(args)) + ")"
        return Processed(ret)

    def __str__(self):  # for zero arity predicates (aka variables)
        return str(self.name)
    def __repr__(self):
        return str(self.name)
    def __hash__(self):
        return hash(str(self))
    def __eq__(self, other):
        return str(self) == other


Variable = PrologCallable


class PrologOperator(BasePrologCallable):
    def __init__(self, operator_separator: str, opening="", enclosing=""):
        self.separator = operator_separator
        self.opening = opening
        self.enclosing = enclosing

    def __call__(self, *args: list[str]):
        assert len(args) > 0
        args = map(stringify, args)
        return Processed(self.opening + (self.separator.join(args)) + self.enclosing)



def stringify(value):
    if isinstance(value, (BasePrologCallable, Processed)):
        return repr(value)
    if isinstance(value, (int, str, float)):
        return repr(value)
    if isinstance(value, (list, tuple)):
        value = list(map(stringify, value))
        value = ", ".join(value)
        return f"[{value}]"
    raise NotImplementedError


def new_class_init(self, *args):
    self.args = args

def new_class_repr(self) -> str:
    ret = self.__call__(*self.args)
    if isinstance(ret, Processed):
        ret = repr(ret)
    return ret

def new_class_eq(self, other) -> bool:
    return str(self) == str(other)

def define_parameterized_predicate(name):

    return type(name, (PrologCallable,), {
        'name': name,
        '__init__': new_class_init,
        '__str__': new_class_repr,
        '__repr__': new_class_repr,
        '__eq__': new_class_eq,
    })


def define_parameterized_functor(name):
    """
    Gaada bedanya dengan define_parameterized_predicate.
    Dibedakan hanya untuk readability
    :param name: nama dari functor
    """
    return type(name, (PrologCallable,), {
        'name': name,
        '__init__': new_class_init,
        '__str__': new_class_repr,
        '__repr__': new_class_repr,
        '__eq__': new_class_eq,
    })

def define_variable(name):
    return Variable(name)


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


# =============== Specific Definitions ===============

T = TypeVar("T")


class PrologList(Generic[T], define_prolog_operator("PrologList", ",", "[", "]")):
    def __init__(self, *args: T):
        super().__init__(*args)
