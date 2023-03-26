class PrologCallable:
    def __init__(self, name):
        self.name = name

    def __call__(self, *args: list[str]):
        ret = f"{self.name}"
        if len(args) == 0:
            return ret
        args = map(str, args)
        ret += "(" + (", ".join(args)) + ")"
        return ret

    def __str__(self):
        return str(self.name)


class PrologOperator:
    def __init__(self, operator_separator: str, opening="", enclosing=""):
        self.separator = operator_separator
        self.opening = opening
        self.enclosing = enclosing

    def __call__(self, *args: list[str]):
        assert len(args) > 0
        args = map(str, args)
        return self.opening + (self.separator.join(args)) + self.enclosing

