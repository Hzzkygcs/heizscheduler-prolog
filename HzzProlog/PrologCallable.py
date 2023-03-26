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
