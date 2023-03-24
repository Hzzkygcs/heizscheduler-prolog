class PrologCallable:
    def __init__(self, name):
        self.name = name

    def __call__(self, *args: list[str]):
        ret = f"{self.name}"
        if len(args) == 0:
            return ret
        ret += "("
        for arg in args:
            ret += str(arg)
        ret += ")"
        return ret
