

class PrologScriptNotFoundException(FileNotFoundError):
    def __init__(self, *args):
        super().__init__(*args)
