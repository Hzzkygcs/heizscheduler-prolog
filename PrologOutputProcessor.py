from typing import overload, TypeVar, Union

from more_itertools import peekable
import subprocess
import codecs
import enum
from pyswip import Prolog


class TokenType(enum.Enum):
    VARIABLE = 1
    ATOM = 2
    NUMERIC = 3
    OPERATOR = 4

class PrologException(Exception):
    pass


class Prologg:
    def __init__(self):
        pass


        self.p = subprocess.Popen(["swipl", '--quiet', "inner.pl"],
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, text=True)

    def query(self):
        # global stdout
        query = "!"
        # query = "append(X, Y, Z), append(Z, A, [1, 2])"
        stdout, stderr = self.p.communicate(input=query+", write('end').\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;")
        print(stdout)
        print(stderr)

stdout = """
end
X = Y, Y = Z, Z = [],
A = [1, 2] end
X = [],
Y = Z, Z = [1],
A = [2] end
X = A, A = [],
Y = Z, Z = [1, 2] end
X = Z, Z = [1],
Y = [],
A = [2] end
X = [1],
Y = [2],
Z = [1, 2],
A = [] end
X = Z, Z = [1, 2],
Y = A, A = []
"""

class CharType(enum.Enum):
    ALPHA_NUMERIC = 1
    WHITESPACE = 2
    SYMBOLS = 3

    @classmethod
    def get_type(cls, char: str):
        if char.isalnum():
            return cls.ALPHA_NUMERIC
        if char.isspace():
            return cls.WHITESPACE
        return cls.SYMBOLS


_T = TypeVar('_T')


class NotAvailable():
    pass


class TokenizerIterator(peekable):
    def __init__(self, string):
        tokenized = HelperTokenizerIterator(string).tokenize()
        super().__init__(tokenized)
        self.include_space = False

    def set_include_space(self, value):
        self.include_space = value

    def peek(self, default=NotAvailable()):
        if isinstance(default, NotAvailable):
            return self.__peek()

        try:
            return self.__peek()
        except StopIteration:
            return default

    def __peek(self):
        try:
            i = 0
            while not (self.include_space or not super().__getitem__(i).isspace()):
                i += 1
            return super().__getitem__(i)
        except IndexError:
            raise StopIteration

    def do_next_if_currently_whitespace(self, multiple_times):
        while super().peek().isspace():
            super().__next__()
            if not multiple_times:
                return

    def __next__(self):
        while True:
            peek: str = super().peek()
            if self.include_space or not peek.isspace():
                break
            super().__next__()
        return super().__next__()



class HelperTokenizerIterator:
    def __init__(self, string):
        self.string = string
        self.ret = []
        self.curr_token = ""  # currently being-built token
        self.prev_char_type = CharType.ALPHA_NUMERIC

    def tokenize(self):
        for char in self.string:
            if char.isalnum():
                self.push_if_different_type(CharType.ALPHA_NUMERIC, char)
                self.curr_token += char
                continue
            if char.isspace():
                self.push_if_different_type(CharType.WHITESPACE, char)
                self.curr_token += char
                continue
            self.push_if_different_type(CharType.SYMBOLS, char)
            self.ret.append(char)
        if self.curr_token != "":
            self.ret.append(self.curr_token)
        return self.ret

    def push_if_different_type(self, desired_type: CharType, curr_char: str):
        if self.curr_token == "":
            self.prev_char_type = CharType.get_type(curr_char)
            return
        if self.prev_char_type == desired_type:
            return
        self.ret.append(self.curr_token)
        self.curr_token = ""
        self.prev_char_type = CharType.get_type(curr_char)


class PrologOutputProcessor:
    def __init__(self, string):
        self.iter_tokens = TokenizerIterator(string)

    def get_token_status(self, token):
        assert len(token) >= 1
        if token.isalpha():
            if token[0].isupper():
                return TokenType.VARIABLE
            return TokenType.ATOM
        if token.isnumeric():
            return TokenType.NUMERIC
        return TokenType.OPERATOR

    def processToken(self, splitter_token='end'):
        if len(self.tokens) == 0:
            return []
        if self.iter_tokens[0] == splitter_token:
            next(self.iter_tokens)
        self._processToken(splitter_token=splitter_token)


    def _processToken(self, splitter_token='end'):
        ret = []
        while bool(self.iter_tokens):
            token = next(self.iter_tokens)
            ret.append({})

            curr_status = self.get_token_status(token)
            while curr_status == TokenType.VARIABLE:
                variable_name = token
                equalsign = next(self.iter_tokens)
                assert equalsign == "=", equalsign

                result = self._processToken(splitter_token='end')
                ret[-1][variable_name] = result
                continue

    def _process_value(self):
        token = self.iter_tokens.peek()
        status = self.get_token_status(token)

        if status == TokenType.ATOM:
            assert next(self.iter_tokens) == token
            return token
        if status == TokenType.NUMERIC:
            return self._process_numeric()
        if token == "[":
            return self._process_square_bracket()
        if token == '"':
            return self._process_string_atom()

    def _process_numeric(self):
        ret = next(self.iter_tokens)

        dot = self._peek_index_or_default(0, default=None)
        floating_number = self._peek_index_or_default(1, default=None)
        if dot == "." and self.get_token_status(floating_number) == TokenType.NUMERIC:
            next(self.iter_tokens)
            next(self.iter_tokens)
            return float(ret + dot + floating_number)
        return int(ret)

    def _process_square_bracket(self):  # precondition: opening left square bracket has already popped out
        assert next(self.iter_tokens) == "[", "not started by ["
        items = []
        while self.iter_tokens.peek("") != "]":
            if self.iter_tokens.peek("") == ",":
                next(self.iter_tokens)
                continue
            items.append(self._process_value())
            next_char = self.iter_tokens.peek("")
            assert next_char in ",]", "not a comma and not an ending square bracket"
        assert next(self.iter_tokens) == "]", "not ended by ]"
        return items

    def _process_string_atom(self):
        self.iter_tokens.do_next_if_currently_whitespace(True)
        temp = next(self.iter_tokens)
        assert temp == '"', f"char is not a double quotes"  # opening quote

        self.iter_tokens.set_include_space(True)

        items = []
        while True:
            token = next(self.iter_tokens)
            if token == "\\":
                escaped = next(self.iter_tokens)  # harusnya ada jika bukan syntax error
                items.append(self._string_unescape(token + escaped))
                continue
            if token == '"':
                self.iter_tokens.set_include_space(False)
                return "".join(items)
            items.append(token)

    def _string_unescape(self, s: str):
        return s.encode('utf-8').decode('unicode-escape')

    def _peek_index_or_default(self, index, default=None):
        try:
            return self.iter_tokens[index]
        except IndexError:
            return default




