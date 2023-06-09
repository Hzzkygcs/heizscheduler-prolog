import enum
import os
import re
import string
import sys
from typing import TypeVar, Callable

from more_itertools import peekable

from HzzProlog.PrologCallable import PrologCallable, Variable

sys.path.append(os.path.dirname(os.path.realpath(__file__)))
from HzzProlog.ChainEquality import ChainedEquality


class BinOp:  # operator that takes two operands (binary operator)
    def __init__(self, operator_symbol, left_operand, right_operand):
        self.operator_symbol = operator_symbol
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"BinOp({self.operator_symbol}, {self.left_operand!r}, {self.right_operand!r})"

    def __eq__(self, other):
        if not isinstance(other, BinOp):
            return False
        return (
            self.operator_symbol == other.operator_symbol
            and self.left_operand == other.left_operand
            and self.right_operand == other.right_operand
        )



class Default:
    pass


class PrologOutputProcessor:
    def __init__(self, string, splitter_token="BACKTRACK",
                 additional_regex: list[tuple[int,str,Callable]] = Default):
        if additional_regex == Default:
            additional_regex = []
        self.string = string
        self.iter_tokens = TokenizerIterator(string, tokenizer_regex=additional_regex)
        self.prev_value = None
        self.splitter_token = splitter_token

    def pop_until(self, to_be_found_inclusive):
        self.iter_tokens.pop_until(to_be_found_inclusive=to_be_found_inclusive)

    def process_token(self):
        if not self.iter_tokens:
            return []
        if self.iter_tokens[0] == self.splitter_token:
            next(self.iter_tokens)
        process_token_helper = PrologOutputHelper(self)
        return process_token_helper.process_token()

    def process_value(self):
        ret = self.__process_value()
        self.prev_value = ret
        return ret

    def check_if_next_is_operator(self):
        operators = "-+*/"
        return self.iter_tokens.peek() in  operators  # current list of operators

    def __process_value(self):
        token = self.iter_tokens.peek()
        status = TokenType.get_token_status(token)

        if self.__current_token_is_functor():
            return self.__process_functor()
        if status == TokenType.ATOM:
            assert next(self.iter_tokens) == token
            return token
        if status == TokenType.VARIABLE:
            assert next(self.iter_tokens) == token
            return Variable(token)
        if token == "-":
            return self._process_minus_sign()
        if status == TokenType.NUMERIC:
            return self._process_numeric()
        if token == "[":
            return self._process_square_bracket()
        if token == '"':
            return self._process_string_atom()

    def __current_token_is_functor(self):
        token = self.iter_tokens.peek()
        status = TokenType.get_token_status(token)
        if status != TokenType.ATOM:
            return False
        token = self._peek_index_or_default(1, None)
        return token == "("

    def __process_functor(self):
        functor_name = next(self.iter_tokens)
        args = self.__process_functor_argumens()
        resulting_functor = PrologCallable(functor_name)
        return resulting_functor(*args)

    def __process_functor_argumens(self):
        assert next(self.iter_tokens) == "("
        if self.iter_tokens.peek() == ")":
            assert next(self.iter_tokens) == ")"
            return []
        args = []
        while True:
            arg = self.__process_value()
            args.append(arg)
            if self.iter_tokens.peek() == ")":
                assert next(self.iter_tokens) == ")"
                break
            assert next(self.iter_tokens) == ","
        return args


    def _process_minus_sign(self):
        prev_token = self.iter_tokens.prev
        assert next(self.iter_tokens) == '-'

        right_operand = self._process_numeric()
        prev_token_is_not_none = prev_token is not None
        if prev_token_is_not_none and TokenType.get_token_status(prev_token) == TokenType.NUMERIC:
            return BinOp('-', self.prev_value, right_operand)
        return -right_operand

    def _process_numeric(self):
        ret = next(self.iter_tokens)

        dot = self._peek_index_or_default(0, default=None)
        floating_number = self._peek_index_or_default(1, default=None)
        if floating_number is None:
            return int(ret)
        is_floating = (dot == "." and TokenType.get_token_status(floating_number) == TokenType.NUMERIC)
        if is_floating:
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
            items.append(self.process_value())
            next_char = self.iter_tokens.peek("")
            assert next_char in ",]", "not a comma and not an ending square bracket"
        assert next(self.iter_tokens) == "]", "not ended by ]"
        return items

    def _process_string_atom(self):
        self.iter_tokens.do_next_if_currently_whitespace(True)
        temp = next(self.iter_tokens)
        assert temp == '"', "char is not a double quotes"  # opening quote

        items = []
        while True:
            token = self.iter_tokens.next(include_space=True)
            if token == "\\":
                escaped = self.iter_tokens.next(include_space=True)  # harusnya ada jika bukan syntax error
                items.append(self._string_unescape(token + escaped))
                continue
            if token == '"':
                return "".join(items)
            items.append(token)

    def _string_unescape(self, s: str):
        return s.encode('utf-8').decode('unicode-escape')

    def _peek_index_or_default(self, index, default=None):
        try:
            return self.iter_tokens.get_index(index)
        except IndexError:
            return default


class FlowControl(enum.Enum):
    BREAK = 1
    CONTINUE = 2


class PrologOutputHelper:
    def __init__(self, prolog_outp_proc: PrologOutputProcessor):
        self.iter_tokens = prolog_outp_proc.iter_tokens
        self.prolog_outp_proc = prolog_outp_proc
        self.splitter_token = prolog_outp_proc.splitter_token  # by default BACKTRACK
        self.ret = []

    def process_token(self):
        while bool(self.iter_tokens):  # parse  ... BACKTRACK ... BACKTRACK ..., etc
            peek = self.iter_tokens.peek()
            if peek == self.splitter_token:
                next(self.iter_tokens)
                continue
            if TokenType.get_token_status(peek) == TokenType.ATOM:
                self.ret.append(next(self.iter_tokens))  # false or true
                continue
            if peek == ".":
                next(self.iter_tokens)
                break
            self._process_multiple_variables_with_different_values()
        return self.ret

    def _process_multiple_variables_with_different_values(self):
        """
        parse  Var1 = value1, Var2 = value2, Var3 = value3, ...
        :return: {'Var1': value1, 'Var2': value2, 'Var3': value3, ...}
        """
        variable_value_mapping = {}
        self.ret.append(variable_value_mapping)

        while True:
            control = self._inner_loop_condition()
            if control == FlowControl.CONTINUE:
                continue
            elif control == FlowControl.BREAK:
                break

            variable_names, instantiation_status = self._process_multiple_equality_chains()
            value = self.prolog_outp_proc.process_value()

            # for example:  `BACKTRACK Val1 = Val2 false.`
            # trailing false which I can't use BACKTRACK to separate it :(
            false_is_not_part_of_equality = (
                    value == "false"
                    and (variable_names == []
                        or instantiation_status == ChainedEqualityInstantiationStatus.PURE_VARiABLE_UNIFICATION))
            if false_is_not_part_of_equality:
                self.ret.append(value)

            if len(variable_names) > 1:
                value = instantiation_status.create_chained_equality(variable_names, value)
            for variable_name in variable_names:
                self.__unify_with_existing_one_if_any(variable_value_mapping, variable_name, value)
            assert (variable_names, value) != ([], None)  # usually cause infinite loops

    def __unify_with_existing_one_if_any(self, variable_value_mapping, variable_name, new_value):
        variable_already_defined = variable_name in variable_value_mapping
        if variable_already_defined:
            prev_value = variable_value_mapping.get(variable_name, None)
            either_is_an_equality = isinstance(new_value, ChainedEquality) or isinstance(prev_value, ChainedEquality)
            if either_is_an_equality:
                ChainedEquality.unify(variable_value_mapping, new_value, prev_value)
                return
            assert prev_value == new_value
        variable_value_mapping[variable_name] = new_value

    def _inner_loop_condition(self):
        try:
            inner_peek = self.iter_tokens.peek()
        except StopIteration:
            print("GATAU INI HARUSNYA BREAK ATAU ENGGA (PrologOutputProcessor:212)")
            return FlowControl.BREAK

        if inner_peek == self.splitter_token:
            return FlowControl.BREAK

        if inner_peek == ",":
            next(self.iter_tokens)
            return FlowControl.CONTINUE

        if inner_peek == ".":
            next(self.iter_tokens)
            return FlowControl.BREAK

    def _process_multiple_equality_chains(self):
        """
        Parse:
            `Var1 = Var2 = Var3 = ... = VarN`
            or
            `Var1 = Var2 = Var3 = ... = VarN = value`
        :return: ["Var1", "Var2", "Var3", ... "VarN"], ChainedEqualityInstantiationStatus
        """
        variable_names = []
        while True:
            token = self.iter_tokens.peek()
            curr_status = TokenType.get_token_status(token)
            if curr_status != TokenType.VARIABLE:
                return variable_names, ChainedEqualityInstantiationStatus.INSTANTIATED
            assert next(self.iter_tokens) == token
            variable_names.append(token)

            next_token = self.iter_tokens.peek()
            if next_token != "=":
                return variable_names, ChainedEqualityInstantiationStatus.PURE_VARiABLE_UNIFICATION
            next(self.iter_tokens)


class ChainedEqualityInstantiationStatus(enum.Enum):
    PURE_VARiABLE_UNIFICATION = 1  # for exampel:  Val1 = Val2
    INSTANTIATED = 2  # for example"  Val1 = Val2 = some_instantiation_value

    def create_chained_equality(self, variable_names: list[str], value):
        if self == self.PURE_VARiABLE_UNIFICATION:
            return ChainedEquality(variable_names)
        assert self == self.INSTANTIATED
        return ChainedEquality(variable_names, value)




class TokenType(enum.Enum):
    VARIABLE = 1
    ATOM = 2
    NUMERIC = 3
    OPERATOR = 4
    FUNCTOR = 5

    @classmethod
    def get_token_status(cls, token: str):
        assert len(token) >= 1
        if set(token) <= set(string.digits):
            return cls.NUMERIC
        if len(set(string.ascii_letters + string.digits) & set(token)) >= 1:
            if token[0].isupper():
                return cls.VARIABLE
            return cls.ATOM
        return cls.OPERATOR


class CharType(enum.Enum):
    UNDERSCORE_ALPHA_NUMERIC = 1
    WHITESPACE = 2
    SYMBOLS = 3

    @classmethod
    def get_type(cls, char: str):
        if char.isalnum() or char == "_":
            return cls.UNDERSCORE_ALPHA_NUMERIC
        if char.isspace():
            return cls.WHITESPACE
        return cls.SYMBOLS


_T = TypeVar('_T')


class NotAvailable:
    pass




class TokenizerIterator(peekable):
    def __init__(self, string, tokenizer_regex: list[tuple[int, str, Callable]] = Default):
        if tokenizer_regex == Default:
            tokenizer_regex = []
        self.prev = None
        tokenizer = Tokenizer(string)
        for priority, pattern, mapping in tokenizer_regex:
            tokenizer.add_new_regex(priority, pattern, mapping)
        tokenized = tokenizer.tokenize
        super().__init__(tokenized)

    def peek(self, default=NotAvailable()):
        if isinstance(default, NotAvailable):
            return self.__peek()  # may rise StopIteration

        try:
            return self.__peek()
        except StopIteration:
            return default

    def pop_until(self, to_be_found_inclusive):
        curr = self.__next__()
        while curr != to_be_found_inclusive:
            curr = self.__next__()

    def __peek(self):
        try:
            i = self.__get_nth_index_that_conform_includeSpace_property(0)  # peek == get index 0
            return super().__getitem__(i)
        except IndexError:
            raise StopIteration

    def __get_nth_index_that_conform_includeSpace_property(self, index, include_space=None):
        if include_space is None:
            include_space = False
        if include_space:
            return index
        i = 0
        while True:
            while super().__getitem__(i).isspace():
                i += 1
            if index == 0:
                return i
            index -= 1
            i += 1

    def do_next_if_currently_whitespace(self, multiple_times):
        while super().peek().isspace():
            super().__next__()
            if not multiple_times:
                return

    def next(self, include_space=None):
        # if there's no ignored-token, then it's guaranteed that giving 0 index returns 0 index
        number_of_ignored_tokens = self.__get_nth_index_that_conform_includeSpace_property(0, include_space=include_space)
        for _ in range(number_of_ignored_tokens):
            super().__next__()

        self.prev = super().__next__()
        return self.prev


    def __next__(self):
        return self.next()

    def as_list(self):
        ret = []
        i = 0
        try:
            while True:
                ret.append(super().__getitem__(i))
                i += 1
        except IndexError:
            pass
        return ret

    def __repr__(self):
        return f"Peekable({self.as_list()})"

    def get_index(self, index, include_space=None):
        index = self.__get_nth_index_that_conform_includeSpace_property(index, include_space=include_space)
        return super().__getitem__(index)

    def get_index_or_default(self, index, default, include_space=None):
        try:
            return self.get_index(index, include_space=include_space)
        except IndexError:
            return default


do_nothing = lambda x: x

class Tokenizer:
    def __init__(self, string):
        self.string = string
        self.curr_token = ""  # currently being-built token
        self.prev_char_type = CharType.UNDERSCORE_ALPHA_NUMERIC
        self.regex_tokenizer = [  # (priority_rank, regex_pattern). From lower to higher
            (200, "[a-zA-Z0-9_]+", do_nothing),
            (400, "\\s+", do_nothing),
            (600, ".", do_nothing),
        ]

    def add_new_regex(self, priority_rank, regex, mapping=do_nothing):
        self.regex_tokenizer.append((priority_rank, regex, mapping))
        self.regex_tokenizer.sort(key=lambda x: x[0])

    @property
    def tokenize(self):
        ret = []
        while len(self.string) > 0:
            final_mapping = lambda x: x
            for _, pattern, mapping in self.regex_tokenizer:
                match_result = re.match(pattern, self.string)
                if match_result is not None:
                    final_mapping = mapping
                    break

            matched_str = self.string[0]
            if match_result is not None:
                matched_str = match_result[0]

            matched_str_length = len(matched_str)
            assert len(matched_str) == match_result.span()[1]
            self.string = self.string[matched_str_length:]
            ret.append(final_mapping(matched_str))
        return ret

