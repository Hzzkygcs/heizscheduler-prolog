from unittest import TestCase

from HzzProlog.PrologOutputProcessor import TokenizerIterator


class TestTokenizerIterator(TestCase):
    def _instantiate(self, parameter):
        return TokenizerIterator(parameter)

    def test_tokenize__should_skip_whitespaces_if_told_so(self):
        result = self._instantiate("123 456  789   abc")
        include_space = False
        self.assertEqual("123", result.next(include_space=include_space))
        self.assertEqual("456", result.next(include_space=include_space))
        self.assertEqual("789", result.next(include_space=include_space))
        self.assertEqual("abc", result.next(include_space=include_space))

    def test_tokenize__should_include_whitespaces_if_told_so(self):
        result = self._instantiate("123 456  789   abc")
        def do_next():
            return result.next(include_space=True)

        self.assertEqual("123", do_next())
        self.assertEqual(" ", do_next())
        self.assertEqual("456", do_next())
        self.assertEqual(" "*2, do_next())
        self.assertEqual("789", do_next())
        self.assertEqual(" "*3, do_next())
        self.assertEqual("abc", do_next())

    def test_tokenize__should_include_whitespaces_if_told_so_even_if_in_the_middle_of_iterating(self):
        result = self._instantiate("12 34 56 78 9a bc de fg hi")
        include_space = False
        def do_next():
            return result.next(include_space=include_space)

        include_space = False
        self.assertEqual("12", do_next())
        self.assertEqual("34", do_next())
        self.assertEqual("56", do_next())

        include_space = True
        self.assertEqual(" ",  do_next())
        self.assertEqual("78", do_next())
        self.assertEqual(" ",  do_next())
        self.assertEqual("9a", do_next())

        include_space = False
        self.assertEqual("bc", do_next())
        self.assertEqual("de", do_next())
        self.assertEqual("fg", do_next())

        include_space = True
        self.assertEqual(" ",  do_next())
        self.assertEqual("hi", do_next())