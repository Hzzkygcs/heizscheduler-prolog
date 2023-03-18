from unittest import TestCase

from PrologOutputProcessor import HelperTokenizerIterator, TokenizerIterator


class TestTokenizerIterator(TestCase):
    def _instantiate(self, parameter):
        return TokenizerIterator(parameter)

    def test_tokenize__should_skip_whitespaces_if_told_so(self):
        result = self._instantiate("123 456  789   abc")
        result.set_include_space(False)
        self.assertEqual("123", next(result))
        self.assertEqual("456", next(result))
        self.assertEqual("789", next(result))
        self.assertEqual("abc", next(result))

    def test_tokenize__should_include_whitespaces_if_told_so(self):
        result = self._instantiate("123 456  789   abc")
        result.set_include_space(True)
        self.assertEqual("123", next(result))
        self.assertEqual(" ", next(result))
        self.assertEqual("456", next(result))
        self.assertEqual(" "*2, next(result))
        self.assertEqual("789", next(result))
        self.assertEqual(" "*3, next(result))
        self.assertEqual("abc", next(result))

    def test_tokenize__should_include_whitespaces_if_told_so_even_if_in_the_middle_of_iterating(self):
        result = self._instantiate("12 34 56 78 9a bc de fg hi")
        result.set_include_space(False)
        self.assertEqual("12", next(result))
        self.assertEqual("34", next(result))
        self.assertEqual("56", next(result))

        result.set_include_space(True)
        self.assertEqual(" ", next(result))
        self.assertEqual("78", next(result))
        self.assertEqual(" ", next(result))
        self.assertEqual("9a", next(result))

        result.set_include_space(False)
        self.assertEqual("bc", next(result))
        self.assertEqual("de", next(result))
        self.assertEqual("fg", next(result))

        result.set_include_space(True)
        self.assertEqual(" ", next(result))
        self.assertEqual("hi", next(result))