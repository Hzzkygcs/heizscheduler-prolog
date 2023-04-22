from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from definitions.misc import define_tokenizer_regex
from definitions.paths import MAIN_PROLOG_FILE_IO


def extract_bool(result):
    if len(result) > 1:
        return True
    if len(result) == 0:
        return False
    return result[0] == 'true'


class TestCheckIfTheyHaveTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)
        define_tokenizer_regex(self.prolog)

    def test__should_be_true_if_both_are_exactly_the_same(self):
        pass
        # TODO

