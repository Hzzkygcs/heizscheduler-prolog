from unittest import TestCase

from HzzProlog.ChainEquality import ChainedEquality
from HzzProlog.HzzProlog import HzzProlog
from tests.definitions import MAIN_PROLOG_FILE_IO, time_point, time_in_range


class TestTimeInRange(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)

    def test__should_return_false_if(self):
        _ = self.prolog.query(time_in_range(
            time_point(1, 10),
            time_point(2, 20)
        ))
        # TODO

