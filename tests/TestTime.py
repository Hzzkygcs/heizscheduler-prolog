from unittest import TestCase

from HzzProlog.HzzProlog import HzzProlog
from tests.definitions import available, time, hari, MAIN_PROLOG_FILE_IO, X, Y, Z


class TestTime(TestCase):
    def setUp(self) -> None:
        self.prolog = HzzProlog(MAIN_PROLOG_FILE_IO)

    def test_time__should_return_list_of_endpoints_correctly(self):
        self.prolog.add_facts("available_definitions", [
            available(hari(0, 1, 2), hari(3, 4, 5)),
            available(hari(6, 10, 10), hari(6, 23, 59)),
        ])
        result = self.prolog.query(time(X, Y, Z))
        print(result)
