
def assert_prolog_output_the_same(self, expected: list, actual: list, ignore_duplicates=False):
    if len(expected) >= 2 and expected[-1] == "false":
        expected.pop()
    if len(actual) >= 2 and actual[-1] == "false":
        actual.pop()
    if ignore_duplicates:
        raise NotImplementedError()
    self.assertCountEqual(expected, actual)

