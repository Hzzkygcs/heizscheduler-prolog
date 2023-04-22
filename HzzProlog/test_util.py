from collections.abc import Iterable


def assert_prolog_output_the_same(self, expected: list, actual: list, ignore_duplicates=False, nested_ignore=False):
    if len(expected) >= 2 and expected[-1] == "false":
        expected.pop()
    if len(actual) >= 2 and actual[-1] == "false":
        actual.pop()
    if ignore_duplicates and nested_ignore:
        comparison = compare_list_just_like_a_set_nested(expected, actual)
        self.assertTrue(comparison)
        return
    if ignore_duplicates:
        comparison = compare_list_just_like_a_set(expected, actual)
        self.assertTrue(comparison)
        return
    self.assertCountEqual(expected, actual)


def compare_list_just_like_a_set_nested(list1, list2):
    def comparator(x, y):
        if not isinstance(x, Iterable) or not isinstance(y, Iterable):
            return x == y
        return compare_list_just_like_a_set_nested(x, y)
    return compare_list_just_like_a_set(list1, list2, comparator)


def compare_list_just_like_a_set(list1, list2, comparator=(lambda x, y: x == y)):
    for item1 in list1:
        for item2 in list2:
            if comparator(item1, item2):
                break
        else:
            return False
    for item2 in list2:
        for item1 in list1:
            if comparator(item1, item2):
                break
        else:
            return False
    return True



def remove_duplicates(lst, nested_remove_duplicates=False):
    if not isinstance(lst, list):
        return lst

    ret = []
    for curr_lst in lst:
        if isinstance(curr_lst, list) and nested_remove_duplicates:
            curr_lst = remove_duplicates(curr_lst, nested_remove_duplicates=nested_remove_duplicates)
        for added in ret:
            if added == curr_lst:
                break
        else:
            ret.append(curr_lst)
    return ret


def remove_trailing_false_or_true(value):
    value = value[:]
    if len(value) == 0:
        return value
    if value[-1] in ('true', 'false'):
        value.pop()
    return value