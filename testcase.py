import Prolog
from random import randint
from typing import Callable, Optional

prolog = Prolog.Prolog("inner_nuel.pl")


class Tree:
    def __init__(self, value, left, right):
        self.value = value
        self.left:  Optional[Tree]  = left
        self.right: Optional[Tree] = right

    def __repr__(self):
        left = 'void'
        if self.left is not None:
            left = repr(self.left)
        right = 'void'
        if self.right is not None:
            right = repr(self.right)
        return f"tree({self.value}, {left}, {right})"

    def is_leaf(self):
        return self.left is None and self.right is None

    def sum_leaf(self):
        ret = 0
        if self.left is not None:
            ret += self.left.sum_leaf()
        if self.right is not None:
            ret += self.right.sum_leaf()
        if self.is_leaf():
            ret += self.value
        return ret

    def sum_non_leaf(self):
        ret = 0
        if self.left is not None:
            ret += self.left.sum_non_leaf()
        if self.right is not None:
            ret += self.right.sum_non_leaf()
        if not self.is_leaf():
            ret += self.value
        return ret


def random_tree(depth, value_randomizer: Callable[[int], int], chance_each_child: Callable[[bool, int], bool]):
    is_right_child = False
    left = None
    if chance_each_child(is_right_child, depth):
        left = random_tree(depth+1,
                           value_randomizer=value_randomizer,
                           chance_each_child=chance_each_child)
    is_right_child = True
    right = None
    if chance_each_child(is_right_child, depth):
        right = random_tree(depth+1,
                            value_randomizer=value_randomizer,
                            chance_each_child=chance_each_child)
    value = value_randomizer(depth)
    return Tree(value, left, right)



def simple_random_tree():
    return random_tree(0,
                        lambda depth: randint(-3 * depth, 3 * depth),
                        lambda is_right_child, depth: randint(1, 100) <= 50-2*depth)

class InnerStrategy:
    def __init__(self):
        self.tree = simple_random_tree()

    def get_query(self):
        return f"inner({self.tree!r}, Sum)"

    def get_answer(self):
        return self.tree.sum_non_leaf()


class LeafStrategy:
    def __init__(self):
        self.tree = simple_random_tree()

    def get_query(self):
        return f"daun({self.tree!r}, Sum)"

    def get_answer(self):
        return self.tree.sum_leaf()

try:
    for i in range(1000):
        print(i)
        strategy = InnerStrategy()
        query = strategy.get_query()
        result = prolog.query(query)


        if len(result) != 1:
            print(query)
            print(result)
            raise RuntimeError("Multiple result")
        act = result[0]['Sum']
        exp = strategy.get_answer()
        if act != exp:
            print(query)
            print('exp:', exp)
            print('act:', act)
            raise RuntimeError("Multiple result")
except Prolog.PrologException as e:
    print(query)
    raise e