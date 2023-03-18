daun(void, 0).
daun(tree(Leaf, void, void), Leaf).
daun(tree(_, Left, Right), Sum) :-
    daun(Left, ResultLeft),
    daun(Right, ResultRight),
    Sum is ResultLeft + ResultRight,
    !.