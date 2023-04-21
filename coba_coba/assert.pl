maketable :-
    L=[0,1,2,3,4,5,6],
    member(X, L),
    member(Y, L),
    A is X*Y mod 7,
    assert(productMOD7(X,Y,A)),
    fail.
