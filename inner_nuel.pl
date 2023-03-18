inner(tree(Labell, X, Y), SumNew) :- 
    X \== void, 
    inner(X, Sum1), 
    inner(Y, Sum2), 
    SumNew is (Labell + Sum1 + Sum2), !.
    
inner(tree(Labell, X, Y), SumNew) :- 
    Y \== void, 
    inner(X, Sum1), 
    inner(Y, Sum2), 
    SumNew is (Labell + Sum1 + Sum2), !.
    
inner(void, 0).
inner(tree(_, void, void), 0).