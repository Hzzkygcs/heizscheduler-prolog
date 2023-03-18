inner(tree(Labell, X, Y), SumNew) :- 
    (X \== void; Y \== void), 
    inner(X, Sum1), 
    inner(Y, Sum2), 
    SumNew is (Labell + Sum1 + Sum2).

inner(tree(_, void, void), 0).

