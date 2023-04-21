:- op(500, yfx, &).
:- op(300, yf, ^).
:- op(400, fx, #).

eval(X^, Y) :- eval(X, Xn), Y is Xn*Xn, !.
eval(#X, Y) :- eval(X, Xn), Y is Xn+1, !.
eval(A&B, Y) :- eval(A, An), eval(B, Bn), Y is An+Bn, !.
eval(X, X).



cermin(void, void).
cermin(tree(L, TL, TR), T2) :-
    cermin(TL, TRnew),
    cermin(TR, TLnew),
    T2=tree(L, TLnew, TRnew).

% tree(2, tree(1, void, void), tree(4, tree(3, void, void), tree(5, void, void)))
