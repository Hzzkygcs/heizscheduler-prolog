min([X], X).
min([X| L], MinNew) :- min(L, MinPrev), (MinPrev < X, MinNew=MinPrev ; MinPrev >= X, MinNew=X), !.