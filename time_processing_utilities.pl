% Format waktu: time_range(00:00:00, 06:23:59)
:- op(500, xfy, :).

% {{hzztime_defintitions}}


convert_from_minutes(Minutes, _) :-
    Minutes < 0,
    throw("Menit tidak boleh negatif").
convert_from_minutes(Minutes, Time) :-
    M is Minutes mod 60,
    Hours is Minutes // 60,
    H is Hours mod 24,
    D is Hours // 24,
    Time = D:H:M.

convert_to_minutes(D:H:M, Minutes) :-
    Minutes is D * 24 * 60 + H * 60 + M.

add_time(D1:H1:M1, D2:H2:M2, Result) :-
    convert_to_minutes(D1:H1:M1, InitialMinutes),
    convert_to_minutes(D2:H2:M2, AddMinutes),
    Sum is InitialMinutes + AddMinutes,
    convert_from_minutes(Sum, Result), !.

add_time(D:H:M, Minutes, Result) :-
    convert_to_minutes(D:H:M, InitialMinutes),
    Sum is InitialMinutes + Minutes,
    convert_from_minutes(Sum, Result).

subtract_time(D1:H1:M1, D2:H2:M2, Result) :-
    convert_to_minutes(D1:H1:M1, InitialMinutes),
    convert_to_minutes(D2:H2:M2, SubMinutes),
    Sum is InitialMinutes - SubMinutes,
    convert_from_minutes(Sum, Result), !.

subtract_time(D:H:M, Minutes, Result) :-
    convert_to_minutes(D:H:M, InitialMinutes),
    Sum is InitialMinutes - Minutes,
    convert_from_minutes(Sum, Result).

duration(time_range(D1:H1:M1, D2:H2:M2), Duration) :-  % In minutes. Not tested yet
    convert_to_minutes(D1:H1:M1, Minutes1),
    convert_to_minutes(D2:H2:M2, Minutes2),
    Duration is Minutes2 - Minutes1.

% green cut
less_than(D1:_:_,  D2:_:_) :- D1 < D2, !.
less_than(D1:H1:_, D2:H2:_) :- D1 == D2, H1 < H2, !.
less_than(D1:H1:M1, D2:H2:M2) :- D1 == D2, H1 == H2, M1 < M2.

less_than_or_equal(X:Y:Z, X:Y:Z).
less_than_or_equal(D1:H1:M1, D2:H2:M2) :- less_than(D1:H1:M1, D2:H2:M2).


time_conflict(time_range(Start1, End1), time_range(Start2, End2)) :-
    \+(
      (less_than_or_equal(End1, Start2) ;  less_than_or_equal(End2, Start1))
    ).



time_conflict_list(TimeRange1, [TimeRange2 | _ListOfTimeRange]) :- time_conflict(TimeRange1, TimeRange2).
time_conflict_list(TimeRange1, [_TimeRange2 | ListOfTimeRange]) :- time_conflict_list(TimeRange1, ListOfTimeRange).

% inside_another_timerange(SmallerTimeRange, BiggerTimeRange)
inside_another_timerange(time_range(ChildStart, ChildEnd), time_range(ParentStart, ParentEnd)) :-
    less_than_or_equal(ParentStart, ChildStart), less_than_or_equal(ChildEnd, ParentEnd).

unique_call(Template, Goal, Bag) :-
    findall(Template, Goal, Result),
    sort(Result, ResultDuplicationRemoved),
    member(Bag, ResultDuplicationRemoved).


list_of_timeranges_to_list_of_timepoints(ListOfTimerange, ResultingListOfTimepoint) :-
    findall(X, (
            member(time_range(Start, End), ListOfTimerange),
            (X=Start ; X=End)
        ), ResultingListOfTimepoint
    ).

