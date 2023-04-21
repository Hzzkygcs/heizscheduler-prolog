% Format waktu: time_range(00:00:00, 06:23:59)
:- op(500, xfy, :).

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

duration(time_range(D1:H1:M1, D2:H2:M2), Duration) :-  % In minutes
    convert_to_minutes(D1:H1:M1, Minutes1),
    convert_to_minutes(D2:H2:M2, Minutes2),
    Duration is Minutes1 + Minutes2.

time_conflict(time_range(LD1:LH1:LM1, RD1:RH1:RM1), time_range(LD2:LH2:LM2, RD2:RH2:RM2)) :-
    duration(time_range(LD1:LH1:LM1, RD1:RH1:RM1), DurationL),
    duration(time_range(LD2:LH2:LM2, RD2:RH2:RM2), DurationR),
    DurationL < DurationR, !,
    time_conflict(time_range(LD2:LH2:LM2, RD2:RH2:RM2), time_range(LD1:LH1:LM1, RD1:RH1:RM1)).

time_conflict(time_range(LD1:LH1:LM1, RD1:RH1:RM1), time_range(LD2:LH2:LM2, _:_:_)) :- % True means conflict
    convert_to_minutes(LD1:LH1:LM1, LHS),
    convert_to_minutes(RD1:RH1:RM1, RHS),
    convert_to_minutes(LD2:LH2:LM2, LHSConflict),
    LHS =< LHSConflict, LHSConflict < RHS.

%  time_conflict(time_range(00:02:30,00:03:30),time_range(00:02:00,00:05:30)). true
%  time_conflict(time_range(00:02:30,00:03:30),time_range(00:04:00,00:05:30)). false
%  time_conflict(time_range(00:02:30,01:01:30),time_range(00:04:00,00:05:30)). true

