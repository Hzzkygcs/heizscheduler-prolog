:- op(500, xfx, :).
:- op(600, xfx, -).

% Time range format: HH:MM-HH:MM
% 24 hour format

duration(H1:M1-H2:M2, Duration) :- % in minutes
    (H2 > H1 ; (H2 =:= H1, M2 >= M1)),
    Duration is ((H2*60 + M2) - (H1*60 + M1)).

duration(H1:M1-H2:M2, Duration) :- % in minutes
    (H2 < H1 ; (H2 =:= H1, M2 < M1)),
    Duration is ((H2*60 + M2) - (H1*60 + M1)) + 24*60.

valid_time_range(H1:M1-H2:M2) :-
    0 =< H1, H1 =< 24,
    0 =< H2, H2 =< 24,
    0 =< M1, M1 =< 60,
    0 =< M2, M2 =< 60,
    duration(H1:M1-H2:M2, Duration),
    Duration > 0.

time_in_range(H:M, _:_-H2:M2) :- % belom bener buat lebih dari 24 jam
    H =:= H2, M =:= M2, !, fail. % fail belom diajarin, materi next ppt

time_in_range(H:_, H1:_-H2:_) :-
    H1 =< H, H < H2, !.

time_in_range(H:M, H1:M1-H2:M2) :-
    H1 =< H, H =< H2,
    M1 =< M, M =< M2.

is_conflict(LH1:LM1-LH2:LM2, RH1:RM1-RH2:RM2) :-
    duration(LH1:LM1-LH2:LM2, DurationL),
    duration(RH1:RM1-RH2:RM2, DurationR),
    DurationL > DurationR,
    is_conflict(RH1:RM1-RH2:RM2, LH1:LM1-LH2:LM2),!.

is_conflict(LH1:LM1-LH2:LM2, RH1:RM1-RH2:RM2) :- % True if left and right time overlap
    duration(LH1:LM1-LH2:LM2, DurationL),
    duration(RH1:RM1-RH2:RM2, DurationR),
    DurationL =< DurationR,
    (time_in_range(LH1:LM1, RH1:RM1-RH2:RM2);
    time_in_range(LH2:LM2, RH1:RM1-RH2:RM2)).

% is_conflict(01:10-02:31, 02:30-03:40). True
% is_conflict(01:10-02:30, 02:30-03:40). False
% is_conflict(01:10-02:30, 02:25-03:40). True

% is_conflict(02:10-01:30, 02:25-03:40). True
