:- [hzztime].

:- dynamic available/2.
:- dynamic have_time/3.


% {%begin ignore%}

%available(1:12:10, 1:14:15).
%available(2:12:10, 2:14:15).
%available(3:12:10, 3:14:15).
%
%have_time(2006463162, 1, time_range(1:10:10, 1:15:10)).
%have_time(2006463162, 1, time_range(3:13:45, 3:16:00)).
%have_time(2006463162, 0, time_range(3:13:30, 3:18:00)).
%
%have_time(2006462664, 0, time_range(1:14:30, 1:17:10)).
%have_time(2006462664, 1, time_range(3:13:45, 3:16:00)).
%have_time(2006462664, 0, time_range(3:13:30, 3:18:00)).

% {%end ignore%}

% {{available_definitions}}
% {{have_time_definitions}}
% {{testing_definitions}}


all_npm(NPM) :- have_time(NPM, _, _).

time(Hari:Jam:Tanggal) :- available(Hari:Jam:Tanggal, _:_:_).
time(Hari:Jam:Tanggal) :- available(_:_:_, Hari:Jam:Tanggal).

time(Hari:Jam:Tanggal) :- have_time(_NPM, _IsPreferred, time_range(Hari:Jam:Tanggal, _:_:_)).
time(Hari:Jam:Tanggal) :- have_time(_NPM, _IsPreferred, time_range(_:_:_, Hari:Jam:Tanggal)).
time_all(List) :- findall(X, time(X), List).


bruteforce_timeranges(Duration, time_range(StartTime, EndTime)) :-
    time(StartTime),
    add_time(StartTime, Duration, EndTime).
bruteforce_timeranges(Duration, time_range(StartTime, EndTime)) :-
    time(EndTime),
    NegativeDuration is -Duration,
    add_time(EndTime, NegativeDuration, StartTime).

% in: Npm, TimeRange. Out: IsPreferred
%check_if_they_have_time(Npm, TimeRange, IsPreferred) :- true.


list_of_timeranges_inside_booked_slot([], []).
list_of_timeranges_inside_booked_slot([booked_slot(_, _, Result) | ListOfBookedSlots], [Result | ListOfResultingTimeranges]) :-
    list_of_timeranges_inside_booked_slot(ListOfBookedSlots, ListOfResultingTimeranges).




%find_jadwal(Duration, Result) :-
%    find_all(X, all_npm(X), NpmList),
%    find_jadwal(Duration, NpmList, Result).
%
%find_jadwal(_, [], []).
%find_jadwal(Duration, [Npm | RemainingNpm], [NewBookedSlot | BookedSlots]) :-
%    bruteforce_timeranges(Duration, TimeRange),
%    list_of_timeranges_inside_booked_slot(BookedSlots, ListOfTimeRanges),
%    \+time_conflict_list(TimeRange, ListOfTimeRanges),
%    NewBookedSlot = booked_slot(),
%    find_jadwal(Duration, RemainingNpm, BookedSlots).


%find_jadwal(_):-
%    all_npm(NPM)
%    .
%have_time_all(List) :- findall(
%    [NPM, Hari:Jam:Tanggal],
%    (have_time(NPM, _, Hari:Jam:Tanggal, _), time(Hari:Jam:Tanggal)), List).

