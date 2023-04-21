:- [hzztime].

:- dynamic available/2.
:- dynamic have_time/4.


% {%begin ignore%}

%available(1:12:10, 1:14:15).
%available(2:12:10, 2:14:15).
%available(3:12:10, 3:14:15).
%
%have_time(2006463162, 1, 1:10:10, 1:15:10).
%have_time(2006463162, 1, 3:13:45, 3:16:00).
%have_time(2006463162, 0, 3:13:30, 3:18:00).
%
%have_time(2006462664, 0, 1:14:30, 1:17:10).
%have_time(2006462664, 1, 3:13:45, 3:16:00).
%have_time(2006462664, 0, 3:13:30, 3:18:00).

% {%end ignore%}

% {{available_definitions}}
% {{have_time_definitions}}
% {{testing_definitions}}


all_npm(NPM) :- have_time(NPM, _, _, _).

time(Hari:Jam:Tanggal) :- available(Hari:Jam:Tanggal, _:_:_).
time(Hari:Jam:Tanggal) :- available(_:_:_, Hari:Jam:Tanggal).

time(Hari:Jam:Tanggal) :- have_time(_NPM, _Is_Preferred, Hari:Jam:Tanggal, _:_:_).
time(Hari:Jam:Tanggal) :- have_time(_NPM, _Is_Preferred, _:_:_, Hari:Jam:Tanggal).
time_all(List) :- findall(X, time(X), List).


bruteforce_timeranges(Duration, time_range(StartTime, EndTime)) :-
    time(StartTime),
    add_time(StartTime, Duration, EndTime).
bruteforce_timeranges(Duration, time_range(StartTime, EndTime)) :-
    time(EndTime),
    NegativeDuration is -Duration,
    add_time(EndTime, NegativeDuration, StartTime).




%find_jadwal :- time find_jadwal(TimePoint).
%
%find_jadwal(time_points, booked_slots, npm_not_yet_book).


%find_jadwal(_):-
%    all_npm(NPM)
%    .
%have_time_all(List) :- findall(
%    [NPM, Hari:Jam:Tanggal],
%    (have_time(NPM, _, Hari:Jam:Tanggal, _), time(Hari:Jam:Tanggal)), List).

