:- [time_processing_utilities].
:- [soft_constraints].

:- dynamic available/1.
:- dynamic have_time/3.


% {%begin ignore%}

% available asdos
available(time_range(1:12:10, 1:14:15)).
available(time_range(2:12:10, 2:14:15)).
available(time_range(3:12:10, 3:14:15)).

% anak asdos booking
have_time(2006463162, 1, time_range(1:10:10, 1:15:10)).
have_time(2006463162, 1, time_range(3:13:45, 3:16:00)).
have_time(2006463162, 0, time_range(3:13:30, 3:18:00)).

have_time(2006462664, 0, time_range(1:14:30, 1:17:10)).
have_time(2006462664, 1, time_range(3:13:45, 3:16:00)).
have_time(2006462664, 0, time_range(3:13:30, 3:18:00)).

% {%end ignore%}

% {{available_definitions}}
% {{have_time_definitions}}
% {{testing_definitions}}


all_npm(NPM) :- have_time(NPM, _, _).

time(Hari:Jam:Tanggal) :- available(time_range(Hari:Jam:Tanggal, _:_:_)).
time(Hari:Jam:Tanggal) :- available(time_range(_:_:_, Hari:Jam:Tanggal)).

time(Hari:Jam:Tanggal) :- have_time(_NPM, _IsPreferred, time_range(Hari:Jam:Tanggal, _:_:_)).
time(Hari:Jam:Tanggal) :- have_time(_NPM, _IsPreferred, time_range(_:_:_, Hari:Jam:Tanggal)).
time_all(List) :- findall(X, time(X), List).


bruteforce_timerange(Duration, BaseTimePoint, time_range(BaseTimePoint, EndTime)) :-
    add_time(BaseTimePoint, Duration, EndTime).
bruteforce_timerange(Duration, BaseTimePoint, time_range(StartTime, BaseTimePoint)) :-
    NegativeDuration is -Duration,
    add_time(BaseTimePoint, NegativeDuration, StartTime).

% in: Npm, TimeRange. Out: IsPreferred
check_if_they_have_time(Npm, TimeRange, IsPreferred) :-
    have_time(Npm, IsPreferred, TheirAvailabilityTimeRange),
    inside_another_timerange(TimeRange, TheirAvailabilityTimeRange).


list_of_timeranges_inside_booked_slot([], []).
list_of_timeranges_inside_booked_slot(
        [booked_slot(_, _, Result) | ListOfBookedSlots],
        [Result | ListOfResultingTimeranges]
    ) :-
    list_of_timeranges_inside_booked_slot(ListOfBookedSlots, ListOfResultingTimeranges).



find_jadwal(Duration, Result) :-
    setof(X, all_npm(X), NpmList),
    time_all(ListOfRawTimePointBruteforces),
    unique_call(
        TemporaryResult, find_jadwal(Duration, NpmList, [], ListOfRawTimePointBruteforces, TemporaryResult), Result
    ).


find_jadwal(_, [], FinalBookedSlots, _, FinalBookedSlots).
find_jadwal(Duration, [Npm | RemainingNpm], CurrentBookedSlots, ListOfRawTimePointBruteforces, FinalBookedSlots) :-
    list_of_timeranges_inside_booked_slot(CurrentBookedSlots, ListOfTimeRangesInsideBookedSlot),
    list_of_timeranges_to_list_of_timepoints(ListOfTimeRangesInsideBookedSlot, ListOfTimePointsInsideBookedSlot),
    
    append(ListOfTimePointsInsideBookedSlot, ListOfRawTimePointBruteforces, TimeRangeBruteforces),
    member(BaseTimeRange, TimeRangeBruteforces),
    bruteforce_timerange(Duration, BaseTimeRange, TimeRange),  % TimeRange == current bruteforced TimeRange

    \+time_conflict_list(TimeRange, ListOfTimeRangesInsideBookedSlot),

    available(AsdosAvailability),
    inside_another_timerange(TimeRange, AsdosAvailability),
    check_if_they_have_time(Npm, TimeRange, IsPreferred),

    NewBookedSlot = booked_slot(Npm, IsPreferred, TimeRange),
    find_jadwal(Duration, RemainingNpm, [NewBookedSlot | CurrentBookedSlots], ListOfRawTimePointBruteforces, FinalBookedSlots).