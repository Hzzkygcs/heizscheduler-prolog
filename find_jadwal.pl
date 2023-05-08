:- [time_processing_utilities].
:- [soft_constraints].

:- dynamic available/1.
:- dynamic have_time/3.




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

find_jadwal_and_penalty(Duration, Result) :-
    find_jadwal(Duration, ListOfBookedSlots),
    get_overall_soft_constraint_penalty(ListOfBookedSlots, PenaltyScore),
    Result=penalty_and_slots(PenaltyScore, ListOfBookedSlots).

:- use_module(library(pairs)).
find_jadwal_and_score_sorted_list(Duration, ResultingList) :-
    findall(Key-Value, find_jadwal_and_penalty(Duration, penalty_and_slots(Key, Value)), UnsortedPairs),
    keysort(UnsortedPairs, SortedPairs),
    ItemResult=penalty_and_slots(PenaltyScore, ListOfBookedSlots),
    Goal=member(PenaltyScore-ListOfBookedSlots, SortedPairs),
    findall(ItemResult, Goal, ResultingList).


find_jadwal_and_score_sorted_member(Duration, ResultingMember) :-
    find_jadwal_and_score_sorted_list(Duration, List),
    member(ResultingMember, List).


get_best_jadwal(Duration, Penalty, BookedSlot) :-
    find_jadwal_and_score_sorted_list(
        Duration, [FirstElement | _]),
    FirstElement=penalty_and_slots(Penalty, Slots),
    member(BookedSlot, Slots).