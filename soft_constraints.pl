% Format waktu: time_range(00:00:00, 06:23:59)
:- op(500, xfy, :).
:- [time_processing_utilities].


count_preferred(ListOfBookedSlot, NumberOfPreferred) :-
    count_preferred(ListOfBookedSlot, 0, NumberOfPreferred).

count_preferred([], X, X).
count_preferred([booked_slot(_, IsPreferred, _) | ListOfBookedSlot], CurrentTotalNumberOfPreferred, FinalNumberOfPreferred) :-
    NewNumberOfPreferred is IsPreferred + CurrentTotalNumberOfPreferred,
    count_preferred(ListOfBookedSlot, NewNumberOfPreferred, FinalNumberOfPreferred).

extract_minutes_from_booked_slot_starting_time(
    booked_slot(_, _, time_range(Start, _)), Minutes
) :- convert_to_minutes(Start, Minutes).


:- use_module(library(pairs)).
sort_booked_slots_by_starting_time(ListOfBookedSlot, SortedListOfBookedSlot) :-
    map_list_to_pairs(extract_minutes_from_booked_slot_starting_time,
                      ListOfBookedSlot,
                      PairsOfStartingTimeAndBookedSlot),
    keysort(PairsOfStartingTimeAndBookedSlot, Sorted),
    pairs_values(Sorted, SortedListOfBookedSlot).

get_booking_slot_distance(
    booked_slot(_, _, time_range(_, FirstTaskEnd)),
    booked_slot(_, _, time_range(SecondTaskStart, _)),
    Duration
) :-
    duration(time_range(FirstTaskEnd, SecondTaskStart), Duration).

get_multiple_booking_slot_distance(BookingSlots, ListOfDistances) :-
    sort_booked_slots_by_starting_time(BookingSlots, SortedBookingSlots),
    Goal=helper_get_multiple_booking_slot_distance(SortedBookingSlots, X),
    findall(X, Goal, ListOfDistances).
helper_get_multiple_booking_slot_distance([First,Second|_], Distance) :-
    get_booking_slot_distance(First, Second, Distance).
helper_get_multiple_booking_slot_distance([_|BookingSlots], Distance) :-
    helper_get_multiple_booking_slot_distance(BookingSlots, Distance).


get_booking_slot_distance_total_penalty(BookingSlots, Penalty) :-
    get_booking_slot_distance_penalties(BookingSlots, Penalties),
    list_sum(Penalties, Penalty).

list_sum([], 0).  % not directly tested
list_sum([X|List], Result) :-
    list_sum(List, PrevSum),
    Result is X + PrevSum.


get_booking_slot_distance_penalties(BookingSlots, Penalties) :-
    get_multiple_booking_slot_distance(BookingSlots, ListOfDistances),
    Goal=(member(Distance, ListOfDistances), get_booked_slot_penalty(Distance, Penalty)),
    findall(Penalty, Goal, Penalties).


% red cut (yg paling bawah mempengaruhi).
% Batasan-batasan ditakar pakai intuisi saja
get_booked_slot_penalty(MinuteRange, 4) :- 10 =< MinuteRange, MinuteRange < 25, !.
get_booked_slot_penalty(MinuteRange, 3) :- 25 =< MinuteRange, MinuteRange < 45, !.
get_booked_slot_penalty(MinuteRange, 2) :- 45 =< MinuteRange, MinuteRange < 60, !.
get_booked_slot_penalty(MinuteRange, 1) :- 60 =< MinuteRange, MinuteRange < 120, !.
get_booked_slot_penalty(_MinuteRange, 0).


get_overall_soft_constraint_penalty(BookingSlots, Penalty) :-
    length(BookingSlots, Length),
    get_booking_slot_distance_total_penalty(BookingSlots, DistancePenalty),
    count_preferred(BookingSlots, NumberOfPreferred),
    UnmetPreferencePenalty is Length - NumberOfPreferred,  % swap
    Penalty is UnmetPreferencePenalty + DistancePenalty*3/4.  % weight ditakar pakai intuisi saja
