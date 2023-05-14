:- dynamic available/1, have_time/3, set_duration/1.

main_menu :-
    write('Set Duration (positive int): '), read_int(1, SetDuration), nl,
    assertz(set_duration(SetDuration)),
    print_menu.

print_menu :-
    write('======= HzzScheduler ======='),nl,
    write('1. Memasukkan schedule asdos'),nl,
    write('2. Memasukkan schedule mahasiswa'),nl,
    write('3. Jalankan program'),nl,
    write('4. Berhenti'),nl,
    write('========================'),nl,
    write('input anda:  '),read(Command), write(Command), nl,
    io_loop_condition(Command).

io_loop_condition(end_of_file) :- !.
io_loop_condition(4) :- !.
io_loop_condition(3) :- switch(3), !.
io_loop_condition(1) :- nl, switch(1), nl, print_menu, !.
io_loop_condition(2) :- nl, switch(2), nl, print_menu, !.

io_loop_condition(_) :- nl, print_menu.


switch(1) :-
    write('hari start  (non-negative int): '),read_int(0, HariStart),
    write('jam start   (non-negative int): '),read_int(0, JamStart),
    write('menit start (non-negative int): '),read_int(0, MenitStart),
    write('hari end    (non-negative int): '),read_int(0, HariEnd),
    write('jam end     (non-negative int): '),read_int(0, JamEnd),
    write('menit end   (non-negative int): '),read_int(0, MenitEnd),
    addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(2) :-
    write('npm or any identifier         : '),read(NPM),
    write('is_preferred          (0 or 1): '),read_zero_one(IsPreferred),
    write('hari start  (non-negative int): '),read_int(0, HariStart),
    write('jam start   (non-negative int): '),read_int(0, JamStart),
    write('menit start (non-negative int): '),read_int(0, MenitStart),
    write('hari end    (non-negative int): '),read_int(0, HariEnd),
    write('jam end     (non-negative int): '),read_int(0, JamEnd),
    write('menit end   (non-negative int): '),read_int(0, MenitEnd),
    addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(3) :-
    write('Schedule Asdos:'), nl,
    findall(available(time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd)),
        available(time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd)), AsdosSchedules),
    write(AsdosSchedules), nl,
    nl,
    write('Schedule Mahasiswa:'), nl,
    findall(have_time(NPM, IsPreferred, time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd)),
        have_time(NPM, IsPreferred, time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd)), MahasiswaSchedules),
    write(MahasiswaSchedules), nl,
    nl,
    write('Schedule Result: '), nl,
    set_duration(SetDuration),
    find_jadwal_and_score_sorted_member(SetDuration, X),
    print_penalty_and_slots(X), write("Press enter to find other possibilities."), nl,
    get_single_char_until_newline,
    nl,
    fail.


addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(available(time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd))).

addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(have_time(NPM, IsPreferred, time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd))).

get_single_char_until_newline :-
    get_single_char_until_newline(0).

get_single_char_until_newline(-1):- !.  % END OF FILE
get_single_char_until_newline(13):- !.  % \n

get_single_char_until_newline(_) :-
    get_single_char(X),
    get_single_char_until_newline(X).

print_penalty_and_slots(penalty_and_slots(Penalty,Slots)) :-
    write('Penalty: '), writeln(Penalty),
    write('Slots: '), writeln(Slots).


read_zero_one(N) :-
    repeat,
    read(X),
    read_zero_one(X, N).
read_zero_one(X, N) :- integer(X), X >= 0, X =< 1, !, N=X.
read_zero_one(end_of_file, _) :- !.
read_zero_one(X, _) :- write('Invalid input. Please try again. Given: '), writeln(X),  fail.




% red cut
read_int(Minimum, N) :-
    repeat,
    read(X),
    read_int(Minimum, X, N).
read_int(Minimum, X, N) :- integer(X), X >= Minimum, !, N=X.
read_int(_, end_of_file, _) :- !.
read_int(_, X, _) :- write('Invalid input. Please try again. Given: '), writeln(X),  fail.
