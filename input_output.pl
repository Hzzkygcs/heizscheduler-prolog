:- dynamic available/1, have_time/3, set_duration/1.

main_menu :-
    write('Set Duration (positive int): '), read(SetDuration), nl,
    assertz(set_duration(SetDuration)),
    print_menu.

print_menu :-
    repeat,
    write('======= HzzScheduler ======='),nl,
    write('1. Memasukkan schedule asdos'),nl,
    write('2. Memasukkan schedule mahasiswa'),nl,
    write('3. Jalankan program'),nl,
    write('========================'),nl,
    write('input anda:  '),read(Command),nl,
    switch(Command),
    (Command == 3 -> ! ; fail).


switch(1) :-
    write('hari start  (non-negative int): '),read(HariStart),nl,
    write('jam start   (non-negative int): '),read(JamStart),nl,
    write('menit start (non-negative int): '),read(MenitStart),nl,
    write('hari end    (non-negative int): '),read(HariEnd),nl,
    write('jam end     (non-negative int): '),read(JamEnd),nl,
    write('menit end   (non-negative int): '),read(MenitEnd),nl,
    addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(2) :-
    write('npm or identifier             : '),read(NPM),nl,
    write('is_preferred          (0 or 1): '),read(IsPreferred),nl,
    write('hari start  (non-negative int): '),read(HariStart),nl,
    write('jam start   (non-negative int): '),read(JamStart),nl,
    write('menit start (non-negative int): '),read(MenitStart),nl,
    write('hari end    (non-negative int): '),read(HariEnd),nl,
    write('jam end     (non-negative int): '),read(JamEnd),nl,
    write('menit end   (non-negative int): '),read(MenitEnd),nl,
    addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(3) :-
    get_single_char_until_newline,
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
    write('Schedule Result: (Press enter to other possibilities)'), nl,
    set_duration(SetDuration),
    find_jadwal_and_score_sorted_member(SetDuration, X),
    print_penalty_and_slots(X), nl,
    get_single_char_until_newline,
    nl,
    fail.

switch(4) :-
    write('Invalid input. Please enter 1, 2, or 3.'), nl,
    nl.

addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(available(time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd))).

addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(have_time(NPM, IsPreferred, time_range(HariStart:JamStart:MenitStart, HariEnd:JamEnd:MenitEnd))).

get_single_char_until_newline :-
    get_single_char_until_newline(0).
get_single_char_until_newline(13):- !.
get_single_char_until_newline(_) :-
    get_single_char(X),
    get_single_char_until_newline(X).

print_penalty_and_slots(penalty_and_slots(Penalty,Slots)) :-
    write('Penalty: '), write(Penalty), nl,
    write('Slots: '), write(Slots),nl.
