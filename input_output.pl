:- dynamic scheduleAsdos/6, scheduleMahasiswa/8.

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
    write('hari start: '),read(HariStart),nl,
    write('jam start: '),read(JamStart),nl,
    write('menit start: '),read(MenitStart),nl,
    write('hari end: '),read(HariEnd),nl,
    write('jam end: '),read(JamEnd),nl,
    write('menit end: '),read(MenitEnd),nl,
    addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(2) :-
    write('npm: '),read(NPM),nl,
    write('is_preferred: '),read(IsPreferred),nl,
    write('hari start: '),read(HariStart),nl,
    write('jam start: '),read(JamStart),nl,
    write('menit start: '),read(MenitStart),nl,
    write('hari end: '),read(HariEnd),nl,
    write('jam end: '),read(JamEnd),nl,
    write('menit end: '),read(MenitEnd),nl,
    addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd).

switch(3) :-
    write('Schedule Asdos:'), nl,
    findall(scheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd),
        scheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd), AsdosSchedules),
    write(AsdosSchedules),nl,

    write('Schedule Mahasiswa:'), nl,
    findall(scheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd),
        scheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd), MahasiswaSchedules),
    write(MahasiswaSchedules).

switch(4) :-
    write('Invalid input. Please enter 1, 2, or 3.'), nl.

addScheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(scheduleAsdos(HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd)).

addScheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd) :-
    assertz(scheduleMahasiswa(NPM, IsPreferred, HariStart, JamStart, MenitStart, HariEnd, JamEnd, MenitEnd)).