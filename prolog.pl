:- [hzztime].

:- dynamic available/2.
:- dynamic have_time/2.



% {%begin ignore%}

available(1:12:10, 1:14:15).
available(2:12:10, 2:14:15).
available(3:12:10, 3:14:15).

% have_time(npm, is_preferred, start, end)
have_time(2006463162, 1, 1:10:10, 1:15:10)
have_time(2006463162, 1, 3:13:45, 3:16:00)
have_time(2006463162, 0, 3:13:30, 3:18:00)

have_time(2006462664, 0, 1:14:30, 1:17:10)
have_time(2006462664, 1, 3:13:45, 3:16:00)
have_time(2006462664, 0, 3:13:30, 3:18:00)

% {%end ignore%}
% {{available_definitions}}
% {{have_time_definitions}}


all_npm(NPM) :- have_time(NPM, _, _, _).

time(Hari:Jam:Tanggal) :- available(Hari:Jam:Tanggal, _:_:_).
time(Hari:Jam:Tanggal) :- available(_:_:_, Hari:Jam:Tanggal).

time(Hari:Jam:Tanggal) :- have_time(_NPM, _Is_Preferred, Hari:Jam:Tanggal, _:_:_).
time(Hari:Jam:Tanggal) :- have_time(_NPM, _Is_Preferred, _:_:_, Hari:Jam:Tanggal).

