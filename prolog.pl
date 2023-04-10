:- [hzztime].



% {%begin ignore%}

available(1:12:10, 1:14:15).
available(2:12:10, 2:14:15).
available(3:12:10, 3:14:15).

% {%end ignore%}
% {{available_definitions}}


time(Hari, Jam, Tanggal) :- available(Hari:Jam:Tanggal, _:_:_).
time(Hari, Jam, Tanggal) :- available(_:_:_, Hari:Jam:Tanggal).

