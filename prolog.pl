
available(hari(1, 12, 10), hari(1, 14, 15)).
available(hari(2, 12, 10), hari(2, 14, 15)).
available(hari(3, 12, 10), hari(3, 14, 15)).

time(Hari, Jam, Tanggal) :- available(hari(Hari, Jam, Tanggal), hari(_, _, _)).
time(Hari, Jam, Tanggal) :- available(hari(_, _, _), hari(Hari, Jam, Tanggal)).

