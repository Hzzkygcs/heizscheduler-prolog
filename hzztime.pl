% Format waktu: range(00:00:00, 06:23:59)
:- op(500, xfy, :).

convert_from_minutes(Minutes, Time) :-
    M is Minutes mod 60,
    Hours is Minutes // 60,
    H is Hours mod 24,
    D is Hours // 24,
    Time = D:H:M.

convert_to_minutes(D:H:M, Minutes) :-
    Minutes is D * 24 * 60 + H * 60 + M.

