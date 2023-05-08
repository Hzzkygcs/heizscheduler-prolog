:- [find_jadwal].


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

% {{definitions}}
% {{available_definitions}}
% {{have_time_definitions}}
% {{testing_definitions}}



% TODO remove this:
available(time_range(1:12:10, 1:14:15)).
available(time_range(2:12:10, 2:14:15)).
available(time_range(3:12:10, 3:14:15)).

have_time(2006463162, 1, time_range(1:10:10, 1:15:10)).
have_time(2006463162, 1, time_range(3:13:45, 3:16:00)).
have_time(2006463162, 0, time_range(3:13:30, 3:18:00)).
have_time(2006462664, 0, time_range(1:14:30, 1:17:10)).
have_time(2006462664, 1, time_range(3:13:45, 3:16:00)).
have_time(2006462664, 0, time_range(3:13:30, 3:18:00)).