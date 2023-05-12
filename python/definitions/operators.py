from typing import Optional, Union

import deprecation

from HzzProlog.PrologCallable import define_prolog_operator, Variable


class datetime(define_prolog_operator('datetime', ":")):
    def __init__(self, day: int, hour: int, minute: int):
        super().__init__(day, hour, minute)


@deprecation.deprecated("time range using - operator is deprecated")
class deprecated_time_range(define_prolog_operator("time_range", "-")):
    def __init__(self, start, end, *args):
        super().__init__(start, end)


class time_point(define_prolog_operator("time_point", ":")):
    def __init__(self, day: Union[int, Variable[int]], hour: Union[int, Variable[int]],
                 minute: Optional[Union[int, Variable[int]]]=None):
        args = [day, hour]
        if minute is not None:
            args.append(minute)
        super().__init__(*args)
