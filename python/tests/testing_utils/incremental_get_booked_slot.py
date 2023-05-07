from definitions.functors import booked_slot, time_range
from definitions.operators import time_point
from definitions.variables import dont_care


class BookedSlotGenerator:
    def __init__(self, starting_minute=107):
        self.minute = starting_minute

    def get_booked_slot(self, minute_range_from_prev_slot, duration):
        self.minute += minute_range_from_prev_slot
        day_start, hour_start, minute_start = get_minute_hour_day(self.minute)
        self.minute += duration
        day_end, hour_end, minute_end = get_minute_hour_day(self.minute)

        return booked_slot(dont_care, dont_care, time_range(
            time_point(day_start, hour_start, minute_start),
            time_point(day_end, hour_end, minute_end),
        ))


def get_minute_hour_day(total_minutes):
    minute = total_minutes % 60
    remaining_in_hours = total_minutes // 60
    hour = remaining_in_hours % 24
    remaining_in_days = remaining_in_hours // 24
    day = remaining_in_days
    return day, hour, minute