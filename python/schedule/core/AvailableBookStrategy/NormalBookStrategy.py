import datetime

from schedule.core.AvailableBookStrategy.AvailableBookStrategy import AvailableBookStrategy
from schedule.models import Schedule, DateRange, Event


class NormalBookStrategy(AvailableBookStrategy):
    def __init__(self, event_obj: Event, schedule_obj: Schedule):
        self.event = event_obj
        self.schedule = schedule_obj

    def get_available_slots(self) -> list[DateRange]:
        booking_width = self.event.slot_book_minute_width
        curr_date = self.schedule.start_date_time
        end_date = self.schedule.end_date_time
        assert curr_date <= end_date

        ret = []
        while True:
            next_date = curr_date + datetime.timedelta(minutes=booking_width)
            if next_date > end_date:
                break
            date_range = DateRange()
            date_range.start_date_time = curr_date
            date_range.end_date_time = next_date
            curr_date = next_date
            ret.append(date_range)
        return ret

