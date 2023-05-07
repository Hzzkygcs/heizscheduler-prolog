from schedule.exceptions.InvalidBookingExceptions import InvalidBookingExceptions
from schedule.models import DateRange


class AvailableBookStrategy:
    def get_available_slots(self) -> list[DateRange]:
        raise NotImplementedError()

    def is_slot_valid(self, daterange: DateRange):
        available_slots = self.get_available_slots()
        for available_slot in available_slots:
            if daterange.more_or_less_the_same(available_slot):
                return True
        return False

