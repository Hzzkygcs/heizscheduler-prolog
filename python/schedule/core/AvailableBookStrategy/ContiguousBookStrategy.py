from datetime import timedelta

from schedule.core.AvailableBookStrategy.AvailableBookStrategy import AvailableBookStrategy
from schedule.core.AvailableBookStrategy.NormalBookStrategy import NormalBookStrategy
from schedule.models import DateRange, Schedule, Event, Booking


class ContiguousBookStrategy(AvailableBookStrategy):
    def __init__(self, event_obj: Event, schedule_obj: Schedule):
        self.event = event_obj
        self.schedule = schedule_obj

    def get_available_slots(self) -> list[DateRange]:
        if self._any_slot_is_booked:
            return self._get_avail_slot_when_theres_a_booked_slot()
        return self._get_avail_slot_when_all_slot_is_not_booked()

    @property
    def _any_slot_is_booked(self):
        return self.schedule.booking_set.exists()

    def _get_avail_slot_when_all_slot_is_not_booked(self):
        normal_book_str = NormalBookStrategy(self.event, self.schedule)
        return normal_book_str.get_available_slots()

    def _get_avail_slot_when_theres_a_booked_slot(self):
        booking_width = self.event.slot_book_minute_width
        bookings = self.schedule.booking_set.all()
        booked_slots = []
        for i in range(len(bookings)):
            booked_slots.append(bookings[i].datetime_range)

        ret = []
        for booked_slot in booked_slots:
            slot = self._create_slot_before(booked_slot, booking_width)
            if not self.schedule.datetime_range.contains_or_equal(slot):
                continue
            if self._does_slot_collides(slot, booked_slots):
                continue
            if self._is_slot_continuous(slot, booked_slots):
                ret.append(slot)
        for booked_slot in booked_slots:
            slot = self._create_slot_after(booked_slot, booking_width)
            if not self.schedule.datetime_range.contains_or_equal(slot):
                continue
            if self._does_slot_collides(slot, booked_slots):
                continue
            if self._is_slot_continuous(slot, booked_slots):
                ret.append(slot)
        return ret

    def _is_slot_continuous(self, slot: DateRange, booked_slots: list[DateRange], tolerance=timedelta(seconds=5)):
        if len(booked_slots) == 0:
            return True

        for booked_slot in booked_slots:
            if abs(slot.start_date_time - booked_slot.end_date_time) < tolerance:
                return True
            if abs(slot.end_date_time - booked_slot.start_date_time) < tolerance:
                return True
        return False

    def _does_slot_collides(self, slot: DateRange, booked_slots: list[DateRange]):
        for booked_slot in booked_slots:
            if slot.intersection_status(booked_slot).intersects_or_contains():
                return True
        return False

    def _create_slot_before(self, booked_slot: DateRange, booking_width) -> DateRange:
        new_slot_end_time = booked_slot.start_date_time
        new_slot_start_time = new_slot_end_time - timedelta(minutes=booking_width)
        return DateRange(start_date_time=new_slot_start_time, end_date_time=new_slot_end_time)

    def _create_slot_after(self, booked_slot: DateRange, booking_width) -> DateRange:
        new_slot_start_time = booked_slot.end_date_time
        new_slot_end_time = new_slot_start_time + timedelta(minutes=booking_width)
        return DateRange(start_date_time=new_slot_start_time, end_date_time=new_slot_end_time)