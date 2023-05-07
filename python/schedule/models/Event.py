from django.db import models
from django.db.models import Model

from auth_module.models import User
from schedule.core.Repository.BookingRepository import BookingRepository
from schedule.core.Repository.DateRangeRepository import DateRangeRepository
from schedule.exceptions.InvalidBookingExceptions import InvalidBookingExceptions
from schedule.models import DateRange, Booking, Schedule
from schedule.models.not_django_models.BookingAvailability import BookingAvailability


class Event(Model):
    ID = models.AutoField(primary_key=True)
    name = models.CharField(max_length=25)
    owner = models.ForeignKey(User, on_delete=models.CASCADE)

    slot_selection_minute_multiplier = models.IntegerField()
    slot_book_minute_width = models.IntegerField()

    def get_all_booking_slots(self) -> list[BookingAvailability]:
        ret = []
        ret.extend(self.get_all_available_booking_slots())
        ret.extend(self.get_all_booked_slots())
        return ret

    def get_all_available_booking_slots(self) -> list[BookingAvailability]:
        ret = []
        schedules = self.schedule_set.all()

        for schedule in schedules:
            available_slots = schedule.get_available_slots()
            for available_slot in available_slots:
                ret.append(BookingAvailability(schedule.ID, available_slot, True))
        return ret

    def get_all_booked_slots(self):
        ret = []
        schedules = self.schedule_set.all()

        for schedule in schedules:
            bookings = schedule.booking_set.all()
            for booking in bookings:
                ret.append(BookingAvailability(schedule.ID, booking.datetime_range, False, booking.name))
        return ret

    def save_booking_if_valid(self, booker_name, daterange: DateRange):
        schedules = self.schedule_set.all()
        for schedule in schedules:
            available_booking_strategy = schedule.available_book_strategy
            if available_booking_strategy.is_slot_valid(daterange):
                self.save_booking(booker_name, daterange, schedule)
                return
        raise InvalidBookingExceptions()

    def save_booking(self, booker_name, slot: DateRange, schedule: Schedule):
        date_range_repository = DateRangeRepository()
        date_range = date_range_repository.create_and_save(slot.start_date_time, slot.end_date_time)

        booking_repository = BookingRepository()
        booking_repository.create(booker_name, date_range.ID, schedule.ID)

