from django.views import View
from kink import di, inject

from schedule.core.Repository.BookingRepository import BookingRepository
from schedule.core.Repository.DateRangeRepository import DateRangeRepository
from schedule.core.Repository.EventRepository import EventRepository
from schedule.core.Repository.LabelRepository import LabelRepository
from schedule.core.Repository.ScheduleRepository import ScheduleRepository



class BaseScheduleView(View):
    def __init__(self):
        super().__init__()
        self._book_repository = di[BookingRepository]
        self._date_range_repository = di[DateRangeRepository]
        self._event_repository = di[EventRepository]
        self._label_repository = di[LabelRepository]
        self._schedule_repository = di[ScheduleRepository]


