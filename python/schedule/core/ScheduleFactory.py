from schedule.core.Repository.DateRangeRepository import DateRangeRepository
from schedule.core.Repository.ScheduleRepository import ScheduleRepository
from schedule.models import Schedule


class ScheduleFactory:
    def __init__(self, date_range_repository: DateRangeRepository, schedule_repository: ScheduleRepository):
        self.date_range_repository = date_range_repository
        self.schedule_repository = schedule_repository

    def create_schedule(self, event_id, start, end):
        date_range = self.date_range_repository.create_and_save(start, end)

        date_range_id = date_range.ID
        schedule = Schedule.objects.create(datetime_range_id=date_range_id, event_id=event_id)
        return schedule
