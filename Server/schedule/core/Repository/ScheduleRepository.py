from kink import inject

from schedule.models import Schedule


@inject
class ScheduleRepository():
    def save(self, model):
        model.save()

    def create(self):
        return Schedule()