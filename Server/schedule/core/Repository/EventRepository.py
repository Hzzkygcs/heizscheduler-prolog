from kink import inject

from schedule.models import Event


@inject
class EventRepository():
    def save(self, model):
        model.save()

    def create(self):
        return Event()