from django.utils import timezone
from datetime import datetime
import json

from django.http import HttpResponse
from django.shortcuts import render
from kink import di

from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.core.repository.UserRepository import UserRepository
from auth_module.models import User
from global_exception.exceptions import BadRequest
from global_exception.exceptions.BadRequest import BadRequestException
from schedule.core.Repository.DateRangeRepository import DateRangeRepository
from schedule.core.Repository.ScheduleRepository import ScheduleRepository
from schedule.core.ScheduleFactory import ScheduleFactory
from schedule.models import Event
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import batch_convert_to_datetime


class EventCreate(BaseScheduleView):
    def __init__(self):
        super(EventCreate, self).__init__()
        self.schedule_factory = ScheduleFactory(di[DateRangeRepository], di[ScheduleRepository])

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, logged_in_user: User):
        events = logged_in_user.get_list_of_events()
        print(events)
        return render(req, "events/event-create.html", {})

    @authenticated
    def post(self, req, logged_in_user: User):
        body = req.POST.get('schedules')
        if body is None:
            raise BadRequestException("No post data: 'schedules'")
        event_name = req.POST.get('event_name')
        if event_name is None:
            raise BadRequestException("No post data: 'event_name'")

        schedules = batch_convert_to_datetime(json.loads(body))

        user_npm = logged_in_user.npm
        self.saveNewEvent(user_npm, event_name, schedules)

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')

    def saveNewEvent(self, user_npm, name, schedules):
        created_schedules = []
        owner = User.objects.get(npm=user_npm)
        event = Event.objects.create(name=name, owner=owner,
                                     slot_book_minute_width=30)

        for schedule in schedules:
            start, end = schedule['start'], schedule['end']
            new_schedule = self.schedule_factory.create_schedule(event.ID, user_npm, start, end)
            created_schedules.append(new_schedule)
        print(created_schedules)