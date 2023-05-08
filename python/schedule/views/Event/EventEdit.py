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
from schedule.models import Event, Schedule
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import batch_convert_to_datetime


class EventEdit(BaseScheduleView):
    def __init__(self):
        super(EventEdit, self).__init__()
        self.schedule_factory = ScheduleFactory(di[DateRangeRepository], di[ScheduleRepository])

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, event_id, logged_in_user: User):
        event_to_be_edited = logged_in_user.event_set.get(ID=event_id)
        schedules = Schedule.objects.filter(event=event_to_be_edited, owner=logged_in_user).all()
        print(schedules)
        return render(req, "events/event-edit.html", {'event_id': event_id, 'schedules': schedules})

    @authenticated
    def put(self, req, event_id, logged_in_user: User):
        event_to_be_edited = logged_in_user.event_set.get(ID=event_id)
        if event_to_be_edited is None:
            raise BadRequestException("")

        body = req.PUT.get('schedules')
        if body is None:
            raise BadRequestException("No post data: 'schedules'")

        schedules = batch_convert_to_datetime(json.loads(body))
        print(schedules, logged_in_user.npm)
        self.saveNewEvent(event_to_be_edited, schedules, logged_in_user)

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')

    def saveNewEvent(self, event, schedules, user):
        created_schedules = []
        old_schedule = Schedule.objects.filter(event=event).all()
        for schedule in old_schedule:
            schedule.delete()

        for schedule in schedules:
            start, end = schedule['start'], schedule['end']
            new_schedule = self.schedule_factory.create_schedule(event.ID, start, end, owner=user)
            created_schedules.append(new_schedule)
        print(created_schedules)