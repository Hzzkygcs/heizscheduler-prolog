from django.db import transaction
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
from schedule.exceptions.EventNotFoundException import EventNotFoundException
from schedule.models import Event, Schedule
from schedule.service.update_booking_result import update_booking_results_of_an_event
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import batch_convert_to_datetime


class EventEdit(BaseScheduleView):
    def __init__(self):
        super(EventEdit, self).__init__()
        self.schedule_factory = ScheduleFactory(di[DateRangeRepository], di[ScheduleRepository])

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, logged_in_user: User, *, event_id):
        print('event_id', event_id)
        event_to_be_edited = get_event_by_id(event_id)
        schedules_obj = Schedule.objects.filter(event=event_to_be_edited, owner=logged_in_user).all()
        
        schedules = []
        for schedule in schedules_obj:
            schedules.append({
                'npm': schedule.owner.npm,
                'start': schedule.start_date_time,
                'end': schedule.end_date_time,
                'is_preferred': schedule.is_preferred,
            })
        print(schedules)
        data = {
                'event_id': event_id, 
                'event_name': event_to_be_edited.name, 
                'schedules': schedules
                }
        print(data)
        return render(req, "events/event-edit.html", data)

    @transaction.atomic
    @authenticated
    def post(self, req, logged_in_user: User, *, event_id):
        event_to_be_edited = get_event_by_id(event_id)

        body = req.POST.get('schedules')
        if body is None:
            raise BadRequestException("No post data: 'schedules'")

        parsed_body = json.loads(body)
        schedules = batch_convert_to_datetime(parsed_body)
        print(schedules, logged_in_user.npm)
        self.saveNewEvent(event_to_be_edited, schedules, logged_in_user)
        update_booking_results_of_an_event(event_id)

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')

    def saveNewEvent(self, event, schedules, user):
        created_schedules = []
        old_schedule = Schedule.objects.filter(event=event, owner_id=user.pk).all()
        old_schedule.delete()

        for schedule in schedules:
            start, end, is_preferred = schedule['start'], schedule['end'], schedule['is_preferred']
            new_schedule = self.schedule_factory.create_schedule(event_id=event.ID, owner_id=user.npm,
                                                                 start=start, end=end, is_preferred=is_preferred)
            created_schedules.append(new_schedule)
        print(created_schedules)


def extract_is_preferred(lst):
    ret = []
    for i in lst:
        ret.append(i['is_preferred'])
    return ret


def get_event_by_id(event_id):
    event_to_be_edited = Event.objects.filter(ID=event_id).first()
    if event_to_be_edited is None:
        raise EventNotFoundException()
    return event_to_be_edited