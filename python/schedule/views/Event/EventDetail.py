import json

from django.http import HttpResponse
from django.shortcuts import render

from auth_module.core.Factory.UserFactory import UserFactory
from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.core.repository.UserRepository import UserRepository
from auth_module.models import User
from schedule.models import Event, Schedule
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.Event.BookingResults import get_template_data_for_event_details_and_booking_result


# Create your views here

class EventDetail(BaseScheduleView):
    @authenticated
    def delete(self, req, logged_in_user: User, event_id):
        event_to_be_deleted = logged_in_user.event_set.get(ID=event_id)
        event_to_be_deleted.delete()

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')

    def get(self, req, event_id):
        event = Event.objects.get(ID=event_id)
        booked_slots = Schedule.objects.all().filter(event=event)
        data = get_template_data_for_event_details_and_booking_result(event, booked_slots)
        return render(req, "events-detail/available-booking-list.html", data)


