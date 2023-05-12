import json

from django.http import HttpResponse
from django.shortcuts import render

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import Event, Schedule
from schedule.service.utils_for_event_details__my_schedule__booking_result import apply_label_modifier, \
    get_template_data_for_event_details_and_booking_result
from schedule.views.BaseScheduleView import BaseScheduleView


# Create your views here

class EventDetail(BaseScheduleView):
    @authenticated
    def delete(self, req, logged_in_user: User, event_id):
        event_to_be_deleted = logged_in_user.event_set.get(ID=event_id)
        event_to_be_deleted.delete()

        response = {'success': 1}
        return HttpResponse(json.dumps(response), content_type='application/json')

    @authenticated
    def get(self, req, logged_in_user: User, *, event_id):
        event = Event.objects.get(ID=event_id)
        booked_slots = Schedule.objects.all().filter(event=event)

        event_owner_npm = event.owner.npm
        logged_in_npm = str(logged_in_user.npm)
        data = get_template_data_for_event_details_and_booking_result(event, booked_slots)
        data['schedules'] = apply_label_modifier(event_owner_npm, logged_in_npm, data['schedules'])

        return render(req, "events-detail/available-booking-list.html", data)


