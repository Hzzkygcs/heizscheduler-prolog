import json

from django.shortcuts import render

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import Event, DateRange, Schedule
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.Event.BookingResults import get_template_data_for_event_details_and_booking_result
from schedule.views.util import convert_to_datetime


class BookingDetail(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    def get(self, req, event_id):
        event = Event.objects.get(ID=event_id)
        booked_slots = Schedule.objects.all().filter(event=event)
        data = get_template_data_for_event_details_and_booking_result(event, booked_slots)
        return render(req, "events-detail/available-booking-list.html", data)

    @authenticated
    def post(self, req, logged_in_user: User, event_id):
        event = logged_in_user.event_set.get(ID=event_id)

        data = req.POST['data']
        parsed_json = json.loads(data)

        name = parsed_json['name']
        start = convert_to_datetime(parsed_json['start'])
        end = convert_to_datetime(parsed_json['end'])
        daterange = DateRange(start_date_time=start, end_date_time=end)

        event.save_booking_if_valid(name, daterange)
        return render(req, "events-detail/available-booking-list.html", {
            'success': 1,
        })
