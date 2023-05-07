import json

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from django.shortcuts import render
from schedule.models import Event, DateRange
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import convert_to_datetime


class BookingDetail(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    def get(self, req, event_id):
        event = Event.objects.get(ID=event_id)
        available_booking_slots = event.get_all_booking_slots()
        available_booking_slots = [i.to_dict() for i in available_booking_slots]  # supaya json-able

        return render(req, "booking/available-booking-list.html", {
            'bookings': available_booking_slots,
            'event_name': event.name,
        })

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
        return render(req, "booking/available-booking-list.html", {
            'success': 1,
        })