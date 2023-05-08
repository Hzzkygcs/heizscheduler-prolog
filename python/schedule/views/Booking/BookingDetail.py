import json

from django.shortcuts import render

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import Event, DateRange, Schedule
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import convert_to_datetime


class BookingDetail(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    def get(self, req, event_id):
        event = Event.objects.get(ID=event_id)
        booked_slots = Schedule.objects.all().filter(event=event)

        set_user = set()
        for slot in booked_slots:
            set_user.add(slot.owner.npm)

        booked_slots_lst = []

        for user in set_user:
            booked_slots_lst.append({
                'user': user,
                'slots': []
            })

        for slot in booked_slots:
            for booked_slot in booked_slots_lst:
                if booked_slot['user'] == slot.owner.npm:
                    booked_slot['slots'].append({
                        'start': slot.start_date_time,
                        'end': slot.end_date_time,
                        'is_preferred': slot.is_preferred,
                    })

        return render(req, "booking/available-booking-list.html", {
            'schedules': booked_slots_lst,
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
