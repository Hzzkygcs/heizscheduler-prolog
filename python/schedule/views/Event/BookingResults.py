import json

from django.shortcuts import render

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import Event, DateRange, Schedule, BookingResult
from schedule.service.update_booking_result import update_booking_results_of_an_event
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.views.util import convert_to_datetime


class BookingResultsViews(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    def get(self, req, event_id):
        update_booking_results_of_an_event(event_id)
        event = Event.objects.get(ID=event_id)
        booked_slots = BookingResult.objects.filter(event=event).all()
        return render_get_for_event_details_and_booking_result(req, event, booked_slots,
                                                               "events-detail/booking-result.html")


def render_get_for_event_details_and_booking_result(req, event, booked_slots, template):
        set_user = set()
        for slot in booked_slots:
            set_user.add(slot.owner.npm)

        users_and_slots = []

        for user in set_user:
            users_and_slots.append({
                'user': user,
                'slots': []
            })

        for slot in booked_slots:
            for booked_slot in users_and_slots:
                if booked_slot['user'] == slot.owner.npm:
                    booked_slot['slots'].append({
                        'start': slot.start_date_time,
                        'end': slot.end_date_time,
                        'is_preferred': slot.is_preferred,
                    })

        return render(req, template, {
            'event_id': event.pk,
            'schedules': users_and_slots,
            'event_name': event.name,
        })

