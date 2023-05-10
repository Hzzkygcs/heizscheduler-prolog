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
        data = get_template_data_for_event_details_and_booking_result(event, booked_slots)
        data['penalty_score'] = get_penalty_score_from_booked_slots(booked_slots)

        return render(req, "events-detail/booking-result.html", data)


def get_penalty_score_from_booked_slots(booked_slots: list[BookingResult]):
    penalty_score = '-'
    if len(booked_slots) != 0:
        penalty_score = booked_slots[0].penalty_score
        for slot in booked_slots:
            assert penalty_score == slot.penalty_score
    return penalty_score


def get_template_data_for_event_details_and_booking_result(event, booked_slots):
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
                booked_slot['slots'].append(booking_result__or__schedule__to_dict(slot))

    return {
        'event_id': event.pk,
        'schedules': users_and_slots,
        'event_name': event.name,
    }


def booking_result__or__schedule__to_dict(slot):
    return {
        'start': slot.start_date_time,
        'end': slot.end_date_time,
        'is_preferred': slot.is_preferred,
    }