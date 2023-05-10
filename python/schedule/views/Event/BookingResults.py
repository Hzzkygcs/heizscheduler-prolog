from django.shortcuts import render

from auth_module.core.decorator.AuthenticatedDecorator import authenticated
from auth_module.models import User
from schedule.models import Event, BookingResult
from schedule.service.update_booking_result import update_booking_results_of_an_event
from schedule.views.BaseScheduleView import BaseScheduleView
from schedule.service.utils_for_event_details__my_schedule__booking_result import apply_label_modifier, \
    get_penalty_score_from_booked_slots, get_template_data_for_event_details_and_booking_result


class BookingResultsViews(BaseScheduleView):
    def __init__(self):
        super().__init__()

    def setup(self, request, *args, **kwargs):
        super().setup(request, *args, **kwargs)

    @authenticated
    def get(self, req, logged_in_user: User, *, event_id):
        update_booking_results_of_an_event(event_id)
        event = Event.objects.get(ID=event_id)
        booked_slots = BookingResult.objects.filter(event=event).all()
        data = get_template_data_for_event_details_and_booking_result(event, booked_slots)
        data['penalty_score'] = get_penalty_score_from_booked_slots(booked_slots)
        data['schedules'] = apply_label_modifier(event.owner.pk, logged_in_user.npm, data['schedules'])

        return render(req, "events-detail/booking-result.html", data)


