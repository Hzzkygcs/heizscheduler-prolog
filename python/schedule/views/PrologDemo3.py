import json

from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.views import View

from definitions.functors import time_range
from definitions.operators import time_point
from definitions.paths import get_main_prolog
from definitions.predicates import available, have_time, find_jadwal_and_score_sorted_member
from definitions.variables import Result
from schedule.exceptions.EventNotFoundException import EventNotFoundException
from schedule.models import Event
from schedule.service.update_booking_result import update_booking_results_of_an_event, get__available__have_time__facts, \
    get_prolog_best_jadwal, get_prolog_all_jadwal


class PrologDemo3(View):
    def get(self, req, event_id):
        event = Event.objects.filter(ID=event_id).first()
        if event is None:
            raise EventNotFoundException()
        facts = get__available__have_time__facts(event.pk, event.owner_id)
        results = get_prolog_all_jadwal(event.booking_minute_duration, facts)
        return HttpResponse(str(results).encode('utf-8'), content_type="text/json")
