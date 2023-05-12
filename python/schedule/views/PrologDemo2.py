import json

from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.views import View

from definitions.functors import time_range
from definitions.operators import time_point
from definitions.paths import get_main_prolog
from definitions.predicates import available, have_time, find_jadwal_and_score_sorted_member
from definitions.variables import Result
from schedule.service.update_booking_result import update_booking_results_of_an_event


class PrologDemo2(View):
    def get(self, req, event_id):
        _, res = update_booking_results_of_an_event(event_id)
        return HttpResponse(json.dumps(res).encode('utf-8'), content_type="text/json")
