import json

from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.views import View

from definitions.functors import time_range
from definitions.operators import time_point
from definitions.paths import get_main_prolog
from definitions.predicates import available, have_time, find_jadwal_and_score_sorted_member
from definitions.variables import Result


class PrologDemo(View):
    def get(self, req):
        prolog = get_main_prolog()
        prolog.add_facts("definitions", [
            available(time_range(time_point(3, 12, 10), time_point(3, 14, 15))),
            available(time_range(time_point(2, 12, 10), time_point(2, 14, 15))),
            available(time_range(time_point(1, 12, 10), time_point(1, 14, 15))),

            have_time(2006462664, 0, time_range(time_point(3, 13, 30), time_point(3, 18, 00))),
            have_time(2006462664, 1, time_range(time_point(3, 13, 45), time_point(3, 16, 00))),
            have_time(2006462664, 0, time_range(time_point(1, 14, 30), time_point(1, 17, 10))),
            have_time(2006463162, 0, time_range(time_point(3, 13, 30), time_point(3, 18, 00))),
            have_time(2006463162, 1, time_range(time_point(3, 13, 45), time_point(3, 16, 00))),
            have_time(2006463162, 1, time_range(time_point(1, 10, 10), time_point(1, 15, 10))),
        ])
        result = prolog.query(find_jadwal_and_score_sorted_member(15, Result))
        return HttpResponse(str(result).replace("'", '"').encode('utf-8'), content_type="text/json")
