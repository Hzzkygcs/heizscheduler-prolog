from django.db import transaction

from HzzProlog.PrologCallable import define_prolog_operator, Variable
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range, penalty_and_slots, booked_slot
from definitions.operators import time_point
from definitions.paths import get_main_prolog
from definitions.predicates import available, have_time, find_jadwal_and_score_sorted_member, get_best_jadwal
from definitions.variables import Result, X, Y
from schedule.exceptions.EventNotFoundException import EventNotFoundException
from schedule.exceptions.InvalidImpossibleScheduleException import InvalidImpossibleScheduleException
from schedule.models import Schedule, Event
from schedule.models.BookingResult import BookingResult

@transaction.atomic
def update_booking_results_of_an_event(event_id):
    clear_booking_results_of_an_event(event_id)
    event = Event.objects.filter(ID=event_id).first()
    if event is None:
        raise EventNotFoundException()
    return generate_and_save_booking_results_of_an_event(event)



def clear_booking_results_of_an_event(event_id):
    booking_results = BookingResult.objects.filter(event_id=event_id)
    booking_results.delete()


def generate_and_save_booking_results_of_an_event(event: Event):
    # TODO save ke database
    facts = get__available__have_time__facts(event.pk, event.owner_id)
    results = get_prolog_best_jadwal(event.booking_minute_duration, facts)
    # TODO convert to BookingResult
    if len(results) == 0:
        raise InvalidImpossibleScheduleException()
    return results



DONT_CARE_1 = Variable('DONT_CARE_1')
NPM = Variable('NPM')
IS_PREFERRED = Variable('IS_PREFERRED')
START_DAY = Variable('START_DAY')
START_HOUR = Variable('START_HOUR')
START_MINUTE = Variable('START_MINUTE')
END_DAY = Variable('END_DAY')
END_HOUR = Variable('END_HOUR')
END_MINUTE = Variable('END_MINUTE')



def get_prolog_best_jadwal(duration, facts):
    prolog = get_main_prolog()
    prolog.add_facts('definitions', facts)
    results = prolog.query_no_chained_equality(get_best_jadwal(
        duration,
        DONT_CARE_1,
        booked_slot(NPM, IS_PREFERRED, time_range(time_point(START_DAY, START_HOUR, START_MINUTE),
                                                  time_point(END_DAY, END_HOUR, END_MINUTE),))
    ), print_query=True)
    results = remove_trailing_false_or_true(results)
    return results




def get__available__have_time__facts(event_id, owner_id):
    schedules = list(Schedule.objects.filter(event_id=event_id).all())
    owner_schedules = get_owner_schedule_prolog_facts_from_list_of_schedules(owner_id, schedules)
    schedules = remove_owners_schedules_from_list(owner_id, schedules)
    guest_schedules = get_guest_schedule_prolog_facts_from_list_of_schedules(schedules)
    return owner_schedules + guest_schedules



def get_owner_schedule_prolog_facts_from_list_of_schedules(owner_id, schedules: list[Schedule]):
    owner_schedules = []
    for schedule in schedules:
        if schedule.owner_id == owner_id:
            fact = get__available__prolog_fact_from_schedule_object(schedule)
            owner_schedules.append(fact)
    return owner_schedules


def remove_owners_schedules_from_list(owner_id, schedules: list[Schedule]):
    ret = []
    for schedule in schedules:
        if schedule.owner_id != owner_id:
            ret.append(schedule)
    return ret


def get_guest_schedule_prolog_facts_from_list_of_schedules(schedules: list[Schedule]):
    ret = []
    for schedule in schedules:
        prolog_fact = get__have_time__prolog_fact_from_schedule_object(schedule)
        ret.append(prolog_fact)
    return ret


def get__available__prolog_fact_from_schedule_object(schedule: Schedule):
    start_time = schedule.start_date_time
    end_time = schedule.end_date_time

    return available(time_range(
        time_point(start_time.day, start_time.hour, start_time.minute),
        time_point(end_time.day, end_time.hour, end_time.minute),
    ))


def get__have_time__prolog_fact_from_schedule_object(schedule: Schedule):
    npm = schedule.owner.id
    is_preferred = schedule.is_preferred
    start_time = schedule.start_date_time
    end_time = schedule.end_date_time

    return have_time(npm, int(is_preferred), time_range(
        time_point(start_time.day, start_time.hour, start_time.minute),
        time_point(end_time.day, end_time.hour, end_time.minute),
    ))

