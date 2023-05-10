from datetime import datetime

from django.db import transaction
from django.utils import timezone
from pydantic.schema import timedelta

from HzzProlog.PrologCallable import define_prolog_operator, Variable
from HzzProlog.test_util import remove_trailing_false_or_true
from definitions.functors import time_range, penalty_and_slots, booked_slot
from definitions.operators import time_point
from definitions.paths import get_main_prolog
from definitions.predicates import available, have_time, find_jadwal_and_score_sorted_member, get_best_jadwal
from definitions.variables import Result, X, Y
from schedule.exceptions.EventNotFoundException import EventNotFoundException
from schedule.exceptions.InvalidImpossibleScheduleException import InvalidImpossibleScheduleException
from schedule.models import Schedule, Event, DateRange
from schedule.models.BookingResult import BookingResult


@transaction.atomic
def update_booking_results_of_an_event(event_id):
    clear_booking_results_of_an_event(event_id)
    if get_member_count_of_an_event(event_id) == 1:
        print("Only one member, not generating new booking list...")
        return

    event = Event.objects.filter(ID=event_id).first()
    if event is None:
        raise EventNotFoundException()
    return generate_and_save_booking_results_of_an_event(event)

def get_member_count_of_an_event(event_id):
    owners = list(Schedule.objects.filter(event_id=event_id).values('owner'))
    unique_npm = set()
    for owner in owners:
        unique_npm.add(owner['owner'])
    return len(unique_npm)


def clear_booking_results_of_an_event(event_id):
    booking_results = BookingResult.objects.filter(event_id=event_id)
    booking_results.delete()


@transaction.atomic
def generate_and_save_booking_results_of_an_event(event: Event):
    facts = get__available__have_time__facts(event.pk, event.owner_id)
    results = get_prolog_best_jadwal(event.booking_minute_duration, facts)
    if len(results) == 0:
        raise InvalidImpossibleScheduleException()

    booking_results = []
    for result in results:
        booking_result = dictionary_to_booking_result(event.pk, result)
        booking_results.append(booking_result)
    return booking_results, results


PENALTY_SCORE = Variable('PENALTY_SCORE')
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
        PENALTY_SCORE,
        booked_slot(NPM, IS_PREFERRED, time_range(time_point(START_DAY, START_HOUR, START_MINUTE),
                                                  time_point(END_DAY, END_HOUR, END_MINUTE),))
    ), print_query=True)
    results = remove_trailing_false_or_true(results)
    return results

def get_prolog_all_jadwal(duration, facts):
    prolog = get_main_prolog()
    prolog.add_facts('definitions', facts)
    results = prolog.query_no_chained_equality(find_jadwal_and_score_sorted_member(
        duration,
        X
    ), print_query=True)
    results = remove_trailing_false_or_true(results)
    return results


def dictionary_to_booking_result(event_id, results: dict[Variable, BookingResult]):
    start = from_day_hour_minute(results[START_DAY], results[START_HOUR], results[START_MINUTE])
    end = from_day_hour_minute(results[END_DAY], results[END_HOUR], results[END_MINUTE])

    datetime_range = DateRange(start_date_time=start, end_date_time=end)
    datetime_range.save()
    return BookingResult.objects.create(
        owner_id=results[NPM],
        datetime_range=datetime_range,
        event_id=event_id,
        penalty_score=results[PENALTY_SCORE],
        is_preferred=results[IS_PREFERRED],
    )



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
        time_point(*get_day_hour_minute(start_time),),
        time_point(*get_day_hour_minute(end_time),),
    ))


def get__have_time__prolog_fact_from_schedule_object(schedule: Schedule):
    npm = schedule.owner.pk
    is_preferred = schedule.is_preferred
    start_time = schedule.start_date_time
    end_time = schedule.end_date_time

    return have_time(npm, int(is_preferred), time_range(
        time_point(*get_day_hour_minute(start_time)),
        time_point(*get_day_hour_minute(end_time)),
    ))


EPOCH = timezone.make_aware(datetime(1970, 1, 1))


def from_day_hour_minute(day, hour, minute):
    return EPOCH + timedelta(days=day, hours=hour, minutes=minute)


def get_day_hour_minute(date_time: datetime):
    since_epoch = date_time - EPOCH
    number_of_days = since_epoch.days

    remaining_hour_second: timedelta = date_time - (EPOCH + timedelta(days=number_of_days))
    remaining_total_seconds = remaining_hour_second.seconds
    minutes = remaining_total_seconds // 60
    minute = minutes % 60
    hour = minutes // 60

    ret = number_of_days, hour, minute
    return ret


temp_for_testing = timezone.make_aware(datetime(2020, 2, 3, 4, 5, 6))
assert (from_day_hour_minute(*get_day_hour_minute(temp_for_testing)) - temp_for_testing).microseconds == 0

