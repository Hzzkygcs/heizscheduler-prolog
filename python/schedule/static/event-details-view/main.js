$(document).ready(function () {
    const parentEl = $(".list-of-schedules");
    const schedules = getSchedules();
    console.log(schedules)
    reloadListOfSchedule(schedules, parentEl);
})

function getSchedules(script_id='available-bookings'){
    const availBookings = load_json_data_from_script_tag(script_id);
    const convertedAvailBookings = {};

    for (const user of availBookings) {
        const userNpm = user.user;
        convertedAvailBookings[userNpm] = [];

        for (const availBooking of user.slots) {
            let start = new Date(availBooking.start);
            let end = new Date(availBooking.end);
            const startSplitted = splitDateTime(start);
            const endSplitted = splitDateTime(end);

            console.assert(startSplitted.date.valueOf() === endSplitted.date.valueOf());
            convertedAvailBookings[userNpm].push({
                schedule: new Schedule(startSplitted.date, startSplitted.time, endSplitted.time),
                start: start,
                end: end,
                is_preferred: availBooking.is_preferred,
                booker_name: userNpm,
            });
        }
    }
    console.log(convertedAvailBookings)
    return convertedAvailBookings;
}


/**
 * @param bookings  --> dict[NPM, list[bookings]]
 * @param parentElement
 */
function reloadListOfSchedule(bookings, parentElement){
    console.log("reloaded");
    for (const userNpm of Object.keys(bookings)){
        const bookingsOfTheUser = bookings[userNpm];
        console.log("NPM");
        console.log(bookingsOfTheUser);
        reloadListOfScheduleOfAUser(userNpm, bookingsOfTheUser);
    }
}
function reloadListOfScheduleOfAUser(npm, bookings){
    console.log(npm);
    bookings = bookings.sort((a, b) => SCHEDULE_SORT(a.schedule, b.schedule))
    const parentElement = newListItemOfSchedulesForEachNpmTemplate(npm);

    let index = 0;
    for (const booking of bookings) {
        console.log(booking.is_preferred)
        const newEl = instantiateItem(index, parentElement, booking,  booking.is_preferred, booking.booker_name)
        parentElement.append(newEl);
        index++;
    }
}
function newListItemOfSchedulesForEachNpmTemplate(npm) {
    const template = $(".list-of-schedules-for-each-npm-template");
    const ret = $(template.html());
    ret.addClass(`${npm}`);
    ret.find('.user-id').text(npm);

    $(".list-of-schedules-for-each-npm").append(ret);
    return ret.find('.list-of-schedules');
}



function instantiateItem(index, parentElement, booking, is_preferred, booker_name) {
    const schedule = booking.schedule;
    const date = dateObjToDateStringFormat(schedule.date);
    const startTime = schedule.startTime.toString();
    const endTime = schedule.endTime.toString();

    const newEl = initializeScheduleItem(date, startTime, endTime);
    if (!is_preferred){
        newEl.click(((availBooking) =>
            (e) => {
                console.log("clicked");
                showModal(date, `${startTime} - ${endTime}`);

                const submitBtn = $("#submit-modal-btn");
                submitBtn.unbind();
                submitBtn.click(() => {
                    submitModal(availBooking.start, availBooking.end);
                });
            }
        )(booking));
    }else{
        newEl.find(".booker-name").text(
            `[Booked by: ${booker_name}]`
        );
        newEl.addClass("red");
        newEl.addClass("unclickable");
    }
    return newEl;
}





function showModal(date, timeRepr){
    const el = $('#my-modal');
    el.css('display', 'grid');
    const book_btn = el.find('#submit-modal-btn');
    book_btn.html("Book");

    el.find('.date-repr').html(date)
    el.find('.time-repr').html(timeRepr)
}
function hideModal(){
    $('#my-modal').css('display', 'none');
}
function submitModal(startDateObj, endDateObj){
    const el = $('#my-modal');
    const booker_name = el.find('#booker-name').val();

    if (booker_name.length === 0){
        alert("Please enter your name")
        return;
    }

    const book_btn = el.find('#submit-modal-btn');
    book_btn.html("Booking");

    $.post(window.location, {
        data: JSON.stringify({
            start: startDateObj,
            end: endDateObj,
            name: booker_name,
        })
    }, function (data) {
        console.assert(data.success === 1);
        window.location.reload();
    }).fail((e) => {
        alert("Booking failed!");
        console.log(e);
    });
}

