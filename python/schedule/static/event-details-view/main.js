$(document).ready(function () {
    const parentEl = $(".list-of-schedules");
    const schedules = getSchedules();
    console.log(schedules)
    reloadListOfSchedule(schedules, parentEl, (lst) => {
        lst = putUserToTop("(Me", lst);
        return putUserToTop("Host)", lst);
    });
});

/**
 * @param {string} substring
 * @param {string[]} NPMs
 */
function putUserToTop(substring, NPMs) {
    let index = 0;
    for (const npm of NPMs) {
        if (npm.toLowerCase().includes(substring.toLowerCase()))
            break;
        index++;
    }
    if (index >= NPMs.length)
        return NPMs;
    const data = NPMs[index];
    NPMs.splice(index, 1);
    NPMs.unshift(data);
    return NPMs;
}


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
                onclick_redirect: availBooking.onclick_redirect,
            });
        }
    }
    return convertedAvailBookings;
}


/**
 * @param bookings  --> dict[NPM, list[bookings]]
 * @param parentElement
 * @param sorter
 */
function reloadListOfSchedule(bookings, parentElement, sorter=(x)=>x){
    console.log("reloaded");
    console.log(bookings)

    let keys = Object.keys(bookings);
    keys = sorter(keys);
    for (const userNpm of keys){
        const bookingsOfTheUser = bookings[userNpm];
        reloadListOfScheduleOfAUser(userNpm, bookingsOfTheUser);
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



function reloadListOfScheduleOfAUser(npm, bookings){
    bookings = bookings.sort((a, b) => SCHEDULE_SORTING_FUNCTION(a.schedule, b.schedule))
    const parentElement = newListItemOfSchedulesForEachNpmTemplate(npm);

    for (const booking of bookings) {
        let onClick = null;

        if (booking.onclick_redirect != null){
            onClick = ((onclick_redirect) => (e) => {
                window.location = onclick_redirect;
            })(booking.onclick_redirect);
        }
        console.log(booking.onclick_redirect, onClick);

        const newEl = instantiateItem(booking.schedule, booking.is_preferred, {
            onClick: onClick
        })
        parentElement.append(newEl);
    }
}






function showModal(date, timeRepr){
    throw new Error("not implemented");

}
function hideModal(){
    $('#my-modal').css('display', 'none');
}
function submitModal(startDateObj, endDateObj){
    throw new Error("not implemented");

}

