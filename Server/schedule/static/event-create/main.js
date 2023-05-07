let schedules = [];


const DATE_EL = "#schedule-date";
const START_TIME_EL = "#start-time";
const END_TIME_EL = "#end-time";
const LIST_OF_SCHEDULES_EL = ".list-of-schedules";

const eventCreateInputModal = {};




function showModal(){
    $('#authentication-modal').css('display', 'grid');
    if ($(DATE_EL).val() === '')
        setToToday($(DATE_EL), 0);
    if ($(START_TIME_EL).val() === '')
        setToThisTime($(START_TIME_EL), 30*60*1000);
    if ($(END_TIME_EL).val() === '')
        setToThisTime($(END_TIME_EL), 60*60*1000);
}

function setToToday(el, addMiliSecond){
    console.log("today")
    addMiliSecond += 7 * 60 * 60 * 1000;  // UTF+7

    let dateObj = new Date();
    dateObj = new Date(dateObj.getTime() + addMiliSecond);
    const date = dateObj.getDate();
    const month = dateObj.getMonth();
    const year = dateObj.getFullYear();

    const text = `${date}/${month}/${year}`;
    console.log(text);
    $(el).val(text);
}
function setToThisTime(el, addMiliSecond){
    addMiliSecond += 7 * 60 * 60 * 1000;  // UTF+7

    let dateObj = new Date();
    dateObj = new Date(dateObj.getTime() + addMiliSecond);
    const hour = dateObj.getHours();
    const minutes = dateObj.getMinutes();

    $(el).val(`${hour}:${minutes}`);
}


function hideModal(){
    $('#authentication-modal').css('display', 'none');
}

function getScheduleObjFromModalInput(){
    let date = getDateObjFromDatePicker($(DATE_EL));
    let start = getDataFromTimePicker($(START_TIME_EL));
    let end = getDataFromTimePicker($(END_TIME_EL));

    if (start > end){
        throw new ValidationError("Start time cannot be greater than the end time");
    }
    return new Schedule(date, start, end);
}

function submitModal(){
    let schedulesCopy = schedules.slice();

    const schedule = getScheduleObjFromModalInput();
    if (schedule == null)  // validation failed
        return;

    schedulesCopy.push(schedule);
    schedulesCopy = schedulesCopy.sort(SCHEDULE_SORT);

    if (!noOverlappingSchedule(schedulesCopy)) {
        throw new ValidationError("This schedule overlaps another schedule");
    }

    schedules = schedulesCopy;
    hideModal();
    reloadListOfSchedule(schedules, $(LIST_OF_SCHEDULES_EL))
}



$(document).ready(() => {
    reloadListOfSchedule(schedules, $(LIST_OF_SCHEDULES_EL));
});

/**
 * @param {Schedule[]} schedules
 * @param parentElement
 */
function reloadListOfSchedule(schedules, parentElement){
    console.log("reloaded");
    schedules = schedules.sort(SCHEDULE_SORT)
    parentElement = $(parentElement);
    parentElement.empty();

    let index = 0;
    for (const schedule of schedules) {
        const date = dateObjToDateStringFormat(schedule.date);
        const startTime = schedule.startTime.toString();
        const endTime = schedule.endTime.toString();

        const newEl = initializeScheduleItem(date, startTime, endTime);

        const delBtn = newEl.find(".delete-btn");
        delBtn.click(((ind) => (e) => {
            const isConfirmed = confirm(`Do you really want to delete this schedule?`);
            if (isConfirmed){
                schedules.splice(ind, 1);
                reloadListOfSchedule(schedules, parentElement);
            }
        })(index));

        parentElement.append(newEl);
        index++;
    }
}


/**
 * @param {Schedule[]} schedules
 * @returns {boolean}
 */
function noOverlappingSchedule(schedules){
    schedules = schedules.sort(SCHEDULE_SORT);

    for (let i = 0; i < schedules.length - 1; i++) {
        const curr = schedules[i];
        const next = schedules[i+1];
        const currEnd = curr.valueOfEndTime();
        const nextStart = next.valueOfStartime();
        if (currEnd > nextStart) {
            console.log(curr);
            console.log(next);
            return false;
        }
    }
    return true
}


function validate(){
    if ($("#event_name").val().length == 0){
        throw new ValidationError("Event Name should not be null");
    }

}

function saveToServer(){
    validate();

    const data = [];
    for (const schedule of schedules) {
        data.push({
            start: schedule.getStartDateObj(),
            end: schedule.getEndDateObj(),
        })
    }

    $.post("/events/create", {
        event_name: $("#event_name").val(),
        schedules: JSON.stringify(data),
    }, (data) => {
        if (data.success === 1) {
            window.location.href = "../";
        }else
            alert(data);
    });
}