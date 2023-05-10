/**
 * @type {Schedule[]}
 */
let schedules = [];


const DATE_EL = "#schedule-date";
const START_TIME_EL = "#start-time";
const END_TIME_EL = "#end-time";
const IS_PREFERRED = "#preferred";
const LIST_OF_SCHEDULES_EL = ".list-of-schedules";

const eventCreateInputModal = {};




function initiateTimepickerDatepicker() {
    window.start_time_el = null;
    window.end_time_el = null;

    window.date_el = new te.Datepicker($(DATE_EL)[0], {
        disablePast: true,
    });

    setStartTimeElementValue("00:00");
    setEndTimeElementValue("03:00");
}

function setStartTimeElementValue(value) {
    if (window.start_time_el != null)
        window.start_time_el.dispose();
    window.start_time_el = new te.Timepicker($(START_TIME_EL)[0], {
        increment: true,
        format24: true,
        defaultTime: value,
    });
}
function setEndTimeElementValue(value) {
    if (window.end_time_el != null)
        window.end_time_el.dispose();
    window.end_time_el = new te.Timepicker($(END_TIME_EL)[0], {
        increment: true,
        format24: true,
        defaultTime: value,
    });
}



function showModal(){
    $('#authentication-modal').css('display', 'grid');
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


function hideModal(){
    $('#authentication-modal').css('display', 'none');
}

function getScheduleObjFromModalInput(){
    let date = getDateObjFromDatePicker($(DATE_EL));
    let start = getDataFromTimePicker($(START_TIME_EL));
    let end = getDataFromTimePicker($(END_TIME_EL));
    let is_preferred = $(IS_PREFERRED)[0].checked;

    if (start > end){
        throw new ValidationError("Start time cannot be greater than the end time");
    }
    if (!isValidDate(date)){
        throw new ValidationError("Please pick valid date");
    }
    return new Schedule(date, start, end, is_preferred);
}

function isValidDate(d) {
    return d instanceof Date && !isNaN(d);
}

function submitModal(){
    let schedulesCopy = schedules.slice();

    const schedule = getScheduleObjFromModalInput();
    if (schedule == null)  // validation failed
        return;

    schedulesCopy.push(schedule);
    schedulesCopy = schedulesCopy.sort(SCHEDULE_SORTING_FUNCTION);

    schedules = schedulesCopy;
    hideModal();
    reloadListOfSchedule(schedules, $(LIST_OF_SCHEDULES_EL))
}


/**
 * @param {Schedule[]} schedules
 * @param parentElement
 */
function reloadListOfSchedule(schedules, parentElement){
    console.log("reloaded");
    schedules = schedules.sort(SCHEDULE_SORTING_FUNCTION)
    parentElement = $(parentElement);
    parentElement.empty();

    let index = 0;
    for (const schedule of schedules) {
        const onDelete = ((ind) => (e) => {
            const isConfirmed = confirm(`Do you really want to delete this schedule?`);
            if (isConfirmed){
                schedules.splice(ind, 1);
                reloadListOfSchedule(schedules, parentElement);
            }
        })(index);
        const onClick = ((schedule) => (e) => {
            schedule.preferred = !schedule.preferred;
            reloadListOfSchedule(schedules, parentElement);
        })(schedule);
        const newEl = instantiateItem(schedule, schedule.preferred, {
            onDelete:onDelete, onClick: onClick});
        parentElement.append(newEl);
        index++;
    }
}


/**
 * @param {Schedule[]} schedules
 * @returns {boolean}
 */
function noOverlappingSchedule(schedules){
    schedules = schedules.sort(SCHEDULE_SORTING_FUNCTION);

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




function commonSaveSchedulesToServer(url="",
                                     validation=()=>{},
                                     onSuccess=(x)=>{window.location.href = "../";}){
    validation();

    const data = [];
    for (const schedule of schedules) {
        data.push({
            start: schedule.getStartDateObj(),
            end: schedule.getEndDateObj(),
            is_preferred: schedule.preferred,
        })
    }

    $.post(url, {
        event_name: $("#event_name").val(),
        schedules: JSON.stringify(data),
    }, (data) => {
        if (data.success === 1) {
            onSuccess(data);
        }else
            alert(data);
    }).fail(function (data) {
        alert(data.responseJSON.err_msg);
    });
}

const MINUTE_STEPS = 15;

function initiateTimeWheelSlider() {
    let start = "00:00";
    let end = "03:00";
    let slider;


    $('#circular-range-slider .slider-content').roundSlider({
        radius: 80,
        width: 6,
        handleSize: "+16",
        handleShape: "dot",
        sliderType: "range",
        startAngle: 90,
        endAngle: "+360",
        min: 0,
        max: 24*60 / MINUTE_STEPS,
        step: 1,
        tooltipFormat: labelFormatter,
        update: ()=>{
            const start_in_minutes = slider.getValue(1)*MINUTE_STEPS;
            const end_in_minutes = slider.getValue(2)*MINUTE_STEPS;

            // must be done latest because this will alter slider.getValue()
            setStartTimeElementValue(integerToTimeString(start_in_minutes))
            setEndTimeElementValue(integerToTimeString(end_in_minutes))
        }
    });
    slider = $("#circular-range-slider .slider-content").data("roundSlider");

    function refreshTimeWheel() {
        const startInMinutesSinceMidnight = Math.floor(timeStringToIntegerMinutes(start) / MINUTE_STEPS);
        const endInMinutesSinceMidnight = Math.floor(timeStringToIntegerMinutes(end) / MINUTE_STEPS);
        slider.setValue(`${startInMinutesSinceMidnight},${endInMinutesSinceMidnight}`);
    }
    $(START_TIME_EL).on('input.te.timepicker', function (input) {
        start = input.target.value;
        refreshTimeWheel();
    });
    $(END_TIME_EL).on('input.te.timepicker', function (input) {
        end = input.target.value;
        refreshTimeWheel();
    });
    refreshTimeWheel();
}

function labelFormatter(args) {
    return integerToTimeString(parseInt(args.value) * MINUTE_STEPS);
}

function integerToTimeString(total_minutes) {
    const hour = Math.floor(total_minutes / 60);
    const minutes = total_minutes % 60;
    const two_digit_hour = hour.toLocaleString('en-US', {minimumIntegerDigits: 2, useGrouping:false});
    const two_digit_minutes = minutes.toLocaleString('en-US', {minimumIntegerDigits: 2, useGrouping:false});
    return `${two_digit_hour}:${two_digit_minutes}`;
}

function timeStringToIntegerMinutes(string) {
    const splitteed = string.split(":");
    const hourString = splitteed[0];
    const minuteString = splitteed[1];
    const hour = parseInt(hourString);
    const minute = parseInt(minuteString);
    return hour * 60 + minute;
}

