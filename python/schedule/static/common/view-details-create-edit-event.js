


function instantiateItem(schedule, is_preferred, {on_click = null, onDelete = null}={}) {
    const date = dateObjToDateStringFormat(schedule.date);
    const startTime = schedule.startTime.toString();
    const endTime = schedule.endTime.toString();

    const newEl = initializeScheduleItem(date, startTime, endTime, false);
    newEl.click(on_click ?? (() => {}));
    if (is_preferred)
        newEl.addClass("starred");
    if (onDelete != null) {
        newEl.addClass("deletable");
        newEl.find(".delete-btn").on('click', onDelete);
    }
    if (on_click == null)
        newEl.removeClass("clickable");
    return newEl;
}