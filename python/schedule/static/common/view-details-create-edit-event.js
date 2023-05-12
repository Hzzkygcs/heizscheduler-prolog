


function instantiateItem(schedule, is_preferred, {onClick: onClick = null, onDelete = null}={}) {
    const date = dateObjToDateStringFormat(schedule.date);
    const startTime = schedule.startTime.toString();
    const endTime = schedule.endTime.toString();

    const newEl = initializeScheduleItem(date, startTime, endTime, false);

    if (is_preferred)
        newEl.addClass("starred");
    if (onDelete != null) {
        newEl.addClass("deletable");
        newEl.find(".delete-btn").on('click', onDelete);
    }
    if (onClick == null)
        newEl.removeClass("clickable");
    else
        newEl.click(onClick);
    console.log(onClick)
    $(newEl).data("schedule", schedule);
    return newEl;
}
