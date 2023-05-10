function errorMsg() {
    return $(".error-msg");
}

function form() {
    return $("form#submit-form");
}

$(document).ready(function() {
    errorMsg().fadeOut(0);
    let valid=false;

    form().on("submit", async function(event) {
        if (valid)
            return true;
        errorMsg().fadeOut();
        event.preventDefault();

        const data = getFormData($(this));
        if (isRegistrationForm() && !validatePasswordConfirmation(data))
            return false;

        console.log(data)
        const res = await post(this.action, data, {isJson: false});
        if (res.validationFail){
            errorMsg().text(res.validationFail.message);
            errorMsg().fadeIn();
            return false;
        }
        valid = true;
        form().submit();
        return false;
    });
});

function getFormData($form){
    let unindexed_array = $form.serializeArray();
    let indexed_array = {};

    $.map(unindexed_array, function(n, i){
        indexed_array[n['name']] = n['value'];
    });

    return indexed_array;
}

function isRegistrationForm() {
    return form().hasClass("register");
}


function validatePasswordConfirmation(data) {
    if (data.password === data['confirm-password']) {
        delete data['confirm-password'];
        return true;
    }
    errorMsg().text("Password does not match");
    errorMsg().fadeIn();

    return false;
}