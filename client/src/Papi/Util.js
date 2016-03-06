"use strict";

var serialize = require('form-serialize')

// module Papi.Util

exports.getLoginCredentials = function (ev) {
    var fields = serialize(ev.target.form, { hash: true })
    return {
        username: fields.username || "",
        password: fields.password || ""
    }
}
