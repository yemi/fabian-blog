"use strict";

var serialize = require('form-serialize')

// module Main

exports.getCredentials = function (ev) {
  var fields = serialize(ev.target.form, { hash: true })
  return {
    username: fields.username || "",
    password: fields.password || ""
  }
}
