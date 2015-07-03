/* global exports */
"use strict";

// module Main

exports.getValue = function(e) {
    return e.target.value;
};

exports.getChecked = function(e) {
    return e.target.checked;
};

exports.getKeyCode = function(e) {
    return e.keyCode;
};