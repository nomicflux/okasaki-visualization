"use strict";

exports.highlightCode = function(data) {
    var hlCode = hljs.highlight(data.value0, data.value1);
    return hlCode.value;
};
