"use strict";

exports.italic = () => document.execCommand("italic", false);
exports.bold = () => document.execCommand("bold", false);
exports.underline = () => document.execCommand("underline", false)
exports.mkList = () => document.execCommand("insertunorderedlist", false)
exports.mkHeading = (i) => () => document.execCommand("formatBlock", false, i === 0 ? "p" : `<h${i}>`)
