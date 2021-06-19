"use strict";

exports.getOffset = () => {
  var sel, range;
  if (window.getSelection) {
    // IE9+ and other browsers
    sel = window.getSelection();
    if (sel.rangeCount > 0) {
      return sel.focusOffset;
    }
  } else if ( (sel = window.document.selection) ) {
    // IE <= 8
    if (sel.type != "Control") {
      range = sel.createRange();
      return range.endOffset;
    }
  }
};

exports.setOffset = (i) => () => {
  var sel, range;
  if (window.getSelection) {
    // IE9+ and other browsers
    sel = window.getSelection();
    if (sel.rangeCount > 0) {
      var textNode = sel.focusNode;
      var newOffset = i;
      sel.collapse(textNode, Math.min(textNode.length, newOffset));
    }
  } else if ( (sel = window.document.selection) ) {
    // IE <= 8
    if (sel.type != "Control") {
      range = sel.createRange();
      range.move("character", i);
      range.select();
    }
  }
};

exports.shiftOffset = (i) => () => {
  var sel, range;
  if (window.getSelection) {
    // IE9+ and other browsers
    sel = window.getSelection();
    if (sel.rangeCount > 0) {
      var textNode = sel.focusNode;
      var newOffset = sel.focusOffset + i;
      sel.collapse(textNode, Math.min(textNode.length, newOffset));
    }
  } else if ( (sel = window.document.selection) ) {
    // IE <= 8
    if (sel.type != "Control") {
      range = sel.createRange();
      range.move("character", i);
      range.select();
    }
  }
};
