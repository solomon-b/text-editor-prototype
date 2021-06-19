"use strict";

exports.getValue = function (e) {
  return {id: e.target.id, content: e.target.textContent};
};

exports.insertDiv = (r) => (i) => () => {
  let div = document.createElement("p");
  div.setAttribute("contenteditable", true);
  div.setAttribute("id", i);
  div.setAttribute("class", "content");

  let text = document.createTextNode("This is a new row");
  div.appendChild(text);

  let currEl = document.getElementById(r);
  let editor = currEl.parentNode;

  editor.insertBefore(div, currEl.nextSibling);;
};
