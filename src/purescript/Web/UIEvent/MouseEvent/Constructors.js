"use strict";

exports.makeMouseEvent = function (eventType) {
  return function (clientX) {
    return function (clientY) { 
      var ev = new MouseEvent(eventType, {
        clientX: clientX,
        clientY: clientY,
        bubbles: true,
        cancelable: true,
      });
      return ev;
    }
  }
};


