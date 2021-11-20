"use strict";


exports.keyPressedFFI =
	function(constant) {
		return function(keyCode) {
			return function() {
				var out = constant(false);
				window.addEventListener("keydown", function(e) {
					if (e.keyCode === keyCode) { // TODO: 'keyCode' is deprecated
						let keyEvent = e;
						out.set(keyEvent);
					}
				});
				window.addEventListener("keyup", function(e) {
					if (e.keyCode === keyCode) {
						let keyEvent = e;
						out.set(keyEvent);
					}
				});
				return out;
			};
		};
	};

exports.noopEvent = function() {
	return new KeyboardEvent("keyup", { keyCode: -1 });
};
exports.preventDefault =
	function(keyEvent) { return function() { keyEvent.preventDefault(); } };
exports.isKeyDown = function(keyEvent) { return keyEvent.type == "keydown" };
exports.isKeyUp = function(keyEvent) { return keyEvent.type == "keyup" };
exports.keycode = function(keyEvent) { return keyEvent.keyCode; };
