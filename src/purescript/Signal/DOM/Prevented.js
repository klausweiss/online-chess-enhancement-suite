"use strict";


exports.keyPressedPWithPrevent =
  function keyPressedPWithPrevent(constant) {
    return function(preventDefault) {
			return function(keyCode) {
				return function() {
					var out = constant(false);
					window.addEventListener("keydown", function(e) {
						if (e.keyCode === keyCode) {
							out.set(true);
							if (preventDefault) e.preventDefault();
						}
					});
					window.addEventListener("keyup", function(e) {
						if (e.keyCode === keyCode) {
							out.set(false); 
							if (preventDefault) e.preventDefault();
						}
					});
					return out;
				};
			};
    };
  };

