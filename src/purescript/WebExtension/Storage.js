"use strict";

var syncArea, localArea;
var storageAreaActionPromise;

if (typeof (browser) !== "undefined") {
	// firefox
	storageAreaActionPromise = function(area, action, param) {
		// common interface with chrome
		return area[action](param);
	}
} else {
	// chrome
	var browser = chrome;

	storageAreaActionPromise = function(area, action, param) {
		// Polyfill for area.get("key") not returning promise in Chrome.
		// It works with manifest V3, but we can't have this at this point.
		return new Promise((resolve, _reject) => {
			area[action](param, function(...args) {
				console.log("resolving ", action);
				resolve(...args);
			});
		});
	}
}

syncArea = browser.storage.sync;
localArea = browser.storage.local;

function injectStorageArea(f) {
	return sync => local => area => {
		if (area == sync) {
			return f(syncArea);
		} else if (area == local) {
			return f(localArea);
		} else {
			throw new Exception('unknown storage area');
		};
	}
};

exports.getImpl = injectStorageArea(
	storageArea => key =>
		storageAreaActionPromise(storageArea, "get", key).then((v) => v[key]));

exports.setImpl = injectStorageArea(
	storageArea => key => value => {
		let obj = {};
		obj[key] = value;
		return storageAreaActionPromise(storageArea, "set", obj);
	});

exports.removeImpl = injectStorageArea(
	storageArea => key => storageAreaActionPromise(storageArea, "remove", key));
