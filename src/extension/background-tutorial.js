"use strict";

var isFirefox = (typeof (browser) !== "undefined");

var browser;
if (!isFirefox) {
	browser = chrome;
}

browser.runtime.onInstalled.addListener(function(object) {
	if (object.reason != 'install') return;
	var url = browser.extension.getURL('tutorial.html');
	browser.tabs.create({ url: url}, function(tab) {
		console.log("options page opened");
	})
});
