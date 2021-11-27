"use strict";

var isFirefox = (typeof (browser) !== "undefined");

var browser;
if (!isFirefox) {
	browser = chrome;
}

browser.runtime.onInstalled.addListener(function(object) {
	var url;
	switch (object.reason) {
		case 'install':
			url = tutorialUrl();
			break;
		case 'update':
			url = changelogUrl(object.previousVersion);
			break;
		default:
			return;
	}
	browser.tabs.create({ url: url }, function(_tab) {})
});

function tutorialUrl() {
	return browser.extension.getURL('sites/tutorial/tutorial.html');
}

function changelogUrl(previousVersion) {
	return browser.extension.getURL(`sites/changelog/changelog.html?from=${previousVersion}`);
}
