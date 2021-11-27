"use strict";

var isFirefox = (typeof (browser) !== "undefined");
var isChrome = !isFirefox;

if (isChrome) {
	chrome.tabs.onUpdated.addListener(function(tabId, changeInfo, tab) {
		if (tab.url && tab.url.match(/https:\/\/lichess\.org/)) {
			chrome.pageAction.show(tabId);
		}
	});
}
