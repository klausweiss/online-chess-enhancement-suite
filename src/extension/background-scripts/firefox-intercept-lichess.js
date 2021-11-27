"use strict";

var isFirefox = (typeof (browser) !== "undefined");

function interceptTraffic(details) {
  let filter = browser.webRequest.filterResponseData(details.requestId);
  let decoder = new TextDecoder("utf-8");
  let encoder = new TextEncoder();
  let ostrichRe = /\!\w+\..strus.../i;

  filter.ondata = event => {
    let str = decoder.decode(event.data, { stream: true });
    str = str.replace(ostrichRe, '(false)');
    filter.write(encoder.encode(str));
  }
  filter.onstop = _event => {
    filter.disconnect();
  }

  return {};
}

function enablePlugin() {
  browser.webRequest.onBeforeRequest.addListener(
    interceptTraffic,
    { urls: ["*://lichess.org/*", "*://lichess1.org/*"], types: ["script"] },
    ["blocking"]
  );
};

if (isFirefox) {
  enablePlugin();
}
