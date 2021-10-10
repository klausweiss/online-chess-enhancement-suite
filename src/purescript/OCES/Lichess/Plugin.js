"use strict";

function allowOCES(text) {
  let ostrichRe = /\!\w+\..strus.../i;
	let ostrich = text.search(ostrichRe);
	let shouldAllow = ostrich > -1;
  let modifiedText = text.replace(ostrichRe, (match, offset, string, groups) => {
    let target = match.match(/\!(\w+)\./)[1];
    return `(false)`;
  });
  return mk(modifiedText);
}

function rm(script) {
  let parent = script.parentNode;
  parent.removeChild(script);
}

function load(script) {
  document.body.appendChild(script);
}

function mk(text, defer) {
  let script = document.createElement("script");
  if (defer != undefined) script.setAttribute("defer", defer);
  script.innerHTML = text;
  return mark(script);
}

function hasScripts(mutation) {
  return mutation.addedNodes[0] && 
    mutation.addedNodes[0].tagName && 
    mutation.addedNodes[0].tagName.toLowerCase() === "script";
}

function isExternal(script) {
  return script.hasAttribute("src");
}

function mark(script) {
  script.setAttribute("oces", 1);
  return script;
}

function isMarked(script) {
  return script.getAttribute("oces") == 1;
}

function windowOnLoad() {
  load(mk("window.onload();"));
}

exports.enablePlugin = function () {
  var nonces = 0;
  let promises = [];
  let afterScriptsProcessed = function() {
    Promise.all(promises)
    .then((result) => {
      result.forEach((script) => {
        load(script);
      });
      windowOnLoad();
    });
  };
  let observer = new MutationObserver((mutations, observer) => {
    mutations.forEach((mutation) => {
      if (hasScripts(mutation)) {
        let script = mutation.addedNodes[0];
        if (isMarked(script)) return;
        rm(script);
        var promise;
        if (isExternal(script)) {
          promise = fetch(script.src)
            .then((r) => r.text())
            .then((t) => {
              return allowOCES(t);
            });
        }
        else {
          promise = new Promise((ok, fail) => ok(mark(script)));
        }
        promises.push(promise);
        if (script.getAttribute("nonce") != null) {
          nonces += 1;
          if (nonces == 2) {  // the last script on page is the second script with a "nonce" attribute
            observer.disconnect();
            afterScriptsProcessed();
          }
        }
      }
    });
  });
  observer.observe(document.documentElement, {
    childList: true,
    subtree: true,
  });
};

