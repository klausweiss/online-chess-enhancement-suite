"use strict";

var fetchFromPage;
if (typeof (content) !== "undefined" && typeof (content.fetch) !== "undefined") {
  fetchFromPage = content.fetch;
} else {
  fetchFromPage = fetch;
}
// TODO: Firefox support (original scripts aren't loaded in Chrome, but are in Firefox)


function allowOCES(text) {
  let ostrichRe = /\!\w+\..strus.../i;
  let modifiedText = text.replace(ostrichRe, (_match, _offset, _string, _groups) => {
    return `(false)`;
  });
  return script(modifiedText);
}

function rm(node) {
  let parent = node.parentNode;
  parent.removeChild(node);
}

function load(node) {
  document.body.appendChild(node);
}

function script(text) {
  let scriptNode = document.createElement("script");
  scriptNode.innerHTML = text;
  return mark(scriptNode);
}

function css(text) {
  let styleNode = document.createElement("style");
  styleNode.innerHTML = text;
  return styleNode;
}

function hasScripts(mutation) {
  return mutation.addedNodes[0] &&
    mutation.addedNodes[0].tagName &&
    mutation.addedNodes[0].tagName.toLowerCase() === "script";
}

function isExternal(node) {
  return node.hasAttribute("src");
}

function mark(node) {
  node.setAttribute("oces", 1);
  return node;
}

function isMarked(node) {
  return node.getAttribute("oces") == 1;
}

function windowOnLoad() {
  load(css(".oces-highlighted { background-color: #89dae7; }"));
  load(script("window.onload();"));
}

exports.enablePlugin = function() {
  var nonces = 0;
  let promises = [];
  let afterScriptsProcessed = function() {
    Promise.all(promises)
      .then((result) => {
        result.forEach((scriptNode) => {
          load(scriptNode);
        });
        windowOnLoad();
      });
  };
  let observer = new MutationObserver((mutations, observer) => {
    mutations.forEach((mutation) => {
      if (hasScripts(mutation)) {
        let scriptNode = mutation.addedNodes[0];
        if (isMarked(scriptNode)) return;
        rm(scriptNode);
        var promise;
        if (isExternal(scriptNode)) {
          promise = fetchFromPage(scriptNode.src)
            .then((r) => r.text())
            .then((t) => allowOCES(t));
        }
        else {
          promise = new Promise((ok, _fail) => ok(mark(scriptNode)));
        }
        promises.push(promise);
        if (scriptNode.getAttribute("nonce") != null) {
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

