"use strict";

function allowOCES(text, parent) {
  let ostrichRe = /\!\w+\..strus.../i;
	let ostrich = text.search(ostrichRe);
	let shouldAllow = ostrich > -1;
  let newScript = document.createElement("script");
  newScript.innerHTML = text.replace(ostrichRe, (match, offset, string, groups) => {
    let target = match.match(/\!(\w+)\./)[1];
    return `(false)`;
  });
  return newScript;
}


exports.enablePlugin = function () {
  let observer = new MutationObserver((mutations, observer) => {
    let promises = [];
    mutations.forEach((mutation) => {
      if (
        mutation.addedNodes[0] &&
        mutation.addedNodes[0].tagName &&
        mutation.addedNodes[0].tagName.toLowerCase() === 'script'
      ) {
        let script = mutation.addedNodes[0];
        let parent = script.parentNode;
        if (script.hasAttribute("src")) {
          parent.removeChild(script);
          console.log(`loading ${script.src}`);
          let promise = fetch(script.src)
            .then((response) => response.text())
            .then((text) => {
              let newScript = allowOCES(text, parent);
              return [newScript, script, parent];
            });
          promises.push(promise);
        }
      }
    });
    Promise.all(promises)
      .then((result) => {
        result.forEach((r) => {
          const [newScript, script, parent] = r;
          // TODO: resolve race condition
          parent.appendChild(newScript);
          observer.disconnect();
          console.log(`loaded ${script.src}`);
        });
      });
  });
  observer.observe(document.documentElement, {
    childList: true,
    subtree: true,
  });
};

