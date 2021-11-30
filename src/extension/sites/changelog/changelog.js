window.onload = function() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  const from = urlParams.get("from") || "0.0.0";

  var seenAllRelevantChanges = false;
  var shouldRemoveUntilNextH2 = false;
  document.querySelectorAll("body *").forEach((el) => {
    seenAllRelevantChanges |= (el.firstElementChild && el.firstElementChild.text == from);
    if (el.tagName == "H2") {
      if (el.nextElementSibling && el.nextElementSibling.querySelector("a[href='#ignore-in-changelog-within-extension']")) {
        shouldRemoveUntilNextH2 = true;
      } else {
        shouldRemoveUntilNextH2 = false;
      }
    }
    if (seenAllRelevantChanges || shouldRemoveUntilNextH2) el.remove();
  });

  if (!document.querySelector("h2")) {
    // no headers left
    window.close();
  }

  document.querySelector("h1").innerText = document.title;
};
