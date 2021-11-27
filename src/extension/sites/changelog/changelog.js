window.onload = function() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  if (!urlParams.has("from")) return;
  const from = urlParams.get("from");

  var seenAllRelevantChanges = false;
  document.querySelectorAll("body *").forEach((el) => {
    seenAllRelevantChanges |= (el.firstElementChild && el.firstElementChild.text == from);
    if (seenAllRelevantChanges) el.remove();
  });

  document.querySelector("h1").innerText = document.title;
};
