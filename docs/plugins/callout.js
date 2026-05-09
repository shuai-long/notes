(function () {
  var callouts = {
    note: { label: "Note", icon: "i" },
    tip: { label: "Tip", icon: "✓" },
    warning: { label: "Warning", icon: "!" },
    attention: { label: "Attention", icon: "!" },
    important: { label: "Important", icon: "*" },
    caution: { label: "Caution", icon: "!" },
  };
  var markerPattern = /^\s*\[!(note|tip|warning|attention|important|caution)\]\s*/i;

  function removeMarker(blockquote) {
    var walker = document.createTreeWalker(blockquote, NodeFilter.SHOW_TEXT, {
      acceptNode: function (node) {
        if (!node.nodeValue || !node.nodeValue.trim()) return NodeFilter.FILTER_REJECT;
        return NodeFilter.FILTER_ACCEPT;
      },
    });
    var node = walker.nextNode();
    var match;

    if (!node) return null;

    match = node.nodeValue.match(markerPattern);
    if (!match) return null;

    node.nodeValue = node.nodeValue.replace(markerPattern, "");
    return match[1].toLowerCase();
  }

  function removeEmptyLeadingParagraph(blockquote) {
    var first = blockquote.firstElementChild;

    if (!first || first.classList.contains("callout-title")) return;
    if (first.tagName !== "P") return;
    if (first.textContent.trim()) return;
    if (first.querySelector("img, code, a, table, pre")) return;

    first.remove();
  }

  function createTitle(type) {
    var title = document.createElement("div");
    var icon = document.createElement("span");
    var text = document.createElement("span");
    var meta = callouts[type] || callouts.note;

    title.className = "callout-title";
    icon.className = "callout-icon";
    icon.setAttribute("aria-hidden", "true");
    icon.textContent = meta.icon;
    text.textContent = meta.label;
    title.appendChild(icon);
    title.appendChild(text);
    return title;
  }

  function enhanceCallout(blockquote) {
    var type;

    if (blockquote.dataset.calloutEnhanced === "true") return;

    type = removeMarker(blockquote);
    if (!type) return;

    blockquote.dataset.calloutEnhanced = "true";
    blockquote.classList.add("callout", "callout-" + type);
    removeEmptyLeadingParagraph(blockquote);
    blockquote.insertBefore(createTitle(type), blockquote.firstChild);
  }

  function enhanceAll() {
    document.querySelectorAll(".markdown-section blockquote").forEach(enhanceCallout);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(enhanceAll);
    });
  });
})();
