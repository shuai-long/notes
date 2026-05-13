(function () {
  var DEFAULT_CONFIG = {
    selector: ".markdown-section, .sidebar",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.cjkSpacing) || {});
  var skipSelector = [
    "code",
    "pre",
    "kbd",
    "samp",
    "script",
    "style",
    "textarea",
    "svg",
    "math",
    ".no-cjk-spacing",
  ].join(",");
  var cjk = "\\u2E80-\\u2EFF\\u2F00-\\u2FDF\\u3040-\\u30FF\\u3100-\\u312F\\u31A0-\\u31BF\\u3400-\\u4DBF\\u4E00-\\u9FFF\\uF900-\\uFAFF";
  var latin = "A-Za-z";
  var digit = "0-9\\uFF10-\\uFF19";
  var alnum = latin + digit;
  var cjkBeforeAlnum = new RegExp("([" + cjk + "])([" + alnum + "])", "g");
  var alnumBeforeCjk = new RegExp("([" + alnum + "])([" + cjk + "])", "g");
  var punctuationAfterCode = /^[,.;:!?，。；：！？、)\]}）】》]/;

  function shouldSkip(node) {
    var parent = node.parentElement;

    return !parent || parent.closest(skipSelector);
  }

  function addSpacing(text) {
    return text.replace(cjkBeforeAlnum, "$1 $2").replace(alnumBeforeCjk, "$1 $2");
  }

  function spacingElement(root) {
    var walker;
    var node;
    var nextText;

    if (!root) return;

    walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
      acceptNode: function (textNode) {
        if (!textNode.nodeValue || !textNode.nodeValue.trim()) return NodeFilter.FILTER_REJECT;
        if (shouldSkip(textNode)) return NodeFilter.FILTER_REJECT;

        return NodeFilter.FILTER_ACCEPT;
      },
    });

    while ((node = walker.nextNode())) {
      nextText = addSpacing(node.nodeValue);

      if (nextText !== node.nodeValue) {
        node.nodeValue = nextText;
      }
    }
  }

  function needsSpaceBeforeCode(code) {
    var node = code.previousSibling;
    var text;

    if (!node || node.nodeType !== Node.TEXT_NODE) return false;

    text = node.nodeValue || "";
    return text.length > 0 && !/\s$/.test(text);
  }

  function needsSpaceAfterCode(code) {
    var node = code.nextSibling;
    var text;

    if (!node || node.nodeType !== Node.TEXT_NODE) return false;

    text = node.nodeValue || "";
    return text.length > 0 && !/^\s/.test(text) && !punctuationAfterCode.test(text);
  }

  function spacingInlineCodes(root) {
    if (!root) return;

    root.querySelectorAll("code").forEach(function (code) {
      if (code.closest("pre")) return;

      if (needsSpaceBeforeCode(code)) {
        code.parentNode.insertBefore(document.createTextNode(" "), code);
      }

      if (needsSpaceAfterCode(code)) {
        code.parentNode.insertBefore(document.createTextNode(" "), code.nextSibling);
      }
    });
  }

  function spacingAll() {
    document.querySelectorAll(config.selector).forEach(function (root) {
      spacingElement(root);
      spacingInlineCodes(root);
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(spacingAll);
    });
  });
})();
