(function () {
  "use strict";

  const CJK_RANGE =
    "\\u2e80-\\u2eff\\u2f00-\\u2fdf\\u3040-\\u30ff\\u3100-\\u312f\\u31a0-\\u31bf\\u3400-\\u4dbf\\u4e00-\\u9fff\\uf900-\\ufaff";
  const LATIN_RANGE = "A-Za-z0-9";
  const cjkLatinRE = new RegExp("([" + CJK_RANGE + "])([" + LATIN_RANGE + "])", "g");
  const latinCjkRE = new RegExp("([" + LATIN_RANGE + "])([" + CJK_RANGE + "])", "g");
  const ignoredSelector = [
    "pre",
    "code",
    "script",
    "style",
    "textarea",
    "kbd",
    "samp",
    "svg",
    "math",
  ].join(",");

  function schedule(task) {
    if ("requestIdleCallback" in window) {
      requestIdleCallback(task, { timeout: 800 });
      return;
    }

    setTimeout(task, 0);
  }

  function spacingText(text) {
    return text.replace(cjkLatinRE, "$1 $2").replace(latinCjkRE, "$1 $2");
  }

  function shouldSkip(node, root) {
    const parent = node.parentElement;
    return !parent || parent === root || Boolean(parent.closest(ignoredSelector));
  }

  function fallbackSpacing(root) {
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
      acceptNode(node) {
        if (shouldSkip(node, root) || !node.nodeValue.trim()) {
          return NodeFilter.FILTER_REJECT;
        }

        return NodeFilter.FILTER_ACCEPT;
      },
    });

    const nodes = [];
    while (walker.nextNode()) {
      nodes.push(walker.currentNode);
    }

    nodes.forEach((node) => {
      const nextValue = spacingText(node.nodeValue);
      if (nextValue !== node.nodeValue) {
        node.nodeValue = nextValue;
      }
    });
  }

  function spacing(root) {
    if (!root) return;

    if (window.pangu && typeof window.pangu.spacingNode === "function") {
      window.pangu.spacingNode(root);
      return;
    }

    fallbackSpacing(root);
  }

  function plugin(hook, vm) {
    const config = Object.assign(
      {
        selector: ".markdown-section",
      },
      (vm.config && vm.config.spacing) || {}
    );

    hook.doneEach(function () {
      if (config.enabled === false) return;

      schedule(function () {
        document.querySelectorAll(config.selector).forEach(spacing);
      });
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
