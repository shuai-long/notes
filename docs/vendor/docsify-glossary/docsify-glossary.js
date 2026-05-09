(function () {
  var GLOSSARY_PATH = "_glossary.md";
  var SKIP_SELECTOR = [
    "a",
    "code",
    "pre",
    "kbd",
    "samp",
    "script",
    "style",
    "textarea",
    "svg",
    ".mermaid",
    ".docs-file-embed",
    ".docs-media-player",
  ].join(",");

  function escapeRegExp(text) {
    return String(text).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  function slugify(term) {
    return encodeURIComponent(String(term).trim().toLowerCase().replace(/\s+/g, "-"));
  }

  function parseGlossary(markdown) {
    return String(markdown || "")
      .split(/\r?\n/)
      .map(function (line) {
        var match = line.match(/^#####\s+(.+)$/);
        return match && match[1].trim();
      })
      .filter(Boolean)
      .sort(function (a, b) {
        return b.length - a.length;
      });
  }

  function shouldSkip(node) {
    var parent = node.parentElement;

    return !parent || parent.closest(SKIP_SELECTOR);
  }

  function createPattern(terms) {
    return new RegExp("(^|[^A-Za-z0-9_\\u4e00-\\u9fff])(" + terms.map(escapeRegExp).join("|") + ")(?=$|[^A-Za-z0-9_\\u4e00-\\u9fff])", "gi");
  }

  function linkTextNode(node, pattern) {
    var text = node.nodeValue;
    var fragment;
    var index = 0;
    var match;

    pattern.lastIndex = 0;
    while ((match = pattern.exec(text))) {
      if (!fragment) fragment = document.createDocumentFragment();

      fragment.appendChild(document.createTextNode(text.slice(index, match.index) + match[1]));

      var link = document.createElement("a");
      link.className = "docsify-glossary-link";
      link.href = "#/" + GLOSSARY_PATH.replace(/\.md$/, "") + "?id=" + slugify(match[2]);
      link.textContent = match[2];
      fragment.appendChild(link);

      index = match.index + match[0].length;
    }

    if (!fragment) return;

    fragment.appendChild(document.createTextNode(text.slice(index)));
    node.parentNode.replaceChild(fragment, node);
  }

  function linkTerms(html, terms) {
    var container = document.createElement("div");
    var pattern;
    var walker;
    var nodes = [];

    if (!terms.length) return html;

    pattern = createPattern(terms);
    container.innerHTML = html;

    walker = document.createTreeWalker(container, NodeFilter.SHOW_TEXT, {
      acceptNode: function (node) {
        if (!node.nodeValue || !node.nodeValue.trim()) return NodeFilter.FILTER_REJECT;
        if (shouldSkip(node)) return NodeFilter.FILTER_REJECT;
        pattern.lastIndex = 0;
        return pattern.test(node.nodeValue) ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT;
      },
    });

    while (walker.nextNode()) {
      nodes.push(walker.currentNode);
    }

    nodes.forEach(function (node) {
      linkTextNode(node, pattern);
    });

    return container.innerHTML;
  }

  function loadTerms() {
    if (window.$docsify.glossaryTerms) {
      return Promise.resolve(window.$docsify.glossaryTerms);
    }

    return fetch(GLOSSARY_PATH)
      .then(function (response) {
        if (!response.ok) return "";
        return response.text();
      })
      .then(function (markdown) {
        window.$docsify.glossaryTerms = parseGlossary(markdown);
        return window.$docsify.glossaryTerms;
      })
      .catch(function () {
        window.$docsify.glossaryTerms = [];
        return window.$docsify.glossaryTerms;
      });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
    hook.afterEach(function (html, next) {
      var route = (vm.route && vm.route.path) || window.location.hash;

      if (/_glossary(?:\.md)?/.test(route)) {
        next(html);
        return;
      }

      loadTerms().then(function (terms) {
        next(linkTerms(html, terms));
      });
    });
  });
})();
