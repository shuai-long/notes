(function () {
  var DEFAULT_ALIASES = {
    ABAP: "abap",
    abap: "abap",
    bat: "batch",
    cmd: "batch",
    html: "markup",
    js: "javascript",
    sh: "bash",
    shell: "bash",
    xml: "markup",
  };
  var IGNORED_LANGUAGES = {
    mermaid: true,
    pdf: true,
  };
  var config = (window.$docsify && window.$docsify.codeHighlight) || {};
  var aliases = Object.assign({}, DEFAULT_ALIASES, config.aliases || {});

  function normalizeLanguage(language) {
    var raw = (language || "").trim();

    if (!raw) return "";

    return aliases[raw] || aliases[raw.toLowerCase()] || raw.toLowerCase();
  }

  function readLanguage(code) {
    var className = code.className || "";
    var classMatch = className.match(/(?:^|\s)(?:lang|language)-([^\s]+)/i);
    var dataLang = code.parentElement && code.parentElement.getAttribute("data-lang");

    return normalizeLanguage((classMatch && classMatch[1]) || dataLang || "");
  }

  function setLanguage(code, language) {
    var classNames = (code.className || "")
      .split(/\s+/)
      .filter(function (name) {
        return name && !/^(?:lang|language)-/i.test(name);
      });
    var pre = code.parentElement;

    classNames.push("language-" + language);
    code.className = classNames.join(" ");

    if (pre && pre.tagName === "PRE") {
      pre.classList.add("language-" + language);
      pre.setAttribute("data-lang", language.toUpperCase());
    }
  }

  function highlight() {
    if (!window.Prism || typeof window.Prism.highlightElement !== "function") return;

    document.querySelectorAll(".markdown-section pre > code").forEach(function (code) {
      var language = readLanguage(code);

      if (!language || IGNORED_LANGUAGES[language]) return;

      setLanguage(code, language);
      window.Prism.highlightElement(code);
    });
  }

  if (window.Prism && window.Prism.plugins && window.Prism.plugins.autoloader) {
    window.Prism.plugins.autoloader.languages_path =
      config.languagesPath || "//cdn.jsdelivr.net/npm/prismjs@1.29.0/components/";
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(highlight);
    });
  });
})();
