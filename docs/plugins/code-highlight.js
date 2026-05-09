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
    audio: true,
    embed: true,
    iframe: true,
    media: true,
    mermaid: true,
    pdf: true,
    video: true,
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

  function highlightCode(code) {
    var language = readLanguage(code);
    var grammar;

    if (!language || IGNORED_LANGUAGES[language]) return false;

    setLanguage(code, language);
    grammar = window.Prism && window.Prism.languages && window.Prism.languages[language];

    if (!grammar || typeof window.Prism.highlight !== "function") return false;

    code.innerHTML = window.Prism.highlight(code.textContent || "", grammar, language);
    code.dataset.highlighted = "true";

    return true;
  }

  function highlightHtml(html) {
    var container;

    if (!window.Prism || typeof window.Prism.highlightElement !== "function") return;

    container = document.createElement("div");
    container.innerHTML = html;

    container.querySelectorAll("pre > code").forEach(highlightCode);

    return container.innerHTML;
  }

  function highlightRemaining() {
    if (!window.Prism || typeof window.Prism.highlightElement !== "function") return;

    document.querySelectorAll(".markdown-section pre > code:not([data-highlighted='true'])").forEach(function (code) {
      if (highlightCode(code)) return;

      window.Prism.highlightElement(code);
    });
  }

  if (window.Prism && window.Prism.plugins && window.Prism.plugins.autoloader) {
    window.Prism.plugins.autoloader.languages_path =
      config.languagesPath || "./vendor/prism/components/";
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.afterEach(function (html, next) {
      next(highlightHtml(html) || html);
    });

    hook.doneEach(highlightRemaining);
  });
})();
