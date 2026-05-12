(function () {
  var DEFAULT_CONFIG = {
    collapseHeight: 420,
    collapseText: "收起代码",
    copyErrorText: "复制失败",
    copySuccessText: "已复制",
    copyText: "复制代码",
    expandText: "展开代码",
    lineNumbers: true,
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
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.codeBlockTools) || {});
  var copyTimer = 0;

  function escapeHtml(text) {
    return String(text || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function getCode(pre) {
    for (var index = 0; index < pre.children.length; index += 1) {
      if (pre.children[index].tagName === "CODE") return pre.children[index];
    }

    return null;
  }

  function getCodeLineCount(code) {
    var text = code.textContent || "";

    if (text.endsWith("\n")) {
      text = text.slice(0, -1);
    }

    return Math.max(text.split("\n").length, 1);
  }

  function getLanguage(pre, code) {
    var className = (code && code.className) || "";
    var classMatch = className.match(/(?:^|\s)(?:lang|language)-([^\s]+)/i);

    return pre.dataset.lang || pre.getAttribute("data-lang") || (classMatch && classMatch[1]) || "code";
  }

  function getLanguageKey(language) {
    return String(language || "").trim().toLowerCase();
  }

  function getCollapseHeight() {
    return Number(config.collapseHeight) || DEFAULT_CONFIG.collapseHeight;
  }

  function getCollapseLineLimit() {
    var configured = Number(config.collapseLines);
    var collapseHeight = getCollapseHeight();

    if (configured > 0) return configured;

    return Math.max(1, Math.floor((collapseHeight - 36) / 22));
  }

  function createLineNumbersHtml(lineCount) {
    var lines = [];
    var index;

    for (index = 1; index <= lineCount; index += 1) {
      lines.push("<span>" + index + "</span>");
    }

    return '<div class="code-line-numbers" aria-hidden="true">' + lines.join("") + "</div>";
  }

  function createCopyButtonHtml() {
    return (
      '<button class="docsify-copy-code-button" type="button" aria-label="' +
      escapeHtml(config.copyText) +
      '" title="' +
      escapeHtml(config.copyText) +
      '">' +
      '<span class="code-copy-icon" aria-hidden="true">' +
      '<svg viewBox="0 0 24 24" focusable="false">' +
      '<rect x="9" y="9" width="10" height="10" rx="2"></rect>' +
      '<path d="M5 15V7a2 2 0 0 1 2-2h8"></path>' +
      "</svg>" +
      "</span>" +
      '<span class="label" aria-hidden="true"></span>' +
      '<span class="error" aria-hidden="true">' +
      escapeHtml(config.copyErrorText) +
      "</span>" +
      '<span class="success" aria-hidden="true">' +
      escapeHtml(config.copySuccessText) +
      "</span>" +
      '<span aria-live="polite"></span>' +
      "</button>"
    );
  }

  function createToggleButtonHtml() {
    return (
      '<button class="code-fold-button" type="button" aria-expanded="false">' +
      escapeHtml(config.expandText) +
      "</button>"
    );
  }

  function wrapCodeBlock(pre) {
    var code = getCode(pre);
    var language;
    var lineCount;
    var shouldCollapse;
    var wrapper;
    var toolbar;
    var languageLabel;
    var body;

    if (!code || pre.closest(".code-block")) return;

    language = getLanguage(pre, code);
    if (IGNORED_LANGUAGES[getLanguageKey(language)]) return;

    lineCount = getCodeLineCount(code);
    shouldCollapse = lineCount > getCollapseLineLimit();

    pre.dataset.codeBlockTools = "true";
    if (!pre.getAttribute("data-lang")) {
      pre.setAttribute("data-lang", language.toUpperCase());
    }

    wrapper = document.createElement("div");
    toolbar = document.createElement("div");
    languageLabel = document.createElement("span");
    body = document.createElement("div");

    wrapper.className = "code-block " + (shouldCollapse ? "is-foldable" : "is-short-code");
    toolbar.className = "code-block-toolbar";
    languageLabel.className = "code-block-language";
    languageLabel.textContent = language || "code";
    body.className = "code-block-body" + (shouldCollapse ? " is-collapsed" : "");
    body.style.setProperty("--code-line-count", String(lineCount));

    if (shouldCollapse) {
      body.style.maxHeight = getCollapseHeight() + "px";
    }

    toolbar.appendChild(languageLabel);
    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(toolbar);
    wrapper.insertAdjacentHTML("beforeend", createCopyButtonHtml());
    wrapper.appendChild(body);

    if (config.lineNumbers) {
      body.insertAdjacentHTML("beforeend", createLineNumbersHtml(lineCount));
    }

    body.appendChild(pre);

    if (shouldCollapse) {
      wrapper.insertAdjacentHTML("beforeend", createToggleButtonHtml());
    }
  }

  function enhanceHtml(html) {
    var container = document.createElement("div");

    container.innerHTML = html;
    container.querySelectorAll("pre").forEach(wrapCodeBlock);

    return container.innerHTML;
  }

  function copyWithFallback(text) {
    var textarea = document.createElement("textarea");
    var success;

    textarea.value = text;
    textarea.setAttribute("readonly", "");
    textarea.style.position = "fixed";
    textarea.style.left = "-9999px";
    textarea.style.top = "0";
    document.body.appendChild(textarea);
    textarea.select();
    success = document.execCommand("copy");
    textarea.remove();

    return success ? Promise.resolve() : Promise.reject(new Error("Copy failed"));
  }

  function copyText(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text).catch(function () {
        return copyWithFallback(text);
      });
    }

    return copyWithFallback(text);
  }

  function setCopyState(button, state, label) {
    var live = button.querySelector("[aria-live]");

    window.clearTimeout(copyTimer);
    button.classList.remove("success", "error");
    button.classList.add(state);

    if (live) live.textContent = label;

    copyTimer = window.setTimeout(function () {
      button.classList.remove(state);
      if (live) live.textContent = "";
    }, 1000);
  }

  function handleCopyClick(button) {
    var wrapper = button.closest(".code-block");
    var code = wrapper && wrapper.querySelector("pre > code");

    if (!code) return;

    copyText(code.textContent || "")
      .then(function () {
        setCopyState(button, "success", config.copySuccessText);
      })
      .catch(function () {
        setCopyState(button, "error", config.copyErrorText);
      });
  }

  function handleFoldClick(button) {
    var wrapper = button.closest(".code-block");
    var body = wrapper && wrapper.querySelector(".code-block-body");
    var expanded = button.getAttribute("aria-expanded") === "true";

    if (!body) return;

    button.setAttribute("aria-expanded", expanded ? "false" : "true");
    button.textContent = expanded ? config.expandText : config.collapseText;
    body.classList.toggle("is-collapsed", expanded);
    body.classList.toggle("is-expanded", !expanded);
    body.style.maxHeight = expanded ? getCollapseHeight() + "px" : "none";
  }

  function handleContentClick(event) {
    if (!event.target || typeof event.target.closest !== "function") return;

    var copyButton = event.target.closest(".docsify-copy-code-button");
    var foldButton;

    if (copyButton) {
      handleCopyClick(copyButton);
      return;
    }

    foldButton = event.target.closest(".code-fold-button");
    if (foldButton) {
      handleFoldClick(foldButton);
    }
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.afterEach(function (html, next) {
      next(enhanceHtml(html));
    });

    hook.mounted(function () {
      var content = document.querySelector(".content");

      if (content && content.dataset.codeBlockToolsMounted !== "true") {
        content.dataset.codeBlockToolsMounted = "true";
        content.addEventListener("click", handleContentClick);
      }
    });
  });
})();
