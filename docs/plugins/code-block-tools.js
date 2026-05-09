(function () {
  var DEFAULT_CONFIG = {
    collapseHeight: 420,
    expandText: "展开代码",
    collapseText: "收起代码",
    lineNumbers: true,
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.codeBlockTools) || {});

  function getCodeLineCount(code) {
    var text = code.textContent || "";

    if (text.endsWith("\n")) {
      text = text.slice(0, -1);
    }

    return Math.max(text.split("\n").length, 1);
  }

  function createLineNumbers(lineCount) {
    var gutter = document.createElement("div");
    var fragment = document.createDocumentFragment();

    gutter.className = "code-line-numbers";
    gutter.setAttribute("aria-hidden", "true");

    for (var index = 1; index <= lineCount; index += 1) {
      var line = document.createElement("span");

      line.textContent = index;
      fragment.appendChild(line);
    }

    gutter.appendChild(fragment);
    return gutter;
  }

  function getLanguage(pre) {
    return pre.dataset.lang || pre.getAttribute("data-lang") || "";
  }

  function createToolbar(pre) {
    var toolbar = document.createElement("div");
    var language = document.createElement("span");
    var langText = getLanguage(pre);

    toolbar.className = "code-block-toolbar";
    language.className = "code-block-language";
    language.textContent = langText || "code";
    toolbar.appendChild(language);

    return toolbar;
  }

  function findCopyButton(pre) {
    for (var index = 0; index < pre.children.length; index += 1) {
      if (pre.children[index].classList.contains("docsify-copy-code-button")) {
        return pre.children[index];
      }
    }

    return null;
  }

  function useIconCopyButton(button) {
    var label = button.querySelector(".label");

    if (button.dataset.iconOnly === "true") return;

    button.dataset.iconOnly = "true";
    button.setAttribute("aria-label", "复制代码");
    button.setAttribute("title", "复制代码");

    if (label) {
      label.textContent = "";
      label.setAttribute("aria-hidden", "true");
    }

    button.insertAdjacentHTML(
      "afterbegin",
      '<span class="code-copy-icon" aria-hidden="true">' +
        '<svg viewBox="0 0 24 24" focusable="false">' +
        '<rect x="9" y="9" width="10" height="10" rx="2"></rect>' +
        '<path d="M5 15V7a2 2 0 0 1 2-2h8"></path>' +
        "</svg>" +
        "</span>"
    );
  }

  function moveCopyButton(pre, wrapper) {
    var copyButton = findCopyButton(pre);

    if (!copyButton) return;

    useIconCopyButton(copyButton);
    wrapper.appendChild(copyButton);
  }

  function createToggleButton(body) {
    var button = document.createElement("button");

    button.className = "code-fold-button";
    button.type = "button";
    button.setAttribute("aria-expanded", "false");
    button.textContent = config.expandText;
    button.addEventListener("click", function () {
      var expanded = button.getAttribute("aria-expanded") === "true";

      button.setAttribute("aria-expanded", expanded ? "false" : "true");
      button.textContent = expanded ? config.expandText : config.collapseText;
      body.classList.toggle("is-collapsed", expanded);
      body.classList.toggle("is-expanded", !expanded);
      body.style.maxHeight = expanded ? config.collapseHeight + "px" : "none";
    });

    return button;
  }

  function enhanceCodeBlock(pre) {
    var existingWrapper = pre.closest(".code-block");

    if (existingWrapper) {
      moveCopyButton(pre, existingWrapper);
      return;
    }

    if (pre.dataset.codeBlockTools === "true") return;

    var code = null;

    for (var index = 0; index < pre.children.length; index += 1) {
      if (pre.children[index].tagName === "CODE") {
        code = pre.children[index];
        break;
      }
    }

    if (!code) return;

    var wrapper = document.createElement("div");
    var toolbar = createToolbar(pre);
    var body = document.createElement("div");

    wrapper.className = "code-block";
    body.className = "code-block-body";
    pre.dataset.codeBlockTools = "true";

    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(toolbar);
    moveCopyButton(pre, wrapper);
    wrapper.appendChild(body);

    if (config.lineNumbers) {
      body.appendChild(createLineNumbers(getCodeLineCount(code)));
    }

    body.appendChild(pre);

    window.requestAnimationFrame(function () {
      var collapseHeight = Number(config.collapseHeight) || DEFAULT_CONFIG.collapseHeight;
      var shouldCollapse = body.scrollHeight > collapseHeight + 8;

      moveCopyButton(pre, wrapper);

      if (!shouldCollapse) {
        wrapper.classList.add("is-short-code");
        return;
      }

      wrapper.classList.add("is-foldable");
      body.classList.add("is-collapsed");
      body.style.maxHeight = collapseHeight + "px";
      wrapper.appendChild(createToggleButton(body));
    });
  }

  function enhanceAll() {
    document.querySelectorAll(".markdown-section pre").forEach(enhanceCodeBlock);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(enhanceAll);
      });
    });
  });
})();
