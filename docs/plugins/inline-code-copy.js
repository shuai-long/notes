(function () {
  var DEFAULT_CONFIG = {
    copiedText: "已复制",
    copyText: "点击复制",
    errorText: "复制失败",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.inlineCodeCopy) || {});

  function fallbackCopy(text) {
    var textarea = document.createElement("textarea");

    textarea.value = text;
    textarea.setAttribute("readonly", "readonly");
    textarea.style.left = "-9999px";
    textarea.style.position = "fixed";
    textarea.style.top = "0";
    document.body.appendChild(textarea);
    textarea.select();

    try {
      return document.execCommand("copy");
    } finally {
      textarea.remove();
    }
  }

  function copyText(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      return navigator.clipboard.writeText(text).catch(function () {
        if (fallbackCopy(text)) return;
        throw new Error("Copy failed");
      });
    }

    return new Promise(function (resolve, reject) {
      if (fallbackCopy(text)) {
        resolve();
      } else {
        reject(new Error("Copy failed"));
      }
    });
  }

  function setState(code, state) {
    var label = state === "success" ? config.copiedText : config.errorText;

    code.dataset.copyState = state;
    code.dataset.copyLabel = label;

    window.clearTimeout(code.__inlineCodeCopyTimer);
    code.__inlineCodeCopyTimer = window.setTimeout(function () {
      code.dataset.copyState = "";
      code.dataset.copyLabel = config.copyText;
    }, 1200);
  }

  function enhanceInlineCode(code) {
    if (code.closest("pre") || code.dataset.inlineCodeCopy === "true") return;

    code.classList.add("inline-code-copy");
    code.dataset.copyLabel = config.copyText;
    code.dataset.copyState = "";
    code.dataset.inlineCodeCopy = "true";
    code.setAttribute("role", "button");
    code.setAttribute("tabindex", "0");

    function handleCopy(event) {
      event.preventDefault();
      event.stopPropagation();

      copyText(code.textContent || "")
        .then(function () {
          setState(code, "success");
        })
        .catch(function () {
          setState(code, "error");
        });
    }

    code.addEventListener("click", handleCopy);
    code.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " ") {
        handleCopy(event);
      }
    });
  }

  function enhanceAll() {
    document.querySelectorAll(".markdown-section code").forEach(enhanceInlineCode);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(enhanceAll);
    });
  });
})();
