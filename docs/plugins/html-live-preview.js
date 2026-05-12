(function () {
  var DEFAULT_CONFIG = {
    copyErrorText: "复制失败",
    copySuccessText: "已复制",
    copyText: "复制 HTML",
    debounce: 120,
    editorLabel: "HTML",
    languages: ["html", "htm"],
    previewText: "运行预览",
    refreshText: "刷新预览",
    sandbox: "allow-scripts allow-forms allow-modals allow-popups",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.htmlLivePreview) || {});
  var htmlLanguages = {};
  var previewTimers = new WeakMap();
  var copyTimer = 0;

  (config.languages || DEFAULT_CONFIG.languages).forEach(function (language) {
    htmlLanguages[normalizeLanguage(language)] = true;
  });

  function normalizeLanguage(language) {
    return String(language || "")
      .trim()
      .toLowerCase()
      .replace(/^(?:lang|language)-/, "");
  }

  function escapeHtml(text) {
    return String(text || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function readSourceLanguage(pre, code) {
    var className = (code && code.className) || "";
    var classMatch = className.match(/(?:^|\s)(?:lang|language)-([^\s]+)/i);

    return (
      (pre && pre.dataset && pre.dataset.sourceLang) ||
      (code && code.dataset && code.dataset.sourceLang) ||
      (pre && pre.getAttribute("data-lang")) ||
      (classMatch && classMatch[1]) ||
      ""
    );
  }

  function isHtmlBlock(pre, code) {
    return htmlLanguages[normalizeLanguage(readSourceLanguage(pre, code))] === true;
  }

  function highlightSource(source) {
    var grammar = window.Prism && window.Prism.languages && window.Prism.languages.markup;

    if (grammar && typeof window.Prism.highlight === "function") {
      return window.Prism.highlight(source || "", grammar, "markup");
    }

    return escapeHtml(source);
  }

  function buildPreviewDocument(source) {
    var baseHref = (document && document.baseURI) || window.location.href;
    var hasDocumentShell = /<!doctype|<html[\s>]|<head[\s>]|<body[\s>]/i.test(source);

    if (hasDocumentShell) return source;

    return (
      "<!doctype html>" +
      '<html lang="zh-CN">' +
      "<head>" +
      '<meta charset="UTF-8">' +
      '<base href="' +
      escapeHtml(baseHref) +
      '">' +
      "<style>" +
      "html,body{min-height:100%;}" +
      "body{box-sizing:border-box;margin:0;padding:12px;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;font-size:14px;line-height:1.5;color:#24292f;background:#fff;}" +
      "img,video{max-width:100%;height:auto;}" +
      "table{border-collapse:collapse;}" +
      "td,th{border:1px solid #d0d7de;padding:4px 8px;}" +
      "</style>" +
      "</head>" +
      "<body>" +
      source +
      "</body>" +
      "</html>"
    );
  }

  function createElement(tagName, className, text) {
    var element = document.createElement(tagName);

    if (className) element.className = className;
    if (text != null) element.textContent = text;

    return element;
  }

  function createIconButton(className, label, iconName) {
    var button = createElement("button", className);
    var icons = {
      copy:
        '<svg viewBox="0 0 24 24" focusable="false" aria-hidden="true">' +
        '<rect x="9" y="9" width="10" height="10" rx="2"></rect>' +
        '<path d="M5 15V7a2 2 0 0 1 2-2h8"></path>' +
        "</svg>",
      play:
        '<svg viewBox="0 0 24 24" focusable="false" aria-hidden="true">' +
        '<path d="M8 5v14l11-7z"></path>' +
        "</svg>",
    };

    button.type = "button";
    button.setAttribute("aria-label", label);
    button.setAttribute("title", label);
    button.innerHTML = icons[iconName] || "";

    return button;
  }

  function createLivePreview(block) {
    var pre = block.querySelector(".code-block-body pre");
    var code = pre && pre.querySelector("code");
    var source;
    var wrapper;
    var toolbar;
    var toolbarActions;
    var copyButton;
    var editorLabel;
    var previewButton;
    var layout;
    var editorPane;
    var editor;
    var previewPane;
    var frame;

    if (!pre || !code || !isHtmlBlock(pre, code)) return null;

    source = code.textContent || "";
    wrapper = createElement("div", "html-live-preview no-cjk-spacing");
    toolbar = createElement("div", "html-live-preview-toolbar");
    toolbarActions = createElement("div", "html-live-preview-actions");
    editorLabel = createElement("span", "html-live-preview-label", config.editorLabel);
    copyButton = createIconButton("html-live-preview-copy", config.copyText, "copy");
    previewButton = createIconButton("html-live-preview-run", config.previewText, "play");
    layout = createElement("div", "html-live-preview-layout");
    editorPane = createElement("div", "html-live-preview-editor-pane");
    editor = createElement("div", "html-live-preview-editor language-markup");
    previewPane = createElement("div", "html-live-preview-preview-pane");
    frame = createElement("iframe", "html-live-preview-frame");

    wrapper.dataset.htmlLivePreview = "true";
    previewPane.hidden = true;
    previewButton.setAttribute("aria-expanded", "false");
    editor.dataset.rawValue = source;
    editor.innerHTML = highlightSource(source);
    editor.setAttribute("aria-label", "编辑 HTML 代码");
    editor.setAttribute("contenteditable", "true");
    editor.setAttribute("role", "textbox");
    editor.setAttribute("spellcheck", "false");
    frame.setAttribute("sandbox", config.sandbox);
    frame.setAttribute("title", "HTML 预览");

    toolbar.appendChild(editorLabel);
    toolbarActions.appendChild(copyButton);
    toolbarActions.appendChild(previewButton);
    toolbar.appendChild(toolbarActions);
    editorPane.appendChild(editor);
    previewPane.appendChild(frame);
    layout.appendChild(editorPane);
    layout.appendChild(previewPane);
    wrapper.appendChild(toolbar);
    wrapper.appendChild(layout);

    return wrapper;
  }

  function enhanceHtml(html) {
    var container = document.createElement("div");

    container.innerHTML = html;
    container.querySelectorAll(".code-block").forEach(function (block) {
      var preview = createLivePreview(block);

      if (preview) {
        block.parentNode.replaceChild(preview, block);
      }
    });

    return container.innerHTML;
  }

  function readEditorValue(editor) {
    if (!editor) return "";

    return editor.dataset.rawValue != null ? editor.dataset.rawValue : editor.textContent || "";
  }

  function updateEditorHighlight(editor) {
    var source = readEditorValue(editor);
    var selection = window.getSelection();
    var offset = getSelectionOffset(editor, selection);

    editor.innerHTML = highlightSource(source);
    restoreSelection(editor, offset);
  }

  function updatePreview(editor) {
    var wrapper = editor.closest(".html-live-preview");
    var frame = wrapper && wrapper.querySelector(".html-live-preview-frame");
    var previewPane = wrapper && wrapper.querySelector(".html-live-preview-preview-pane");
    var previewButton = wrapper && wrapper.querySelector(".html-live-preview-run");

    if (!frame || !previewPane) return;

    frame.setAttribute("srcdoc", buildPreviewDocument(readEditorValue(editor)));
    previewPane.hidden = false;
    wrapper.classList.add("is-previewing");

    if (previewButton) {
      previewButton.setAttribute("aria-label", config.refreshText);
      previewButton.setAttribute("title", config.refreshText);
      previewButton.setAttribute("aria-expanded", "true");
    }
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
    window.clearTimeout(copyTimer);
    button.dataset.copyState = state;
    button.setAttribute("aria-label", label);
    button.setAttribute("title", label);

    copyTimer = window.setTimeout(function () {
      button.dataset.copyState = "";
      button.setAttribute("aria-label", config.copyText);
      button.setAttribute("title", config.copyText);
    }, 1000);
  }

  function handleCopy(button) {
    var wrapper = button.closest(".html-live-preview");
    var editor = wrapper && wrapper.querySelector(".html-live-preview-editor");

    if (!editor) return;

    copyText(readEditorValue(editor))
      .then(function () {
        setCopyState(button, "success", config.copySuccessText);
      })
      .catch(function () {
        setCopyState(button, "error", config.copyErrorText);
      });
  }

  function schedulePreview(editor) {
    var timer = previewTimers.get(editor);
    var wrapper = editor.closest(".html-live-preview");

    if (!wrapper || !wrapper.classList.contains("is-previewing")) return;

    window.clearTimeout(timer);
    previewTimers.set(
      editor,
      window.setTimeout(function () {
        updatePreview(editor);
      }, Number(config.debounce) || DEFAULT_CONFIG.debounce)
    );
  }

  function getSelectionOffset(root, selection) {
    var offsets = getSelectionOffsets(root, selection);

    return offsets && offsets.end;
  }

  function getSelectionOffsets(root, selection) {
    var range;
    var beforeStart;
    var beforeEnd;

    if (!selection || selection.rangeCount === 0) return null;
    if (!root.contains(selection.anchorNode)) return null;

    range = selection.getRangeAt(0);
    beforeStart = range.cloneRange();
    beforeStart.selectNodeContents(root);
    beforeStart.setEnd(range.startContainer, range.startOffset);
    beforeEnd = range.cloneRange();
    beforeEnd.selectNodeContents(root);
    beforeEnd.setEnd(range.endContainer, range.endOffset);

    return {
      end: beforeEnd.toString().length,
      start: beforeStart.toString().length,
    };
  }

  function restoreSelection(root, offset) {
    var selection = window.getSelection();
    var walker;
    var node;
    var remaining;
    var range;

    if (offset == null || !selection) return;

    walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    remaining = offset;

    while ((node = walker.nextNode())) {
      if (node.nodeValue.length >= remaining) {
        range = document.createRange();
        range.setStart(node, remaining);
        range.collapse(true);
        selection.removeAllRanges();
        selection.addRange(range);
        return;
      }

      remaining -= node.nodeValue.length;
    }

    range = document.createRange();
    range.selectNodeContents(root);
    range.collapse(false);
    selection.removeAllRanges();
    selection.addRange(range);
  }

  function replaceSelection(editor, text) {
    var selection = window.getSelection();
    var offsets = getSelectionOffsets(editor, selection);
    var value = readEditorValue(editor);
    var start;
    var end;

    if (!offsets) return;

    start = Math.min(offsets.start, offsets.end);
    end = Math.max(offsets.start, offsets.end);
    editor.dataset.rawValue = value.slice(0, start) + text + value.slice(end);
    updateEditorHighlight(editor);
    restoreSelection(editor, start + text.length);
  }

  function closestTarget(event, selector) {
    var target = event.target;

    return target && typeof target.closest === "function" ? target.closest(selector) : null;
  }

  function isComposing(editor, event) {
    return Boolean((event && event.isComposing) || (editor && editor.dataset.composing === "true"));
  }

  function handleInput(event) {
    var editor = closestTarget(event, ".html-live-preview-editor");

    if (!editor) return;

    if (isComposing(editor, event)) {
      editor.dataset.rawValue = editor.textContent || "";
      return;
    }

    editor.dataset.rawValue = editor.textContent || "";
    updateEditorHighlight(editor);
    schedulePreview(editor);
  }

  function handleCompositionStart(event) {
    var editor = closestTarget(event, ".html-live-preview-editor");

    if (!editor) return;

    editor.dataset.composing = "true";
  }

  function handleCompositionEnd(event) {
    var editor = closestTarget(event, ".html-live-preview-editor");

    if (!editor) return;

    editor.dataset.composing = "false";
    editor.dataset.rawValue = editor.textContent || "";
    updateEditorHighlight(editor);
    schedulePreview(editor);
  }

  function handlePaste(event) {
    var editor = closestTarget(event, ".html-live-preview-editor");
    var text;

    if (!editor) return;

    event.preventDefault();
    text = event.clipboardData ? event.clipboardData.getData("text/plain") : "";
    replaceSelection(editor, text);
    schedulePreview(editor);
  }

  function handleKeydown(event) {
    var editor = closestTarget(event, ".html-live-preview-editor");

    if (!editor || (event.key !== "Tab" && event.key !== "Enter")) return;
    if (isComposing(editor, event)) return;

    event.preventDefault();
    replaceSelection(editor, event.key === "Tab" ? "  " : "\n");
    schedulePreview(editor);
  }

  function handleClick(event) {
    var copyButton = closestTarget(event, ".html-live-preview-copy");
    var button = closestTarget(event, ".html-live-preview-run");
    var wrapper;
    var editor;

    if (copyButton) {
      handleCopy(copyButton);
      return;
    }

    if (!button) return;

    wrapper = button.closest(".html-live-preview");
    editor = wrapper && wrapper.querySelector(".html-live-preview-editor");

    if (!editor) return;

    updatePreview(editor);
  }

  function mountEvents() {
    var content = document.querySelector(".content");

    if (!content || content.dataset.htmlLivePreviewMounted === "true") return;

    content.dataset.htmlLivePreviewMounted = "true";
    content.addEventListener("click", handleClick);
    content.addEventListener("compositionend", handleCompositionEnd);
    content.addEventListener("compositionstart", handleCompositionStart);
    content.addEventListener("input", handleInput);
    content.addEventListener("keydown", handleKeydown);
    content.addEventListener("paste", handlePaste);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.afterEach(function (html, next) {
      next(enhanceHtml(html));
    });

    hook.mounted(mountEvents);
  });
})();
