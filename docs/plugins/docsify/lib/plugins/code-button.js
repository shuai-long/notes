window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  const DEFAULT_MAX_LINE_NUMBERS = 500;

  const styles = `
    .code-buttons {
      position: absolute;
      top: 8px;
      right: 8px;
      display: flex;
      gap: 8px;
      opacity: 0.68;
      transition: opacity 0.2s;
      z-index: 2;
      background: rgba(255,255,255,0.86);
      padding: 4px;
      border-radius: 4px;
    }
    pre:hover > .code-buttons {
      opacity: 1;
    }
    .code-buttons button {
      background: none;
      border: none;
      cursor: pointer;
      color: #666;
      font-size: 14px;
      line-height: 1;
      padding: 2px;
    }
    .code-line-numbers {
      position: absolute;
      left: 0;
      top: 2.15em;
      padding: var(--code-block-padding);
      border-right: 1px solid;
      text-align: right;
      user-select: none;
      line-height: 1.5;
      box-sizing: border-box;
    }
    .code-line-numbers span {
      display: block;
    }
    pre[data-linenos] {
      position: relative;
      padding-left: 4em !important;
    }
    pre[data-linenos-skipped] {
      position: relative;
    }
  `;

  const style = document.createElement("style");
  style.textContent = styles;
  document.head.appendChild(style);

  function getConfig() {
    const config = (vm && vm.config && vm.config.codeButton) || {};
    return {
      lineNumbers: config.lineNumbers !== false,
      maxLineNumbers:
        Number(config.maxLineNumbers) || DEFAULT_MAX_LINE_NUMBERS,
    };
  }

  function schedule(task) {
    if ("requestIdleCallback" in window) {
      requestIdleCallback(task, { timeout: 800 });
      return;
    }
    setTimeout(task, 0);
  }

  function createButton(icon, title, onClick) {
    const button = document.createElement("button");
    button.className = "fa-solid " + icon;
    button.type = "button";
    button.title = title;
    button.setAttribute("aria-label", title);
    button.addEventListener("click", onClick);
    return button;
  }

  function showToast(message, isError = false) {
    const toast = document.createElement("div");
    toast.textContent = message;
    toast.style.position = "fixed";
    toast.style.bottom = "20px";
    toast.style.left = "50%";
    toast.style.transform = "translateX(-50%)";
    toast.style.background = isError ? "#ff4444" : "#4CAF50";
    toast.style.color = "white";
    toast.style.padding = "8px 16px";
    toast.style.borderRadius = "20px";
    toast.style.fontSize = "14px";
    toast.style.boxShadow = "0 2px 4px rgba(0,0,0,0.2)";
    toast.style.zIndex = "10001";
    document.body.appendChild(toast);
    setTimeout(() => toast.remove(), 1500);
  }

  function runHtmlCss(code) {
    const iframe = document.createElement("iframe");
    iframe.style.cssText =
      "width: 100%;height: 300px;border: 1px solid #e1e4e8;border-radius: 6px;background: white;margin: 10px 0;";
    iframe.srcdoc = `<!DOCTYPE html><html><head><style>body {padding: 20px;font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto;line-height: 1.6; }</style></head><body>${code}</body></html>`;
    showCodeResult(iframe);
  }

  function runJavaScript(code) {
    const result = eval(code);
    showCodeResult(document.createTextNode(String(result)));
  }

  function showCodeResult(content) {
    const overlay = document.createElement("div");
    overlay.style.cssText =
      "position: fixed;top: 0;left: 0;width: 100%;height: 100%;background: rgba(0,0,0,0.5);display: flex;justify-content: center;align-items: center;backdrop-filter: blur(2px);z-index: 9999;";
    const panel = document.createElement("div");
    panel.style.cssText =
      "background: white;padding: 24px;border-radius: 12px;width: min(90%, 800px);max-height: 80vh;box-shadow: 0 8px 32px rgba(0,0,0,0.2);position: relative;";
    const header = document.createElement("div");
    header.style.cssText =
      "display: flex;justify-content: space-between;align-items: center;margin-bottom: 16px;padding-bottom: 8px;border-bottom: 1px solid #eee;";
    const title = document.createElement("h3");
    title.textContent = "运行结果";
    title.style.cssText = "margin: 0;font-size: 18px;color: #333;";
    const closeButton = document.createElement("button");
    closeButton.innerHTML = '<i class="fa-solid fa-xmark"></i>';
    closeButton.style.cssText =
      "background: none;border: none;cursor: pointer;color: #666;font-size: 16px;padding: 4px;";
    closeButton.onclick = () => overlay.remove();
    header.append(title, closeButton);
    panel.append(header, content);
    overlay.append(panel);
    document.body.appendChild(overlay);
  }

  function countLines(text) {
    return text ? text.split("\n").length : 0;
  }

  function addLineNumbers(preElement, codeElement, lineCount, config) {
    if (!config.lineNumbers || lineCount > config.maxLineNumbers) {
      preElement.setAttribute("data-linenos-skipped", "");
      return;
    }

    const lineNumbers = document.createElement("div");
    lineNumbers.className = "code-line-numbers";
    lineNumbers.innerHTML = Array.from(
      { length: lineCount },
      (_, index) => `<span>${index + 1}</span>`
    ).join("");

    preElement.insertBefore(lineNumbers, codeElement);
    preElement.setAttribute("data-linenos", "");
  }

  function enhanceCodeBlock(codeElement, config) {
    if (codeElement.dataset.codeButtonProcessed === "true") return;

    const preElement = codeElement.parentElement;
    const lineCount = countLines(codeElement.textContent);
    const language =
      (
        codeElement.className.match(/language-([\w-]+)/) ||
        codeElement.className.match(/lang-([\w-]+)/) ||
        []
      )[1] || "txt";

    codeElement.dataset.codeButtonProcessed = "true";
    preElement.style.position = "relative";

    if (!preElement.querySelector(".code-line-numbers")) {
      addLineNumbers(preElement, codeElement, lineCount, config);
    }

    if (preElement.querySelector(".code-buttons")) return;

    const buttons = document.createElement("div");
    buttons.className = "code-buttons";

    const downloadButton = createButton("fa-download", "下载代码", () => {
      const blob = new Blob([codeElement.textContent], { type: "text/plain" });
      const url = URL.createObjectURL(blob);
      const link = document.createElement("a");
      link.href = url;
      link.download = `code.${language}`;
      link.click();
      URL.revokeObjectURL(url);
    });

    const copyButton = createButton("fa-copy", "复制代码", async () => {
      await navigator.clipboard.writeText(codeElement.textContent);
      showToast("复制成功");
    });

    buttons.append(downloadButton, copyButton);

    if (["html", "css", "javascript", "js"].includes(language)) {
      const runButton = createButton("fa-play", "运行代码", () => {
        try {
          ["html", "css"].includes(language)
            ? runHtmlCss(codeElement.textContent)
            : runJavaScript(codeElement.textContent);
        } catch (error) {
          showToast(`Error: ${error.message}`, true);
        }
      });
      buttons.append(runButton);
    }

    preElement.prepend(buttons);
  }

  hook.doneEach(function () {
    const config = getConfig();
    schedule(function () {
      document
        .querySelectorAll(".content pre > code")
        .forEach((codeElement) => enhanceCodeBlock(codeElement, config));
    });
  });
});
