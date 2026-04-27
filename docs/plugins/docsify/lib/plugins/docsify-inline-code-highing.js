(function () {
  "use strict";

  function showTip(text) {
    const tip = document.createElement("div");
    tip.className = "copy-success-tip";
    tip.textContent = `已复制：${text}`;
    document.body.appendChild(tip);
    setTimeout(function () {
      tip.remove();
    }, 2000);
  }

  async function copy(text) {
    try {
      await navigator.clipboard.writeText(text);
      return true;
    } catch (error) {
      console.error("复制失败:", error);
      return false;
    }
  }

  function plugin(hook) {
    hook.mounted(function () {
      const content = document.querySelector(".content");
      if (!content) return;

      content.addEventListener("click", async function (event) {
        const code = event.target.closest("code");
        if (!code || code.closest("pre")) return;

        const text = code.textContent;
        if (await copy(text)) {
          showTip(text);
        }
      });
    });
  }

  const style = document.createElement("style");
  style.textContent = `
    code:not(pre code) {
      background-color: #f3f4f4;
      border: 1px solid #e8e8e8;
      border-radius: 4px;
      padding: 2px 6px;
      margin: 0 2px;
      cursor: pointer;
      transition: all 0.2s;
    }
    code:not(pre code):active {
      background-color: #e8e8e8;
    }
    .copy-success-tip {
      position: fixed;
      bottom: 20px;
      left: 50%;
      transform: translateX(-50%);
      background: rgba(0, 0, 0, 0.75);
      color: #fff;
      padding: 8px 14px;
      border-radius: 4px;
      z-index: 10000;
      max-width: min(80vw, 720px);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
  `;
  document.head.appendChild(style);

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
