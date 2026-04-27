(function () {
  "use strict";

  function addStyle(css) {
    if (!css || typeof document === "undefined") return;
    const head = document.head || document.getElementsByTagName("head")[0];
    const style = document.createElement("style");
    style.type = "text/css";
    style.appendChild(document.createTextNode(css));
    head.appendChild(style);
  }

  function schedule(task) {
    if ("requestIdleCallback" in window) {
      requestIdleCallback(task, { timeout: 1000 });
      return;
    }
    setTimeout(task, 80);
  }

  function lineCount(text) {
    return text ? text.split("\n").length : 0;
  }

  function plugin(hook, vm) {
    const config = vm.config.hideCode;
    const maskHtml =
      '<div class="hide-code-mask"><button class="hide-code-mask-btn" type="button" aria-label="展开或收起代码"></button></div>';

    hook.doneEach(function () {
      if (!config) return;

      const maxHeight = Number(config.height) || 500;
      const minLines = Number(config.minLines) || 28;
      const blocks = Array.from(document.querySelectorAll(".content pre"));

      schedule(function () {
        blocks.forEach(function (block) {
          if (block.dataset.hideCodeProcessed === "true") return;

          const code = block.querySelector("code");
          if (code && lineCount(code.textContent) < minLines) {
            block.dataset.hideCodeProcessed = "true";
            return;
          }

          block.dataset.hideCodeProcessed = "true";

          if (block.scrollHeight <= maxHeight) return;

          block.classList.add("hide-code");
          block.style.maxHeight = maxHeight + "px";

          if (!block.querySelector(".hide-code-mask")) {
            block.insertAdjacentHTML("beforeend", maskHtml);
          }
        });
      });
    });

    hook.mounted(function () {
      if (!config) return;

      const content = document.querySelector(".content");
      if (!content) return;

      content.addEventListener("click", function (event) {
        if (!event.target.classList.contains("hide-code-mask-btn")) return;

        const block = event.target.closest("pre");
        if (!block) return;

        const maxHeight = Number(config.height) || 500;
        const expanded = block.classList.contains("expanded");

        block.classList.toggle("expanded");
        block.style.maxHeight = expanded ? maxHeight + "px" : block.scrollHeight + "px";

        if (!expanded) {
          setTimeout(function () {
            block.style.maxHeight = "none";
          }, 300);
        }
      });
    });
  }

  const css =
    ".hide-code{overflow-y:hidden!important;position:relative}" +
    ".hide-code .hide-code-mask{background-image:linear-gradient(-180deg,hsla(0,0%,100%,0),#fff 85%);bottom:0;left:0;padding-top:78px;position:absolute;right:0;text-align:center;width:100%;z-index:10}" +
    ".hide-code .hide-code-mask .hide-code-mask-btn{background:transparent;border:0;cursor:pointer;display:inline-block;height:28px;width:64px}" +
    ".hide-code .hide-code-mask .hide-code-mask-btn:before{border:9px solid transparent;border-top-color:#aaa;content:\"\";display:inline-block;height:0;transform:translateY(10px);transition:all .3s;width:0}" +
    ".hide-code.expanded .hide-code-mask{background:none;padding-top:0}" +
    ".hide-code.expanded .hide-code-mask-btn:before{transform:translateY(10px) rotate(180deg)}";

  addStyle(css);

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
