(function () {
  "use strict";

  function wrapTables(root) {
    const tables = Array.from(
      root.querySelectorAll(".markdown-section table:not(.docs-responsive-table)")
    );

    tables.forEach((table) => {
      table.classList.add("docs-responsive-table");

      if (table.closest(".docs-table-scroll")) return;

      const wrapper = document.createElement("div");
      wrapper.className = "docs-table-scroll";
      table.parentNode.insertBefore(wrapper, table);
      wrapper.appendChild(table);
    });
  }

  function schedule(task) {
    if ("requestAnimationFrame" in window) {
      requestAnimationFrame(task);
      return;
    }

    setTimeout(task, 0);
  }

  function plugin(hook) {
    hook.doneEach(function () {
      schedule(function () {
        wrapTables(document);
      });
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
