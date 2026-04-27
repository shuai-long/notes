(function () {
  "use strict";

  const classes = {
    container: "docsify-tabs",
    tab: "docsify-tabs__tab",
    tabActive: "docsify-tabs__tab--active",
    content: "docsify-tabs__content",
  };

  function directChildren(element, className) {
    return Array.prototype.filter.call(element.children, function (child) {
      return child.classList.contains(className);
    });
  }

  function setActiveTab(tab) {
    const tabs = tab.closest("." + classes.container);
    if (!tabs) return;

    directChildren(tabs, classes.tab).forEach(function (item) {
      const active = item === tab;
      item.classList.toggle(classes.tabActive, active);
      item.setAttribute("aria-selected", active ? "true" : "false");
      item.setAttribute("tabindex", active ? "0" : "-1");
    });
  }

  function activateMissingTabs(root) {
    const containers = (root || document).querySelectorAll("." + classes.container);

    containers.forEach(function (tabs) {
      const tabButtons = directChildren(tabs, classes.tab);
      if (!tabButtons.length) return;

      const hasActive = tabButtons.some(function (tab) {
        return tab.classList.contains(classes.tabActive);
      });

      if (!hasActive) {
        setActiveTab(tabButtons[0]);
      }
    });
  }

  function schedule() {
    requestAnimationFrame(function () {
      activateMissingTabs(document);
    });
  }

  function plugin(hook) {
    hook.doneEach(schedule);

    hook.mounted(function () {
      const content = document.querySelector(".content");
      if (!content) return;

      content.addEventListener("click", function (event) {
        const tab = event.target.closest("." + classes.tab);
        if (!tab || !content.contains(tab)) return;
        setActiveTab(tab);
      });
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
