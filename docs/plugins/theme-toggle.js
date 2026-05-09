(function () {
  var DEFAULT_CONFIG = {
    storageKey: "docsify-theme-mode",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.themeToggle) || {});
  var darkMediaText = "(prefers-color-scheme: dark)";
  var mediaQuery = window.matchMedia ? window.matchMedia(darkMediaText) : null;
  var lightIcon =
    '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false">' +
    '<circle cx="12" cy="12" r="4"></circle>' +
    '<path d="M12 2v2"></path><path d="M12 20v2"></path>' +
    '<path d="m4.93 4.93 1.41 1.41"></path><path d="m17.66 17.66 1.41 1.41"></path>' +
    '<path d="M2 12h2"></path><path d="M20 12h2"></path>' +
    '<path d="m6.34 17.66-1.41 1.41"></path><path d="m19.07 4.93-1.41 1.41"></path>' +
    "</svg>";
  var darkIcon =
    '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false">' +
    '<path d="M21 12.8A8.5 8.5 0 1 1 11.2 3 6.6 6.6 0 0 0 21 12.8Z"></path>' +
    "</svg>";

  function getStoredMode() {
    try {
      var mode = window.localStorage.getItem(config.storageKey);

      return mode === "light" || mode === "dark" ? mode : "auto";
    } catch (error) {
      return "auto";
    }
  }

  function storeMode(mode) {
    try {
      window.localStorage.setItem(config.storageKey, mode);
    } catch (error) {
      return;
    }
  }

  function getEffectiveMode(mode) {
    if (mode === "light" || mode === "dark") return mode;
    return mediaQuery && mediaQuery.matches ? "dark" : "light";
  }

  function applyTheme(mode) {
    var lightLink = document.getElementById("docsify-theme-light");
    var darkLink = document.getElementById("docsify-theme-dark");
    var effectiveMode = getEffectiveMode(mode);

    if (lightLink) {
      lightLink.media = "all";
    }

    if (darkLink) {
      darkLink.media = mode === "dark" ? "all" : mode === "light" ? "not all" : darkMediaText;
    }

    document.documentElement.dataset.docsifyTheme = effectiveMode;
    updateButton(effectiveMode);
  }

  function updateButton(effectiveMode) {
    var button = document.querySelector(".docsify-theme-toggle");
    var nextMode = effectiveMode === "dark" ? "light" : "dark";

    if (!button) return;

    button.innerHTML = effectiveMode === "dark" ? lightIcon : darkIcon;
    button.dataset.themeMode = effectiveMode;
    button.setAttribute("aria-label", nextMode === "dark" ? "切换到深色主题" : "切换到浅色主题");
    button.setAttribute("title", nextMode === "dark" ? "切换到深色主题" : "切换到浅色主题");
  }

  function createButton() {
    var button = document.querySelector(".docsify-theme-toggle");

    if (button) return button;

    button = document.createElement("button");
    button.className = "docsify-theme-toggle";
    button.type = "button";
    button.addEventListener("click", function () {
      var mode = getEffectiveMode(getStoredMode()) === "dark" ? "light" : "dark";

      storeMode(mode);
      applyTheme(mode);
    });
    document.body.appendChild(button);
    return button;
  }

  function initThemeToggle() {
    createButton();
    applyTheme(getStoredMode());
  }

  if (mediaQuery) {
    if (mediaQuery.addEventListener) {
      mediaQuery.addEventListener("change", function () {
        if (getStoredMode() === "auto") applyTheme("auto");
      });
    } else if (mediaQuery.addListener) {
      mediaQuery.addListener(function () {
        if (getStoredMode() === "auto") applyTheme("auto");
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initThemeToggle);
  } else {
    initThemeToggle();
  }
})();
