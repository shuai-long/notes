(function () {
  var DEFAULT_CONFIG = {
    minWidth: 260,
    maxWidth: 520,
    storageKey: "docsify-sidebar-width",
    breakpoint: "(min-width: 48em)",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.sidebarResize) || {});
  var root = document.documentElement;
  var desktopMedia = window.matchMedia ? window.matchMedia(config.breakpoint) : null;
  var handle = null;
  var dragState = null;
  var defaultWidth = getCssSidebarWidth() || 300;

  function toNumber(value) {
    var number = Number.parseFloat(value);

    return Number.isFinite(number) ? number : 0;
  }

  function isDesktop() {
    return !desktopMedia || desktopMedia.matches;
  }

  function getCssSidebarWidth() {
    return toNumber(window.getComputedStyle(root).getPropertyValue("--sidebar-width"));
  }

  function getDynamicMaxWidth() {
    var configuredMax = toNumber(config.maxWidth) || DEFAULT_CONFIG.maxWidth;
    var configuredMin = toNumber(config.minWidth) || DEFAULT_CONFIG.minWidth;
    var viewportMax = Math.floor(window.innerWidth * 0.5);

    return Math.max(configuredMin, Math.min(configuredMax, viewportMax || configuredMax));
  }

  function clampWidth(width) {
    var minWidth = toNumber(config.minWidth) || DEFAULT_CONFIG.minWidth;
    var maxWidth = getDynamicMaxWidth();

    return Math.min(maxWidth, Math.max(minWidth, Math.round(width)));
  }

  function readStoredWidth() {
    try {
      return toNumber(window.localStorage.getItem(config.storageKey));
    } catch (error) {
      return 0;
    }
  }

  function storeWidth(width) {
    try {
      window.localStorage.setItem(config.storageKey, String(width));
    } catch (error) {
      return;
    }
  }

  function updateHandleAria(width) {
    if (!handle) return;

    handle.setAttribute("aria-valuemin", String(toNumber(config.minWidth) || DEFAULT_CONFIG.minWidth));
    handle.setAttribute("aria-valuemax", String(getDynamicMaxWidth()));
    handle.setAttribute("aria-valuenow", String(width));
  }

  function applyWidth(width, shouldStore) {
    var nextWidth = clampWidth(width);

    root.style.setProperty("--sidebar-width", nextWidth + "px");
    updateHandleAria(nextWidth);

    if (shouldStore) {
      storeWidth(nextWidth);
    }

    return nextWidth;
  }

  function clearDesktopWidth() {
    root.style.removeProperty("--sidebar-width");
    updateHandleAria(defaultWidth);
  }

  function syncResponsiveWidth() {
    var storedWidth = readStoredWidth();

    if (isDesktop()) {
      applyWidth(storedWidth || getCssSidebarWidth() || defaultWidth, false);
      return;
    }

    clearDesktopWidth();
  }

  function getCurrentWidth() {
    var sidebar = document.querySelector(".sidebar");
    var rectWidth = sidebar ? sidebar.getBoundingClientRect().width : 0;

    return getCssSidebarWidth() || rectWidth || defaultWidth;
  }

  function stopDrag() {
    if (!dragState) return;

    dragState = null;
    root.classList.remove("is-sidebar-resizing");
    document.removeEventListener("pointermove", resizeByPointer);
    document.removeEventListener("pointerup", stopDrag);
    document.removeEventListener("pointercancel", stopDrag);
  }

  function resizeByPointer(event) {
    if (!dragState) return;

    event.preventDefault();
    applyWidth(dragState.startWidth + event.clientX - dragState.startX, true);
  }

  function startDrag(event) {
    if (!isDesktop()) return;

    event.preventDefault();
    event.stopPropagation();

    dragState = {
      startX: event.clientX,
      startWidth: getCurrentWidth(),
    };

    root.classList.add("is-sidebar-resizing");
    document.addEventListener("pointermove", resizeByPointer, { passive: false });
    document.addEventListener("pointerup", stopDrag);
    document.addEventListener("pointercancel", stopDrag);
  }

  function resizeByKeyboard(event) {
    var step = event.shiftKey ? 32 : 16;
    var currentWidth;
    var nextWidth;

    if (!isDesktop()) return;

    if (event.key === "ArrowLeft") {
      nextWidth = getCurrentWidth() - step;
    } else if (event.key === "ArrowRight") {
      nextWidth = getCurrentWidth() + step;
    } else if (event.key === "Home") {
      nextWidth = toNumber(config.minWidth) || DEFAULT_CONFIG.minWidth;
    } else if (event.key === "End") {
      nextWidth = getDynamicMaxWidth();
    } else {
      return;
    }

    event.preventDefault();
    currentWidth = applyWidth(nextWidth, true);
    updateHandleAria(currentWidth);
  }

  function createHandle() {
    var sidebar = document.querySelector(".sidebar");

    if (!sidebar) return;

    handle = sidebar.querySelector(".sidebar-resize-handle");

    if (!handle) {
      handle = document.createElement("button");
      handle.className = "sidebar-resize-handle";
      handle.type = "button";
      handle.setAttribute("aria-label", "调整侧边栏宽度");
      handle.setAttribute("aria-orientation", "vertical");
      handle.setAttribute("role", "separator");
      handle.setAttribute("tabindex", "0");
      handle.setAttribute("title", "拖动调整侧边栏宽度");
      sidebar.appendChild(handle);
    }

    if (handle.dataset.sidebarResizeBound === "true") return;

    handle.dataset.sidebarResizeBound = "true";
    handle.addEventListener("pointerdown", startDrag);
    handle.addEventListener("keydown", resizeByKeyboard);
  }

  function initSidebarResize() {
    createHandle();
    syncResponsiveWidth();
  }

  if (desktopMedia) {
    if (desktopMedia.addEventListener) {
      desktopMedia.addEventListener("change", syncResponsiveWidth);
    } else if (desktopMedia.addListener) {
      desktopMedia.addListener(syncResponsiveWidth);
    }
  }

  window.addEventListener("resize", syncResponsiveWidth);
  syncResponsiveWidth();

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.mounted(initSidebarResize);
    hook.doneEach(initSidebarResize);
  });
})();
