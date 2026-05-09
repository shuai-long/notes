(function () {
  var DEFAULT_CONFIG = {
    collapseByDefault: true,
    expandActive: true,
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.sidebarTree) || {});
  var folderState = Object.create(null);

  function normalizeText(text) {
    return (text || "").replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
  }

  function getDirectChildList(item) {
    for (var index = 0; index < item.children.length; index += 1) {
      if (item.children[index].tagName === "UL" && !item.children[index].classList.contains("app-sub-sidebar")) {
        return item.children[index];
      }
    }

    return null;
  }

  function getDirectTextNodes(item) {
    var nodes = [];
    var childList = getDirectChildList(item);

    for (var index = 0; index < item.childNodes.length; index += 1) {
      var node = item.childNodes[index];

      if (node === childList) break;
      if (node.nodeType === 3 && normalizeText(node.textContent)) {
        nodes.push(node);
      }
    }

    return nodes;
  }

  function getLabel(item) {
    for (var index = 0; index < item.children.length; index += 1) {
      if (
        item.children[index].tagName === "P" ||
        item.children[index].tagName === "A" ||
        item.children[index].classList.contains("sidebar-folder-label")
      ) {
        return item.children[index];
      }
    }

    return null;
  }

  function ensureFolderLabel(item) {
    var label = getLabel(item);
    var textNodes;
    var text;
    var childList;

    if (label) return label;

    textNodes = getDirectTextNodes(item);
    text = normalizeText(
      textNodes
        .map(function (node) {
          return node.textContent;
        })
        .join(" ")
    );

    if (!text) return null;

    childList = getDirectChildList(item);
    label = document.createElement("p");
    label.className = "sidebar-folder-label";
    label.textContent = text;
    item.insertBefore(label, childList);

    textNodes.forEach(function (node) {
      node.remove();
    });

    return label;
  }

  function getToggle(item) {
    for (var index = 0; index < item.children.length; index += 1) {
      if (item.children[index].classList.contains("sidebar-folder-toggle")) {
        return item.children[index];
      }
    }

    return null;
  }

  function getDirectLink(item) {
    for (var index = 0; index < item.children.length; index += 1) {
      if (item.children[index].tagName === "A") {
        return item.children[index];
      }
    }

    return null;
  }

  function getItemText(item) {
    var label = getLabel(item);
    var textNodes;

    if (label) return normalizeText(label.textContent);

    textNodes = getDirectTextNodes(item);
    return normalizeText(
      textNodes
        .map(function (node) {
          return node.textContent;
        })
        .join(" ")
    );
  }

  function getDepth(item) {
    var depth = 0;
    var node = item.parentNode;

    while (node && !(node.classList && node.classList.contains("sidebar-nav"))) {
      if (node.tagName === "UL") depth += 1;
      node = node.parentNode;
    }

    return depth;
  }

  function isInSubSidebar(item) {
    var node = item.parentNode;

    while (node && !(node.classList && node.classList.contains("sidebar-nav"))) {
      if (node.classList && node.classList.contains("app-sub-sidebar")) return true;
      node = node.parentNode;
    }

    return false;
  }

  function getFolderKey(item) {
    var parts = [];
    var node = item;

    while (node && !(node.classList && node.classList.contains("sidebar-nav"))) {
      if (node.tagName === "LI") {
        parts.unshift(getItemText(node));
      }

      node = node.parentNode;
    }

    return parts.join("/");
  }

  function isActiveBranch(item) {
    return Boolean(item.querySelector("li.active, a.active"));
  }

  function setExpanded(item, expanded) {
    var button = getToggle(item);
    var label = ensureFolderLabel(item);

    item.classList.toggle("is-folder-expanded", expanded);
    item.classList.toggle("is-folder-collapsed", !expanded);

    if (button) {
      button.setAttribute("aria-expanded", expanded ? "true" : "false");
      button.setAttribute("aria-label", expanded ? "关闭文件夹" : "展开文件夹");
    }

    if (label && label.tagName === "P") {
      label.setAttribute("aria-expanded", expanded ? "true" : "false");
    }
  }

  function toggleFolder(item) {
    var key = getFolderKey(item);
    var expanded = item.classList.contains("is-folder-collapsed");

    folderState[key] = expanded;
    setExpanded(item, expanded);
  }

  function stopEvent(event) {
    event.preventDefault();
    event.stopPropagation();

    if (event.stopImmediatePropagation) {
      event.stopImmediatePropagation();
    }
  }

  function ensureToggle(item) {
    var label = ensureFolderLabel(item);
    var button = getToggle(item);

    if (!label) return;

    if (button) {
      button.parentNode.removeChild(button);
    }

    if (label.tagName === "P") {
      label.setAttribute("role", "button");
      label.setAttribute("tabindex", "0");
    }
  }

  function enhanceFolder(item) {
    ensureFolderLabel(item);

    var active = config.expandActive && isActiveBranch(item);
    var key = getFolderKey(item);
    var hasState = Object.prototype.hasOwnProperty.call(folderState, key);
    var shouldExpand = hasState ? folderState[key] : active || !config.collapseByDefault;

    item.classList.add("sidebar-folder");
    item.classList.remove("sidebar-file", "sidebar-section");
    item.classList.toggle("is-active-branch", active);
    item.dataset.sidebarDepth = String(getDepth(item));
    ensureToggle(item);
    setExpanded(item, shouldExpand);
  }

  function enhanceLeaf(item) {
    item.classList.remove("sidebar-folder", "is-folder-expanded", "is-folder-collapsed", "is-active-branch");
    item.classList.toggle("sidebar-section", isInSubSidebar(item));
    item.classList.toggle("sidebar-file", !isInSubSidebar(item) && Boolean(getLabel(item)));
    item.dataset.sidebarDepth = String(getDepth(item));
  }

  function clearSectionNumbers(sidebar) {
    sidebar.querySelectorAll(".sidebar-section-number").forEach(function (number) {
      number.remove();
    });
  }

  function numberSectionList(list, prefix) {
    var index = 0;
    var lastNumber = null;

    Array.prototype.slice.call(list.children).forEach(function (item) {
      var link = getDirectLink(item);
      var current;
      var number;

      if (item.tagName === "UL" && lastNumber) {
        numberSectionList(item, lastNumber);
        return;
      }

      if (item.tagName !== "LI") return;

      index += 1;
      current = prefix.concat(index);
      lastNumber = current;

      if (link) {
        number = document.createElement("span");
        number.className = "sidebar-section-number";
        number.setAttribute("aria-hidden", "true");
        number.textContent = current.join(".") + ".";
        link.insertBefore(number, link.firstChild);
      }
    });
  }

  function isRootSectionList(list) {
    var node = list.parentNode;

    while (node && !(node.classList && node.classList.contains("sidebar-nav"))) {
      if (node.classList && node.classList.contains("app-sub-sidebar")) {
        return false;
      }

      node = node.parentNode;
    }

    return true;
  }

  function numberSections(sidebar) {
    clearSectionNumbers(sidebar);
    Array.prototype.slice.call(sidebar.querySelectorAll(".app-sub-sidebar")).forEach(function (list) {
      if (isRootSectionList(list)) {
        numberSectionList(list, []);
      }
    });
  }

  function getFolderItemFromTarget(target, sidebar) {
    var node = target;

    while (node && node !== sidebar) {
      if (node.classList && node.classList.contains("sidebar-folder-toggle")) {
        return node.parentNode;
      }

      if (node.tagName === "P" && node.parentNode && node.parentNode.classList.contains("sidebar-folder")) {
        return node.parentNode;
      }

      node = node.parentNode;
    }

    return null;
  }

  function bindSidebarEvents(sidebar) {
    if (sidebar.dataset.sidebarTreeBound === "true") return;

    sidebar.dataset.sidebarTreeBound = "true";
    sidebar.addEventListener(
      "click",
      function (event) {
        var item = getFolderItemFromTarget(event.target, sidebar);

        if (!item) return;

        stopEvent(event);
        toggleFolder(item);
      },
      true
    );
    sidebar.addEventListener(
      "keydown",
      function (event) {
        var item;

        if (event.key !== "Enter" && event.key !== " ") return;

        item = getFolderItemFromTarget(event.target, sidebar);
        if (!item) return;

        stopEvent(event);
        toggleFolder(item);
      },
      true
    );
  }

  function shouldStopScrollChain(element, deltaY) {
    var atTop = element.scrollTop <= 0;
    var atBottom = Math.ceil(element.scrollTop + element.clientHeight) >= element.scrollHeight;

    if (!deltaY) return false;
    if (element.scrollHeight <= element.clientHeight) return true;
    return (deltaY < 0 && atTop) || (deltaY > 0 && atBottom);
  }

  function bindSidebarScrollContainment(sidebar) {
    var lastTouchY = 0;

    if (sidebar.dataset.sidebarScrollBound === "true") return;

    sidebar.dataset.sidebarScrollBound = "true";
    sidebar.addEventListener(
      "wheel",
      function (event) {
        if (Math.abs(event.deltaX) > Math.abs(event.deltaY)) return;
        if (!shouldStopScrollChain(sidebar, event.deltaY)) return;

        event.preventDefault();
      },
      { passive: false }
    );
    sidebar.addEventListener(
      "touchstart",
      function (event) {
        if (!event.touches || !event.touches.length) return;
        lastTouchY = event.touches[0].clientY;
      },
      { passive: true }
    );
    sidebar.addEventListener(
      "touchmove",
      function (event) {
        var touch;
        var deltaY;

        if (!event.touches || !event.touches.length) return;

        touch = event.touches[0];
        deltaY = lastTouchY - touch.clientY;
        lastTouchY = touch.clientY;

        if (!shouldStopScrollChain(sidebar, deltaY)) return;

        event.preventDefault();
      },
      { passive: false }
    );
  }

  function enhanceSidebar() {
    var sidebar = document.querySelector(".sidebar .sidebar-nav");

    if (!sidebar) return;

    bindSidebarEvents(sidebar);
    bindSidebarScrollContainment(sidebar);
    numberSections(sidebar);

    Array.prototype.slice.call(sidebar.querySelectorAll("li")).forEach(function (item) {
      if (!isInSubSidebar(item) && getDirectChildList(item)) {
        enhanceFolder(item);
      } else {
        enhanceLeaf(item);
      }
    });
  }

  function scheduleEnhanceSidebar() {
    window.requestAnimationFrame(enhanceSidebar);
    window.setTimeout(enhanceSidebar, 80);
    window.setTimeout(enhanceSidebar, 240);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.mounted(scheduleEnhanceSidebar);
    hook.doneEach(scheduleEnhanceSidebar);
  });
})();
