(function () {
  var observer = null;
  var searchTarget = null;
  var searchIndexCache = null;
  var SEARCH_TARGET_KEY = "docsify.search.enhance.target";

  function normalizeText(text) {
    return (text || "").replace(/\s+/g, " ").trim();
  }

  function safeDecode(text) {
    try {
      return decodeURIComponent(text);
    } catch (error) {
      return text;
    }
  }

  function decodeText(text) {
    if (!/%[0-9a-f]{2}/i.test(text || "")) return text || "";

    return safeDecode(text);
  }

  function normalizeResultUrl(url) {
    var normalized = url || "#/";
    var hashIndex = normalized.indexOf("#");

    if (hashIndex >= 0) return normalized.slice(hashIndex);
    if (normalized.charAt(0) === "/") return "#" + normalized;
    if (normalized.charAt(0) === "#") return normalized;

    return "#/" + normalized.replace(/^\/+/, "");
  }

  function parseHashUrl(url) {
    var normalized = normalizeResultUrl(url || window.location.hash || "#/");
    var route = normalized.charAt(0) === "#" ? normalized.slice(1) : normalized;
    var queryIndex = route.indexOf("?");
    var path = queryIndex >= 0 ? route.slice(0, queryIndex) : route;
    var query = queryIndex >= 0 ? route.slice(queryIndex + 1) : "";
    var idMatch = query.match(/(?:^|&)id=([^&]*)/i);
    var id = idMatch ? safeDecode(idMatch[1].replace(/\+/g, " ")) : "";

    if (path.charAt(0) !== "/") path = "/" + path;

    return {
      id: decodeText(id),
      path: decodeText(path).replace(/\/+$/, "") || "/",
      url: normalized,
    };
  }

  function getCurrentRoute() {
    return parseHashUrl(window.location.hash || "#/");
  }

  function routeMatches(target, current) {
    return target && current && target.path === current.path;
  }

  function getFileName(url) {
    var hashIndex = url.indexOf("#");
    var route = hashIndex >= 0 ? url.slice(hashIndex + 1) : url;
    var queryIndex = route.indexOf("?");
    var path = queryIndex >= 0 ? route.slice(0, queryIndex) : route;
    var parts = path.split("/").filter(Boolean);
    var name = parts.length ? parts[parts.length - 1] : "首页";

    return decodeText(name).replace(/\.md$/i, "") || "首页";
  }

  function decodeTextNodes(root) {
    var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null);
    var node;
    var nextText;

    while ((node = walker.nextNode())) {
      nextText = decodeText(node.nodeValue);

      if (nextText !== node.nodeValue) {
        node.nodeValue = nextText;
      }
    }
  }

  function createTextElement(tagName, className, text) {
    var element = document.createElement(tagName);

    element.className = className;
    element.textContent = decodeText(text || "");
    return element;
  }

  function createContentFragment(html) {
    var template = document.createElement("template");

    template.innerHTML = html || "";
    decodeTextNodes(template.content);
    return template.content;
  }

  function escapeHtml(text) {
    return String(text || "").replace(/[&<>"']/g, function (char) {
      return {
        "&": "&amp;",
        "<": "&lt;",
        ">": "&gt;",
        '"': "&quot;",
        "'": "&#39;",
      }[char];
    });
  }

  function escapeRegExp(text) {
    return String(text || "").replace(/[|\\{}()[\]^$+*?.]/g, "\\$&");
  }

  function getSearchTerms() {
    var input = document.querySelector('.sidebar .search input[type="search"]');
    var value = normalizeText(input && input.value);
    var terms = [];

    if (!value) return terms;

    [value].concat(value.split(/[\s\-，\\/]+/)).forEach(function (term) {
      term = normalizeText(term);
      if (term && terms.indexOf(term) === -1) {
        terms.push(term);
      }
    });

    return terms.slice(0, 8);
  }

  function getSearchIndexes() {
    var indexes = [];
    var index;
    var key;

    if (searchIndexCache) return searchIndexCache;

    try {
      for (index = 0; index < window.localStorage.length; index += 1) {
        key = window.localStorage.key(index);

        if (key && key.indexOf("docsify.search.index") === 0) {
          indexes.push(JSON.parse(window.localStorage.getItem(key) || "{}"));
        }
      }
    } catch (error) {
      indexes = [];
    }

    searchIndexCache = indexes;
    return searchIndexCache;
  }

  function getRecordRoute(record, slug) {
    return parseHashUrl((record && record.slug) || slug || "");
  }

  function findIndexedRecord(url) {
    var target = parseHashUrl(url);
    var indexes = getSearchIndexes();
    var index;
    var path;
    var slug;
    var record;
    var route;

    for (index = 0; index < indexes.length; index += 1) {
      for (path in indexes[index]) {
        if (!Object.prototype.hasOwnProperty.call(indexes[index], path)) continue;

        for (slug in indexes[index][path]) {
          if (!Object.prototype.hasOwnProperty.call(indexes[index][path], slug)) continue;

          record = indexes[index][path][slug];
          route = getRecordRoute(record, slug);

          if (route.path === target.path && (!target.id || route.id === target.id)) {
            return record;
          }
        }
      }
    }

    return null;
  }

  function findExcerptStart(body, terms) {
    var lowerBody = body.toLowerCase();
    var bestIndex = -1;

    terms.forEach(function (term) {
      var index = lowerBody.indexOf(term.toLowerCase());

      if (index >= 0 && (bestIndex < 0 || index < bestIndex)) {
        bestIndex = index;
      }
    });

    return bestIndex;
  }

  function highlightTerms(text, terms) {
    var pattern;
    var lowerTerms;

    if (!terms.length) return escapeHtml(text);

    pattern = new RegExp("(" + terms.map(escapeRegExp).join("|") + ")", "gi");
    lowerTerms = terms.map(function (term) {
      return term.toLowerCase();
    });

    return text
      .split(pattern)
      .map(function (part) {
        if (lowerTerms.indexOf(part.toLowerCase()) >= 0) {
          return '<em class="search-keyword">' + escapeHtml(part) + "</em>";
        }

        return escapeHtml(part);
      })
      .join("");
  }

  function createLongerContent(url, fallbackHtml, terms) {
    var record = findIndexedRecord(url);
    var body = normalizeText(record && record.body);
    var startIndex;
    var start;
    var end;
    var excerpt;

    if (!body) return fallbackHtml || "";

    startIndex = findExcerptStart(body, terms);
    start = startIndex < 0 ? 0 : Math.max(0, startIndex - 40);
    end = startIndex < 0 ? 260 : Math.min(body.length, startIndex + 220);
    excerpt = (start > 0 ? "..." : "") + body.slice(start, end) + (end < body.length ? "..." : "");

    return highlightTerms(excerpt, terms);
  }

  function getResultContentText(link) {
    var clone = link.cloneNode(true);
    var title = clone.querySelector(".search-result-section-title");

    if (title) title.remove();

    return normalizeText(clone.textContent.replace(/\.\.\./g, " "));
  }

  function getResultKeywords(link) {
    var keywords = [];

    link.querySelectorAll(".search-keyword").forEach(function (keyword) {
      var text = normalizeText(keyword.textContent);

      if (text && keywords.indexOf(text) === -1) {
        keywords.push(text);
      }
    });

    return keywords.slice(0, 5);
  }

  function persistSearchTarget(target) {
    searchTarget = target;

    try {
      window.sessionStorage.setItem(SEARCH_TARGET_KEY, JSON.stringify(target));
    } catch (error) {
      // sessionStorage may be unavailable under file:// or strict browser settings.
    }
  }

  function readSearchTarget() {
    if (searchTarget) return searchTarget;

    try {
      searchTarget = JSON.parse(window.sessionStorage.getItem(SEARCH_TARGET_KEY) || "null");
    } catch (error) {
      searchTarget = null;
    }

    return searchTarget;
  }

  function clearSearchTarget(target) {
    if (target && searchTarget && target.time !== searchTarget.time) return;

    searchTarget = null;

    try {
      window.sessionStorage.removeItem(SEARCH_TARGET_KEY);
    } catch (error) {
      // sessionStorage may be unavailable under file:// or strict browser settings.
    }
  }

  function storeSearchTarget(link) {
    var route = parseHashUrl(link.getAttribute("href") || link.href || "");
    var title = link.querySelector(".search-result-section-title");

    persistSearchTarget({
      content: getResultContentText(link),
      id: route.id,
      keywords: getResultKeywords(link),
      path: route.path,
      time: Date.now(),
      title: normalizeText(title && title.textContent),
    });
  }

  function getTopMargin() {
    var configured = window.$docsify && Number(window.$docsify.topMargin);
    var barHeight = 0;
    var beforeStyle;

    if (configured) return configured;

    try {
      beforeStyle = window.getComputedStyle(document.body, "::before");
      barHeight = parseFloat(beforeStyle.height) || 0;
    } catch (error) {
      barHeight = 0;
    }

    return Math.max(barHeight + 12, 12);
  }

  function cssString(value) {
    if (window.CSS && typeof window.CSS.escape === "function") {
      return window.CSS.escape(value);
    }

    return String(value).replace(/["\\]/g, "\\$&");
  }

  function findElementById(id) {
    var candidates;
    var index;
    var element;
    var anchor;

    if (!id) return null;

    candidates = [
      id,
      id.toLowerCase(),
      decodeText(id),
      decodeText(id).toLowerCase(),
      encodeURIComponent(id).toLowerCase(),
    ];

    for (index = 0; index < candidates.length; index += 1) {
      element = document.getElementById(candidates[index]);
      if (element) return element;
    }

    try {
      anchor = document.querySelector('.anchor[data-id="' + cssString(id) + '"]');
    } catch (error) {
      anchor = null;
    }

    return anchor ? anchor.closest("h1,h2,h3,h4,h5,h6") || anchor : null;
  }

  function stripHeadingNumber(text) {
    return text.replace(/^\s*(?:\d+\.)+\s*/, "");
  }

  function normalizeComparable(text) {
    return stripHeadingNumber(decodeText(text || ""))
      .replace(/\s+/g, "")
      .replace(/[^\w\u3400-\u4dbf\u4e00-\u9fff\uf900-\ufaff]/g, "")
      .toLowerCase();
  }

  function getHeadingText(heading) {
    var clone = heading.cloneNode(true);

    clone.querySelectorAll(".heading-number").forEach(function (number) {
      number.remove();
    });

    return normalizeText(clone.textContent);
  }

  function findHeadingByText(title) {
    var targetText = normalizeComparable(title);
    var headings;
    var index;
    var heading;
    var headingText;

    if (!targetText) return null;

    headings = document.querySelectorAll(".markdown-section h1, .markdown-section h2, .markdown-section h3, .markdown-section h4, .markdown-section h5, .markdown-section h6");

    for (index = 0; index < headings.length; index += 1) {
      heading = headings[index];
      headingText = normalizeComparable(getHeadingText(heading));

      if (headingText === targetText || headingText.indexOf(targetText) >= 0) {
        return heading;
      }
    }

    return null;
  }

  function getHeadingLevel(element) {
    return element && /^H[1-6]$/.test(element.tagName) ? Number(element.tagName.slice(1)) : 0;
  }

  function textContainsKeyword(text, keywords) {
    var comparable = normalizeComparable(text);
    var index;
    var keyword;

    if (!comparable) return false;

    for (index = 0; index < keywords.length; index += 1) {
      keyword = normalizeComparable(keywords[index]);

      if (keyword && comparable.indexOf(keyword) >= 0) return true;
    }

    return false;
  }

  function addContentCandidates(node, candidates) {
    if (!node || !node.matches) return;

    if (node.matches("p,li,td,blockquote,pre")) {
      candidates.push(node);
      return;
    }

    node.querySelectorAll("p,li,td,blockquote,pre").forEach(function (candidate) {
      candidates.push(candidate);
    });
  }

  function findContentTarget(heading, keywords) {
    var root = document.querySelector(".markdown-section");
    var candidates = [];
    var current;
    var headingLevel;
    var index;

    if (!root || !keywords || !keywords.length) return null;

    if (heading) {
      headingLevel = getHeadingLevel(heading);
      current = heading.nextElementSibling;

      while (current) {
        if (getHeadingLevel(current) && getHeadingLevel(current) <= headingLevel) break;

        addContentCandidates(current, candidates);
        current = current.nextElementSibling;
      }
    } else {
      root.querySelectorAll("p,li,td,blockquote,pre").forEach(function (candidate) {
        candidates.push(candidate);
      });
    }

    for (index = 0; index < candidates.length; index += 1) {
      if (textContainsKeyword(candidates[index].textContent, keywords)) {
        return candidates[index];
      }
    }

    return null;
  }

  function scrollToTarget(element) {
    var top;

    if (!element) return false;

    top = Math.max(element.getBoundingClientRect().top + window.pageYOffset - getTopMargin(), 0);
    window.scrollTo(0, top);
    return true;
  }

  function repairCurrentSearchJump() {
    var target = readSearchTarget();
    var current = getCurrentRoute();
    var heading;
    var contentTarget;

    if (!target || !routeMatches(target, current)) return;
    if (target.time && Date.now() - target.time > 10000) return;

    heading = findElementById(current.id || target.id) || findHeadingByText(target.title);
    contentTarget = findContentTarget(heading, target.keywords);

    if (scrollToTarget(contentTarget || heading)) {
      window.setTimeout(function () {
        clearSearchTarget(target);
      }, 1500);
    }
  }

  function scheduleRepairSearchJump() {
    window.requestAnimationFrame(function () {
      repairCurrentSearchJump();
      window.setTimeout(repairCurrentSearchJump, 120);
      window.setTimeout(repairCurrentSearchJump, 420);
    });
  }

  function bindResultClick() {
    if (document.documentElement.dataset.searchResultClickBound === "true") return;

    document.documentElement.dataset.searchResultClickBound = "true";
    document.addEventListener("click", function (event) {
      var link = event.target.closest && event.target.closest(".sidebar .search .results-panel a");

      if (!link) return;

      storeSearchTarget(link);
      scheduleRepairSearchJump();
    });

    window.addEventListener("hashchange", scheduleRepairSearchJump);
  }

  function groupDefaultResults(panel) {
    var posts = Array.prototype.slice.call(panel.querySelectorAll(".matching-post"));
    var groups = [];
    var groupMap = Object.create(null);
    var fragment = document.createDocumentFragment();
    var searchTerms = getSearchTerms();

    if (!posts.length) return;

    posts.forEach(function (post) {
      var link = post.querySelector("a");
      var title = post.querySelector("h2");
      var content = post.querySelector("p");
      var url;
      var file;
      var group;

      if (!link) return;

      url = link.getAttribute("href") || link.href || "";
      file = getFileName(url);
      group = groupMap[file];

      if (!group) {
        group = {
          file: file,
          items: [],
        };
        groupMap[file] = group;
        groups.push(group);
      }

      group.items.push({
        url: url,
        title: decodeText(normalizeText(title && title.textContent)) || file,
        content: createLongerContent(url, content ? content.innerHTML : "", searchTerms),
      });
    });

    groups.forEach(function (group) {
      var fileNode = document.createElement("div");

      fileNode.className = "search-result-file";
      fileNode.appendChild(createTextElement("div", "search-result-file-title", group.file));

      group.items.forEach(function (item) {
        var section = document.createElement("div");
        var link = document.createElement("a");
        var content = document.createElement("div");

        section.className = "search-result-section";
        link.setAttribute("href", normalizeResultUrl(item.url));
        link.className = "search-result-link";
        link.appendChild(createTextElement("strong", "search-result-section-title", item.title));
        content.className = "search-result-content";
        content.appendChild(createContentFragment(item.content));
        if (normalizeText(content.textContent)) {
          link.appendChild(content);
        }
        section.appendChild(link);
        fileNode.appendChild(section);
      });

      fragment.appendChild(fileNode);
    });

    panel.dataset.searchEnhanced = "true";
    panel.innerHTML = "";
    panel.appendChild(fragment);
  }

  function enhanceSearchPanel() {
    var panel = document.querySelector(".sidebar .search .results-panel");

    if (!panel) return;
    if (panel.dataset.searchEnhancing === "true") return;
    if (!panel.querySelector(".matching-post")) return;

    panel.dataset.searchEnhancing = "true";
    groupDefaultResults(panel);
    panel.dataset.searchEnhancing = "false";
  }

  function bindObserver() {
    var panel = document.querySelector(".sidebar .search .results-panel");

    if (!panel || panel.dataset.searchObserverBound === "true") return;

    panel.dataset.searchObserverBound = "true";
    observer = new MutationObserver(function () {
      window.requestAnimationFrame(enhanceSearchPanel);
    });
    observer.observe(panel, {
      childList: true,
      subtree: false,
    });
  }

  function scheduleEnhance() {
    window.requestAnimationFrame(function () {
      bindResultClick();
      bindObserver();
      enhanceSearchPanel();
      scheduleRepairSearchJump();
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.mounted(scheduleEnhance);
    hook.doneEach(scheduleEnhance);
  });
})();
