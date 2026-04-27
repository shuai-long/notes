(function () {
  "use strict";

  const DEFAULTS = {
    maxAge: 86400000,
    paths: "auto",
    placeholder: "搜索",
    noData: "No Results",
    depth: 4,
    hideOtherSidebarContent: false,
    namespace: "",
    loadingText: "索引加载中...",
  };

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function normalize(value) {
    return String(value || "").toLowerCase();
  }

  function stripMarkdown(markdown) {
    return String(markdown || "")
      .replace(/```[\s\S]*?```/g, " ")
      .replace(/`([^`]+)`/g, "$1")
      .replace(/!\[[^\]]*]\([^)]*\)/g, " ")
      .replace(/\[([^\]]+)]\([^)]*\)/g, "$1")
      .replace(/<[^>]+>/g, " ")
      .replace(/[#>*_~|`-]+/g, " ")
      .replace(/\s+/g, " ")
      .trim();
  }

  function routeUrl(path) {
    let route = path || "/";
    route = route.replace(/\.md$/i, "");
    route = route.replace(/\/README$/i, "/");
    if (!route.startsWith("/")) route = "/" + route;
    return "#" + route;
  }

  function parseSections(path, markdown, depth) {
    const lines = String(markdown || "").replace(/```[\s\S]*?```/g, " ").split(/\r?\n/);
    const sections = [];
    let title = path === "/" ? "README" : decodeURIComponent(path.split("/").pop() || path);
    let body = [];

    function push() {
      const content = stripMarkdown(body.join("\n"));
      if (title || content) {
        sections.push({
          title: stripMarkdown(title),
          content: content.slice(0, 12000),
          url: routeUrl(path),
        });
      }
      body = [];
    }

    lines.forEach(function (line) {
      const match = /^(#{1,6})\s+(.+)$/.exec(line);
      if (match && match[1].length <= depth) {
        push();
        title = match[2];
        return;
      }
      body.push(line);
    });

    push();
    return sections.filter(function (item) {
      return item.title || item.content;
    });
  }

  function isExternalLink(href) {
    return /^(?:[a-z]+:)?\/\//i.test(href || "") || /^(?:mailto|tel):/i.test(href || "");
  }

  function collectPaths(config, vm) {
    if (Array.isArray(config.paths)) {
      return config.paths.slice();
    }

    const paths = [];
    const links = document.querySelectorAll(".sidebar-nav a:not(.section-link):not([data-nosearch])");

    links.forEach(function (link) {
      const href = link.getAttribute("href") || "";
      if (!href || href === "#" || isExternalLink(href)) return;

      const parsed = vm.router.parse(link.href);
      const path = parsed && parsed.path;

      if (path && paths.indexOf(path) === -1) {
        paths.push(path);
      }
    });

    if (paths.indexOf("/") === -1 && paths.indexOf("/README") === -1) {
      paths.unshift("/");
    }

    return paths;
  }

  function cacheKeys(config) {
    const namespace = config.namespace ? "/" + config.namespace : "";
    return {
      expires: "docsify.lazy-search.expires" + namespace,
      index: "docsify.lazy-search.index" + namespace,
    };
  }

  function readCache(config) {
    try {
      const keys = cacheKeys(config);
      const expires = Number(localStorage.getItem(keys.expires));
      if (!expires || expires < Date.now()) return null;
      return JSON.parse(localStorage.getItem(keys.index) || "null");
    } catch (error) {
      return null;
    }
  }

  function writeCache(config, index) {
    try {
      const keys = cacheKeys(config);
      localStorage.setItem(keys.expires, String(Date.now() + config.maxAge));
      localStorage.setItem(keys.index, JSON.stringify(index));
    } catch (error) {
      // localStorage can be full or disabled. Search still works for this session.
    }
  }

  function fetchIndex(config, vm) {
    const cached = readCache(config);
    if (cached) return Promise.resolve(cached);

    const paths = collectPaths(config, vm);
    const queue = paths.slice();
    const index = [];
    const concurrency = 6;

    function worker() {
      const path = queue.shift();
      if (!path) return Promise.resolve();

      return Promise.resolve(Docsify.get(vm.router.getFile(path), false, vm.config.requestHeaders))
        .then(function (markdown) {
          index.push.apply(index, parseSections(path, markdown, config.depth));
        }, function () {
          // Ignore missing pages from stale sidebar links.
        })
        .then(worker);
    }

    return Promise.all(
      Array.from({ length: Math.min(concurrency, queue.length) }, worker)
    ).then(function () {
      writeCache(config, index);
      return index;
    });
  }

  function highlight(text, query) {
    const value = escapeHtml(text || "");
    if (!query) return value;
    const escapedQuery = query.replace(/[|\\{}()[\]^$+*?.]/g, "\\$&");
    return value.replace(new RegExp(escapedQuery, "gi"), function (match) {
      return '<em class="search-keyword">' + match + "</em>";
    });
  }

  function search(index, query) {
    const keyword = normalize(query).trim();
    if (!keyword) return [];

    return index
      .map(function (item) {
        const title = normalize(item.title);
        const content = normalize(item.content);
        const titlePos = title.indexOf(keyword);
        const contentPos = content.indexOf(keyword);
        let score = 0;

        if (titlePos >= 0) score += 4;
        if (contentPos >= 0) score += 1;
        if (!score) return null;

        const start = Math.max(0, contentPos - 40);
        const end = Math.min(item.content.length, contentPos + keyword.length + 90);
        const snippet = contentPos >= 0 ? item.content.slice(start, end) : item.content.slice(0, 120);

        return {
          title: item.title,
          content: snippet,
          url: item.url,
          score: score,
        };
      })
      .filter(Boolean)
      .sort(function (a, b) {
        return b.score - a.score;
      })
      .slice(0, 30);
  }

  function plugin(hook, vm) {
    const config = Object.assign({}, DEFAULTS, vm.config.search || {});
    let index = null;
    let indexPromise = null;
    let input;
    let panel;
    let clearButton;
    let sidebarNav;
    let appName;

    function ensureIndex() {
      if (index) return Promise.resolve(index);
      if (!indexPromise) {
        indexPromise = fetchIndex(config, vm).then(function (result) {
          index = result;
          return index;
        });
      }
      return indexPromise;
    }

    function render(query) {
      if (!query) {
        panel.classList.remove("show");
        clearButton.classList.remove("show");
        panel.innerHTML = "";
        if (config.hideOtherSidebarContent) {
          sidebarNav && sidebarNav.classList.remove("hide");
          appName && appName.classList.remove("hide");
        }
        return;
      }

      clearButton.classList.add("show");

      if (!index) {
        panel.classList.add("show");
        panel.innerHTML = '<p class="empty">' + escapeHtml(config.loadingText) + "</p>";
        ensureIndex().then(function () {
          render(input.value);
        });
        return;
      }

      const results = search(index, query);
      const html = results
        .map(function (item) {
          return (
            '<div class="matching-post"><a href="' +
            item.url +
            '"><h2>' +
            highlight(item.title, query) +
            "</h2><p>" +
            highlight("..." + item.content + "...", query) +
            "</p></a></div>"
          );
        })
        .join("");

      panel.classList.add("show");
      panel.innerHTML = html || '<p class="empty">' + escapeHtml(config.noData) + "</p>";

      if (config.hideOtherSidebarContent) {
        sidebarNav && sidebarNav.classList.add("hide");
        appName && appName.classList.add("hide");
      }
    }

    hook.mounted(function () {
      Docsify.dom.style(`
        .sidebar { padding-top: 0; }
        .search { margin-bottom: 20px; padding: 6px; border-bottom: 1px solid #eee; }
        .search .input-wrap { display: flex; align-items: center; }
        .search .results-panel { display: none; }
        .search .results-panel.show { display: block; }
        .search input { outline: none; border: 1px solid transparent; width: 100%; padding: 0.6em 7px; font-size: inherit; }
        .search input:focus { box-shadow: 0 0 5px var(--theme-color, #42b983); border: 1px solid var(--theme-color, #42b983); }
        .search .clear-button { cursor: pointer; width: 36px; text-align: right; display: none; border: 0; background: transparent; }
        .search .clear-button.show { display: block; }
        .search h2 { font-size: 17px; margin: 10px 0; }
        .search a { text-decoration: none; }
        .search .matching-post { border-bottom: 1px solid #eee; }
        .search .matching-post p { overflow: hidden; text-overflow: ellipsis; font-size: 14px; }
        .search .search-keyword { color: var(--theme-color, #42b983); font-style: normal; font-weight: bold; }
        .search .empty { color: #777; font-size: 14px; }
        .sidebar-nav.hide, .app-name.hide { display: none; }
      `);

      const sidebar = document.querySelector(".sidebar");
      sidebarNav = document.querySelector(".sidebar-nav");
      appName = document.querySelector(".app-name");
      if (!sidebar || !sidebarNav) return;

      const searchBox = document.createElement("div");
      searchBox.className = "search";
      searchBox.innerHTML =
        '<div class="input-wrap"><input type="search" placeholder="' +
        escapeHtml(config.placeholder) +
        '"><button class="clear-button" type="button" aria-label="清空搜索">×</button></div><div class="results-panel"></div>';

      sidebar.insertBefore(searchBox, sidebarNav);

      input = searchBox.querySelector("input");
      panel = searchBox.querySelector(".results-panel");
      clearButton = searchBox.querySelector(".clear-button");

      input.addEventListener("focus", ensureIndex);
      input.addEventListener("input", function () {
        render(input.value);
      });
      clearButton.addEventListener("click", function () {
        input.value = "";
        input.focus();
        render("");
      });
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(plugin);
})();
