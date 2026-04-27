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
  const CACHE_VERSION = "v2";

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

  function titleFromPath(path) {
    if (path === "/" || /\/README$/i.test(path || "")) return "README";

    return decodeURIComponent(String(path || "").split("/").pop() || path)
      .replace(/\.md$/i, "")
      .trim();
  }

  function slugify(value) {
    return stripMarkdown(value)
      .toLowerCase()
      .replace(/[^\w\u2e80-\u2eff\u2f00-\u2fdf\u3040-\u30ff\u3100-\u312f\u31a0-\u31bf\u3400-\u4dbf\u4e00-\u9fff\uf900-\ufaff\s-]/g, "")
      .trim()
      .replace(/\s+/g, "-");
  }

  function sectionUrl(path, title) {
    const baseUrl = routeUrl(path);
    const slug = slugify(title);
    return slug ? baseUrl + "?id=" + encodeURIComponent(slug) : baseUrl;
  }

  function parseSections(path, markdown, depth) {
    const markdownText = String(markdown || "");
    const lines = markdownText.replace(/```[\s\S]*?```/g, " ").split(/\r?\n/);
    const fileTitleMatch = markdownText.match(/^#\s+(.+)$/m);
    const fileTitle = stripMarkdown(fileTitleMatch ? fileTitleMatch[1] : titleFromPath(path));
    const sections = [];
    let title = "全文";
    let body = [];

    function push() {
      const content = stripMarkdown(body.join("\n"));
      if (title || content) {
        sections.push({
          path: path,
          fileTitle: fileTitle,
          fileUrl: routeUrl(path),
          sectionTitle: stripMarkdown(title),
          content: content.slice(0, 12000),
          url: title === "全文" ? routeUrl(path) : sectionUrl(path, title),
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
      return item.sectionTitle || item.content;
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
      expires: "docsify.lazy-search." + CACHE_VERSION + ".expires" + namespace,
      index: "docsify.lazy-search." + CACHE_VERSION + ".index" + namespace,
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

  function matchSnippets(content, keyword, limit) {
    const normalized = normalize(content);
    const snippets = [];
    let fromIndex = 0;

    while (snippets.length < limit) {
      const position = normalized.indexOf(keyword, fromIndex);
      if (position < 0) break;

      const start = Math.max(0, position - 42);
      const end = Math.min(content.length, position + keyword.length + 90);
      snippets.push(content.slice(start, end));
      fromIndex = position + keyword.length;
    }

    return snippets;
  }

  function search(index, query) {
    const keyword = normalize(query).trim();
    if (!keyword) return [];

    const files = new Map();

    index.forEach(function (item) {
      const fileTitle = item.fileTitle || item.title || titleFromPath(item.path);
      const sectionTitle = item.sectionTitle || item.title || "全文";
      const fileKey = item.path || item.fileUrl || fileTitle;
      const fileTitleMatch = normalize(fileTitle).indexOf(keyword) >= 0;
      const sectionTitleMatch = normalize(sectionTitle).indexOf(keyword) >= 0;
      const snippets = matchSnippets(item.content || "", keyword, 3);
      const matches = [];
      const hasSectionTitleMatch = sectionTitleMatch && sectionTitle !== fileTitle;
      let sectionScore = 0;
      let fileGroup = files.get(fileKey);

      if (!fileGroup) {
        fileGroup = {
          title: fileTitle,
          url: item.fileUrl || item.url,
          score: 0,
          matchCount: 0,
          titleMatched: false,
          sections: new Map(),
        };
        files.set(fileKey, fileGroup);
      }

      if (fileTitleMatch && !fileGroup.titleMatched) {
        fileGroup.titleMatched = true;
        fileGroup.score += 8;
        fileGroup.matchCount += 1;
      }

      if (hasSectionTitleMatch) {
        sectionScore += 5;
      }

      snippets.forEach(function (snippet) {
        sectionScore += 1;
        matches.push({ label: "正文", text: "..." + snippet + "..." });
      });

      if (!hasSectionTitleMatch && !matches.length) return;

      const sectionKey = item.url || sectionTitle;
      let sectionGroup = fileGroup.sections.get(sectionKey);

      if (!sectionGroup) {
        sectionGroup = {
          title: sectionTitle,
          url: item.url || fileGroup.url,
          score: 0,
          matches: [],
        };
        fileGroup.sections.set(sectionKey, sectionGroup);
      }

      fileGroup.score += sectionScore;
      sectionGroup.score += sectionScore;
      sectionGroup.matches.push.apply(sectionGroup.matches, matches);
      fileGroup.matchCount += matches.length + (hasSectionTitleMatch ? 1 : 0);
    });

    return Array.from(files.values())
      .map(function (file) {
        file.sections = Array.from(file.sections.values())
          .sort(function (a, b) {
            return b.score - a.score;
          })
          .slice(0, 8)
          .map(function (section) {
            section.matches = section.matches.slice(0, 4);
            return section;
          });
        return file;
      })
      .filter(function (file) {
        return file.matchCount > 0;
      })
      .sort(function (a, b) {
        return b.score - a.score;
      })
      .slice(0, 12);
  }

  function renderResults(results, query) {
    return results
      .map(function (file) {
        const sectionsHtml = file.sections
          .map(function (section) {
            const matchesHtml = section.matches
              .map(function (match) {
            return (
              '<li><a href="' +
              escapeHtml(section.url) +
              '"><span class="search-match-label">' +
              escapeHtml(match.label) +
              '</span><span class="search-match-text">' +
              highlight(match.text, query) +
              "</span></a></li>"
            );
          })
          .join("");
        const matchesBlock = matchesHtml
          ? '<ul class="search-matches">' + matchesHtml + "</ul>"
          : "";

        return (
          '<div class="search-section"><a class="search-section-title" href="' +
          escapeHtml(section.url) +
          '"><span class="search-dim search-dim-section">章节</span><span class="search-section-text">' +
          highlight(section.title, query) +
          "</span></a>" +
          matchesBlock +
          "</div>"
        );
      })
      .join("");

    return (
      '<div class="search-file"><a class="search-file-title" href="' +
      escapeHtml(file.url) +
      '"><span class="search-dim search-dim-file">文件</span><strong>' +
      highlight(file.title, query) +
      '</strong><span class="search-count">' +
          file.matchCount +
          " 处</span></a>" +
          sectionsHtml +
          "</div>"
        );
      })
      .join("");
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
      const html = renderResults(results, query);

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
        .search a { text-decoration: none; }
        .search .search-file { border-bottom: 1px solid #eee; padding: 10px 0 8px; }
        .search .search-file-title { align-items: center; color: inherit; display: flex; gap: 6px; line-height: 1.35; }
        .search .search-file-title strong { flex: 1; font-family: "PingFang SC", "Microsoft YaHei", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; font-size: 15px; font-weight: 700; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
        .search .search-count { color: #999; flex: 0 0 auto; font-size: 12px; }
        .search .search-section { margin: 8px 0 0 14px; }
        .search .search-section-title { align-items: flex-start; color: inherit; display: flex; font-size: 13px; gap: 5px; line-height: 1.35; min-width: 0; }
        .search .search-section-text { flex: 1; font-family: "Songti SC", "STSong", "SimSun", serif; font-weight: 600; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
        .search .search-matches { border-left: 2px solid #f0f0f0; list-style: none; margin: 5px 0 0 18px; padding: 0 0 0 10px; }
        .search .search-matches li { margin: 4px 0; }
        .search .search-matches a { align-items: flex-start; color: #666; display: flex; font-family: "Kaiti SC", "STKaiti", "KaiTi", cursive; font-size: 12px; gap: 5px; line-height: 1.5; max-height: 3.2em; overflow: hidden; }
        .search .search-match-text { flex: 1; min-width: 0; }
        .search .search-match-label,
        .search .search-dim { border: 1px solid #ddd; border-radius: 3px; color: #888; display: inline-block; flex: 0 0 auto; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; font-size: 11px; line-height: 1.2; padding: 1px 3px; vertical-align: 1px; }
        .search .search-dim-file { border-color: #b9d8ff; color: #2f6db3; }
        .search .search-dim-section { border-color: #d8c7ef; color: #7553a6; }
        .search .search-match-label { border-color: #d8ead1; color: #4b8a3f; }
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
