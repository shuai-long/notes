(function () {
  var DEFAULT_CONFIG = {
    htmlLabel: "HTML",
    openText: "打开",
    pdfLabel: "PDF",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.fileEmbed) || {});
  var routePattern = "^.*\\.(pdf|html?)$";
  var fencePattern = /(^|\n)```([^\n`]*)\n([\s\S]*?)\n```/gi;

  function escapeHtml(text) {
    return String(text || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function getExtension(src) {
    var clean = String(src || "").split(/[?#]/)[0].toLowerCase();
    var match = clean.match(/\.([a-z0-9]+)$/);

    return match ? match[1] : "";
  }

  function getFileName(src) {
    var clean = String(src || "").split(/[?#]/)[0];
    var name = clean.split("/").filter(Boolean).pop() || clean;

    try {
      return decodeURIComponent(name);
    } catch (error) {
      return name;
    }
  }

  function isExternal(src) {
    return /^(?:[a-z][a-z0-9+.-]*:)?\/\//i.test(src) || /^(?:data|blob):/i.test(src);
  }

  function cleanPath(path) {
    var protocol = "";

    if (/^[a-z][a-z0-9+.-]*:\/\//i.test(path)) return path;

    if (path.indexOf("//") === 0) {
      protocol = "//";
      path = path.slice(2);
    }

    path = path.replace(/\/{2,}/g, "/");

    return protocol + path;
  }

  function normalizeRelative(src, routePath) {
    var base;
    var segments;

    if (!/^\.\.?\//.test(src)) return src;

    base = (routePath || "/").replace(/[?#].*$/, "").replace(/\/[^/]*$/, "/");
    segments = (base + src).split("/");

    return cleanPath(
      segments
        .reduce(function (result, segment) {
          if (!segment || segment === ".") return result;
          if (segment === "..") {
            result.pop();
          } else {
            result.push(segment);
          }

          return result;
        }, [])
        .join("/")
    ).replace(/^([^/])/, "/$1");
  }

  function normalizeProjectPath(src) {
    var projectName = window.$docsify && window.$docsify.name;
    var prefix;

    if (!projectName || src.charAt(0) !== "/") return src;

    prefix = "/" + projectName + "/";

    if (src.indexOf(prefix) === 0 && window.location.pathname.indexOf(prefix) !== 0) {
      return "/" + src.slice(prefix.length);
    }

    return src;
  }

  function normalizeSource(src, routePath) {
    src = String(src || "").trim();

    if (!src || isExternal(src) || src.charAt(0) === "#") return src;

    src = normalizeRelative(src, routePath);
    src = normalizeProjectPath(src);

    return cleanPath(src);
  }

  function getTypeFromSource(src, fallbackType) {
    var ext = getExtension(src);

    if (ext === "pdf") return "pdf";
    if (ext === "html" || ext === "htm") return "html";

    return fallbackType || "";
  }

  function renderEmbed(source, type, routePath) {
    var src = normalizeSource(source, routePath);
    var safeSrc = escapeHtml(src);
    var label = type === "pdf" ? config.pdfLabel : config.htmlLabel;
    var title = getFileName(src);
    var viewer;

    if (!src) return "";

    if (type === "pdf") {
      viewer =
        '<object class="docs-file-embed-viewer" data="' +
        safeSrc +
        '" type="application/pdf">' +
        '<iframe class="docs-file-embed-viewer" src="' +
        safeSrc +
        '" title="' +
        escapeHtml(title) +
        '"></iframe>' +
        '<p class="docs-file-embed-fallback">无法直接预览 PDF，<a href="' +
        safeSrc +
        '" target="_blank" rel="noopener">打开文件</a>。</p>' +
        "</object>";
    } else {
      viewer =
        '<iframe class="docs-file-embed-viewer" src="' +
        safeSrc +
        '" title="' +
        escapeHtml(title) +
        '" loading="lazy"></iframe>';
    }

    return [
      '<div class="docs-file-embed docs-file-embed-' + escapeHtml(type) + '">',
      '<div class="docs-file-embed-toolbar">',
      '<span class="docs-file-embed-type">' + escapeHtml(label) + "</span>",
      '<span class="docs-file-embed-title" title="' + escapeHtml(title) + '">' + escapeHtml(title) + "</span>",
      '<a class="docs-file-embed-action" href="' + safeSrc + '" target="_blank" rel="noopener">' + escapeHtml(config.openText) + "</a>",
      "</div>",
      viewer,
      "</div>",
    ].join("");
  }

  function parseFence(langLine, body, routePath) {
    var lang = String(langLine || "").trim().split(/\s+/)[0].toLowerCase();
    var lines = String(body || "")
      .split(/\r?\n/)
      .map(function (line) {
        return line.trim();
      })
      .filter(Boolean);
    var source = lines[0] || "";
    var type;

    if (lines.length !== 1) return null;
    if (!/^(pdf|html|htm|iframe|embed)$/i.test(lang)) return null;
    if (/[<>]/.test(source)) return null;

    type = getTypeFromSource(source, lang === "pdf" ? "pdf" : "");

    if (type !== "pdf" && type !== "html") return null;

    return renderEmbed(source, type, routePath);
  }

  function transformFencedEmbeds(content, routePath) {
    return String(content || "").replace(fencePattern, function (match, prefix, langLine, body) {
      var embed = parseFence(langLine, body, routePath);

      return embed ? prefix + embed : match;
    });
  }

  function extractWrapperSource(content, expectedType) {
    var transformed = transformFencedEmbeds(content, "");

    if (transformed !== String(content || "")) return transformed;

    content = String(content || "").trim();

    if (!/\r?\n/.test(content) && getTypeFromSource(content, expectedType) === expectedType) {
      return renderEmbed(content, expectedType, "");
    }

    return "";
  }

  function getWrapperUrl(vm, path) {
    if (vm.router && typeof vm.router.getFile === "function") {
      return vm.router.getFile(path);
    }

    return path + ((vm.config && vm.config.ext) || ".md");
  }

  function requestText(url, onSuccess, onError) {
    var xhr = new XMLHttpRequest();

    xhr.open("GET", url, true);
    xhr.onload = function () {
      if ((xhr.status >= 200 && xhr.status < 300) || xhr.status === 0) {
        onSuccess(xhr.responseText);
      } else {
        onError();
      }
    };
    xhr.onerror = onError;
    xhr.send();
  }

  function registerRoutes(vm) {
    var routes = vm.config.routes || (vm.config.routes = {});

    if (routes[routePattern]) return;

    routes[routePattern] = function (path, match, next) {
      var type = getTypeFromSource(path);

      if (type === "html") {
        next(renderEmbed(path, "html", path));
        return;
      }

      requestText(
        getWrapperUrl(vm, path),
        function (content) {
          next(extractWrapperSource(content, "pdf") || renderEmbed(path, "pdf", path));
        },
        function () {
          next(renderEmbed(path, "pdf", path));
        }
      );
    };
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
    registerRoutes(vm);

    hook.beforeEach(function (content, next) {
      next(transformFencedEmbeds(content, vm.route && vm.route.path));
    });
  });
})();
