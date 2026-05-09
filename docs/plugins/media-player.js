(function () {
  var DEFAULT_CONFIG = {
    audioExtensions: ["mp3", "wav", "ogg", "m4a", "aac", "flac", "opus"],
    audioLabel: "Audio",
    openText: "打开",
    preload: "metadata",
    videoExtensions: ["mp4", "webm", "ogv", "mov", "m4v"],
    videoLabel: "Video",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.mediaPlayer) || {});
  var audioExtensions = config.audioExtensions.map(function (ext) {
    return String(ext).toLowerCase();
  });
  var videoExtensions = config.videoExtensions.map(function (ext) {
    return String(ext).toLowerCase();
  });
  var mediaExtensions = audioExtensions.concat(videoExtensions);
  var routePattern = "^.*\\.(" + mediaExtensions.join("|") + ")$";
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

  function getMediaTypeFromSource(src, fallbackType) {
    var ext = getExtension(src);

    if (audioExtensions.indexOf(ext) > -1) return "audio";
    if (videoExtensions.indexOf(ext) > -1) return "video";

    return fallbackType || "";
  }

  function parseOptions(lines, routePath) {
    var options = {};
    var valid = true;

    lines.slice(1).forEach(function (line) {
      var index = line.indexOf("=");
      var key;
      var value;

      if (index < 1) {
        valid = false;
        return;
      }

      key = line.slice(0, index).trim().toLowerCase();
      value = line.slice(index + 1).trim();

      if (key === "poster") {
        options.poster = normalizeSource(value, routePath);
      } else if (key === "title") {
        options.title = value;
      } else {
        valid = false;
      }
    });

    return valid ? options : null;
  }

  function renderPlayer(source, type, routePath, options) {
    var src = normalizeSource(source, routePath);
    var safeSrc = escapeHtml(src);
    var label = type === "audio" ? config.audioLabel : config.videoLabel;
    var title = (options && options.title) || getFileName(src);
    var safeTitle = escapeHtml(title);
    var media;
    var poster = options && options.poster ? ' poster="' + escapeHtml(options.poster) + '"' : "";

    if (!src) return "";

    if (type === "audio") {
      media =
        '<audio class="docs-media-audio" controls preload="' +
        escapeHtml(config.preload) +
        '" src="' +
        safeSrc +
        '">' +
        '<p class="docs-media-fallback">无法直接播放音频，<a href="' +
        safeSrc +
        '" target="_blank" rel="noopener">打开文件</a>。</p>' +
        "</audio>";
    } else {
      media =
        '<video class="docs-media-video" controls playsinline preload="' +
        escapeHtml(config.preload) +
        '"' +
        poster +
        ' src="' +
        safeSrc +
        '">' +
        '<p class="docs-media-fallback">无法直接播放视频，<a href="' +
        safeSrc +
        '" target="_blank" rel="noopener">打开文件</a>。</p>' +
        "</video>";
    }

    return [
      '<div class="docs-media-player docs-media-player-' + escapeHtml(type) + '">',
      '<div class="docs-media-toolbar">',
      '<span class="docs-media-type">' + escapeHtml(label) + "</span>",
      '<span class="docs-media-title" title="' + safeTitle + '">' + safeTitle + "</span>",
      '<a class="docs-media-action" href="' + safeSrc + '" target="_blank" rel="noopener">' + escapeHtml(config.openText) + "</a>",
      "</div>",
      '<div class="docs-media-body">',
      media,
      "</div>",
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
    var fallbackType =
      lang === "audio" || audioExtensions.indexOf(lang) > -1
        ? "audio"
        : lang === "video" || videoExtensions.indexOf(lang) > -1
          ? "video"
          : "";
    var options;
    var type;

    if (!lines.length) return null;
    if (!/^(audio|video|media|mp3|wav|ogg|m4a|aac|flac|opus|mp4|webm|ogv|mov|m4v)$/i.test(lang)) return null;
    if (/[<>]/.test(source)) return null;

    type = getMediaTypeFromSource(source, fallbackType);

    if (type !== "audio" && type !== "video") return null;
    options = parseOptions(lines, routePath);
    if (!options) return null;

    return renderPlayer(source, type, routePath, options);
  }

  function transformFencedMedia(content, routePath) {
    return String(content || "").replace(fencePattern, function (match, prefix, langLine, body) {
      var player = parseFence(langLine, body, routePath);

      return player ? prefix + player : match;
    });
  }

  function extractWrapperSource(content, expectedType) {
    var transformed = transformFencedMedia(content, "");

    if (transformed !== String(content || "")) return transformed;

    content = String(content || "").trim();

    if (!/\r?\n/.test(content) && getMediaTypeFromSource(content, expectedType) === expectedType) {
      return renderPlayer(content, expectedType, "");
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
      var type = getMediaTypeFromSource(path);

      requestText(
        getWrapperUrl(vm, path),
        function (content) {
          next(extractWrapperSource(content, type) || renderPlayer(path, type, path));
        },
        function () {
          next(renderPlayer(path, type, path));
        }
      );
    };
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
    registerRoutes(vm);

    hook.beforeEach(function (content, next) {
      next(transformFencedMedia(content, vm.route && vm.route.path));
    });
  });
})();
