(function () {
  var DEFAULT_CONFIG = {
    defaultColumns: 2,
    defaultGap: "12px",
    defaultMinWidth: "260px",
    maxColumns: 6,
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.imageGrid) || {});
  var blockPattern = /(^|\n)<!--\s*image-grid:start([^>]*)-->\s*([\s\S]*?)\s*<!--\s*image-grid:end\s*-->/gi;

  function escapeHtml(text) {
    return String(text || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
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
    ).replace(/^\/+/, "");
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

  function parseAttributes(text) {
    var attrs = {};

    String(text || "").replace(/([a-z][a-z0-9-]*)=(?:"([^"]*)"|'([^']*)'|([^\s]+))/gi, function (_, key, doubleValue, singleValue, plainValue) {
      attrs[key.toLowerCase()] = doubleValue || singleValue || plainValue || "";
      return "";
    });

    return attrs;
  }

  function toColumns(value) {
    var columns = parseInt(value || config.defaultColumns, 10);
    var maxColumns = parseInt(config.maxColumns || DEFAULT_CONFIG.maxColumns, 10);

    if (!columns || columns < 1) columns = DEFAULT_CONFIG.defaultColumns;
    if (!maxColumns || maxColumns < 1) maxColumns = DEFAULT_CONFIG.maxColumns;

    return Math.min(columns, maxColumns);
  }

  function getItemWidth(columns) {
    var gaps = [];
    var index;

    if (columns <= 1) return "100%";

    for (index = 1; index < columns; index += 1) {
      gaps.push("var(--docs-image-grid-gap)");
    }

    return "calc((100% - " + gaps.join(" - ") + ") / " + columns + ")";
  }

  function toCssSize(value, fallback) {
    var size = String(value || fallback || "").trim();

    if (!size) return "";
    if (/^-?\d+(?:\.\d+)?$/.test(size)) return size + "px";

    return size;
  }

  function parseTarget(text) {
    var value = String(text || "").trim();
    var title = "";
    var match;
    var src;

    if (value.charAt(0) === "<") {
      match = value.match(/^<([^>]*)>\s*(.*)$/);
      src = match ? match[1] : value;
      value = match ? match[2] : "";
    } else {
      match = value.match(/^(\S+)\s*(.*)$/);
      src = match ? match[1] : value;
      value = match ? match[2] : "";
    }

    match = value.trim().match(/^["']([^"']*)["']$/);
    if (match) title = match[1];

    return {
      src: src,
      title: title,
    };
  }

  function parseImageLine(line) {
    var linked = line.match(/^\[!\[([^\]]*)\]\(([\s\S]+)\)\]\(([\s\S]+)\)$/);
    var image = linked ? null : line.match(/^!\[([^\]]*)\]\(([\s\S]+)\)$/);
    var target;

    if (linked) {
      target = parseTarget(linked[2]);

      return {
        alt: linked[1],
        href: parseTarget(linked[3]).src,
        src: target.src,
        title: target.title,
      };
    }

    if (!image) return null;

    target = parseTarget(image[2]);

    return {
      alt: image[1],
      href: "",
      src: target.src,
      title: target.title,
    };
  }

  function getGridMarker(title) {
    var marker = String(title || "").trim();
    var match = marker.match(/^(?:image-grid|grid)\s*:?\s*([\s\S]*)$/i);

    return match ? match[1] : null;
  }

  function parseImages(body) {
    return String(body || "")
      .split(/\r?\n/)
      .map(function (line) {
        return line.trim();
      })
      .filter(function (line) {
        return line && !/^<!--[\s\S]*-->$/.test(line);
      })
      .map(parseImageLine)
      .filter(Boolean);
  }

  function renderImage(image, routePath) {
    var src = normalizeSource(image.src, routePath);
    var href = normalizeSource(image.href, routePath);
    var title = image.title ? ' title="' + escapeHtml(image.title) + '"' : "";
    var img =
      '<img src="' +
      escapeHtml(src) +
      '" alt="' +
      escapeHtml(image.alt) +
      '"' +
      title +
      ' loading="lazy">';

    if (href) {
      img =
        '<a href="' +
        escapeHtml(href) +
        '" target="_blank" rel="noopener">' +
        img +
        "</a>";
    }

    return '<span class="docs-image-grid-item docs-image-frame">' + img + "</span>";
  }

  function renderGridFromImages(attrText, images, routePath, original) {
    var attrs = parseAttributes(attrText);
    var columns = toColumns(attrs.cols || attrs.columns || attrs.col);
    var gap = toCssSize(attrs.gap, config.defaultGap);
    var minWidth = toCssSize(attrs.min || attrs.minwidth || attrs["min-width"], config.defaultMinWidth);
    var maxHeight = toCssSize(attrs.height || attrs["max-height"], "");
    var style;

    if (!images.length) return original;

    style = "--docs-image-grid-cols:" + columns + ";";
    if (gap) style += "--docs-image-grid-gap:" + gap + ";";
    if (minWidth) style += "--docs-image-grid-min-width:" + minWidth + ";";
    style += "--docs-image-grid-item-width:" + getItemWidth(columns) + ";";
    if (maxHeight) style += "--docs-image-grid-max-height:" + maxHeight + ";";

    return (
      '<div class="docs-image-grid" style="' +
      escapeHtml(style) +
      '">' +
      images
        .map(function (image) {
          return renderImage(image, routePath);
        })
        .join("") +
      "</div>"
    );
  }

  function renderGrid(attrText, body, routePath, original) {
    return renderGridFromImages(attrText, parseImages(body), routePath, original);
  }

  function transformMarkedImageGroups(content, routePath) {
    var lines = String(content || "").split(/\r?\n/);
    var result = [];
    var index = 0;
    var image;
    var marker;
    var images;
    var nextImage;

    while (index < lines.length) {
      image = parseImageLine(lines[index].trim());
      marker = image ? getGridMarker(image.title) : null;

      if (marker !== null) {
        image.title = "";
        images = [image];
        index += 1;

        while (index < lines.length) {
          nextImage = parseImageLine(lines[index].trim());
          if (!nextImage || getGridMarker(nextImage.title) !== null) break;

          images.push(nextImage);
          index += 1;
        }

        result.push(renderGridFromImages(marker, images, routePath, lines[index - 1]));
      } else {
        result.push(lines[index]);
        index += 1;
      }
    }

    return result.join("\n");
  }

  function transform(content, routePath) {
    return String(content || "").replace(blockPattern, function (match, prefix, attrs, body) {
      return prefix + renderGrid(attrs, body, routePath, match.trim());
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
    hook.beforeEach(function (content, next) {
      content = transform(content, vm.route && vm.route.path);
      next(transformMarkedImageGroups(content, vm.route && vm.route.path));
    });
  });
})();
