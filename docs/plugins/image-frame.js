(function () {
  var DEFAULT_CONFIG = {
    imageClass: "docs-framed-image",
    selector: ".markdown-section",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.imageFrame) || {});
  var skipSelector = [
    ".emoji",
    ".github-corner img",
    ".sidebar img",
    ".app-nav img",
    ".docs-image-frame img",
    "pre img",
    "code img",
    "table img",
    "svg image",
  ].join(",");

  function isSkippableImage(img) {
    return !img || img.matches(skipSelector) || img.closest(".docs-image-frame");
  }

  function getWrapTarget(img) {
    var parent = img.parentElement;

    if (parent && parent.tagName === "A" && parent.children.length === 1) {
      return parent;
    }

    return img;
  }

  function wrapImage(img) {
    var target;
    var frame;

    if (isSkippableImage(img)) return;

    target = getWrapTarget(img);
    if (!target.parentNode || target.closest(".docs-image-frame")) return;

    frame = document.createElement("span");
    frame.className = "docs-image-frame";
    target.parentNode.insertBefore(frame, target);
    frame.appendChild(target);
    img.classList.add(config.imageClass);
  }

  function enhanceImages() {
    document.querySelectorAll(config.selector).forEach(function (root) {
      root.querySelectorAll("img").forEach(wrapImage);
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(enhanceImages);
    });
  });
})();
