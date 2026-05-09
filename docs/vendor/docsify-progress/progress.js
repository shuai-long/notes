(function () {
  var DEFAULT_CONFIG = {
    color: "var(--theme-color,#42b983)",
    height: "2px",
    position: "top",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.progress) || {});
  var ticking = false;
  var progress;
  var wrapper;

  function createProgress() {
    if (progress) return;

    wrapper = document.createElement("div");
    wrapper.className = "docsify-progress";
    wrapper.style.cssText = [
      "height:" + config.height,
      "left:0",
      "position:fixed",
      config.position === "bottom" ? "bottom:0" : "top:0",
      "right:0",
      "width:100%",
      "z-index:999",
    ].join(";");

    progress = document.createElement("div");
    progress.id = "progress-display";
    progress.style.cssText = [
      "background:" + config.color,
      "border-radius:999px",
      "height:100%",
      "transition:width .18s ease",
      "width:0",
    ].join(";");

    wrapper.appendChild(progress);
    document.body.appendChild(wrapper);
  }

  function getScrollPercent() {
    var doc = document.documentElement;
    var body = document.body;
    var scrollTop = window.pageYOffset || doc.scrollTop || body.scrollTop || 0;
    var scrollHeight = Math.max(doc.scrollHeight, body.scrollHeight);
    var viewportHeight = window.innerHeight || doc.clientHeight || 1;
    var maxScroll = scrollHeight - viewportHeight;

    if (maxScroll <= 0) return 0;

    return Math.min(100, Math.max(0, (scrollTop / maxScroll) * 100));
  }

  function updateProgress() {
    ticking = false;
    if (!progress) return;

    progress.style.width = getScrollPercent().toFixed(2) + "%";
  }

  function requestUpdate() {
    if (ticking) return;

    ticking = true;
    window.requestAnimationFrame(updateProgress);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.progress = config;
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.mounted(function () {
      createProgress();
      window.addEventListener("scroll", requestUpdate, { passive: true });
      window.addEventListener("resize", requestUpdate);
      requestUpdate();
    });

    hook.doneEach(function () {
      requestUpdate();
    });
  });
})();
