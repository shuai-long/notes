(function () {
  if (!("serviceWorker" in navigator)) return;
  if (!/^https?:$/.test(window.location.protocol)) return;

  function registerServiceWorker() {
    navigator.serviceWorker
      .register("./sw.js", { updateViaCache: "none" })
      .then(function (registration) {
        if (registration.waiting) {
          registration.waiting.postMessage({ type: "SKIP_WAITING" });
        }

        registration.addEventListener("updatefound", function () {
          var worker = registration.installing;

          if (!worker) return;

          worker.addEventListener("statechange", function () {
            if (worker.state === "installed" && navigator.serviceWorker.controller) {
              worker.postMessage({ type: "SKIP_WAITING" });
            }
          });
        });
      })
      .catch(function (error) {
        console.warn("[PWA] service worker registration failed:", error);
      });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", registerServiceWorker, { once: true });
  } else {
    registerServiceWorker();
  }
})();
