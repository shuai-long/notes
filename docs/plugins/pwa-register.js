(function () {
  if (!("serviceWorker" in navigator)) return;
  if (!/^https?:$/.test(window.location.protocol)) return;

  window.addEventListener("load", function () {
    navigator.serviceWorker
      .register("./sw.js")
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
  });

  navigator.serviceWorker.addEventListener("controllerchange", function () {
    if (window.__docsifyPwaReloading) return;

    window.__docsifyPwaReloading = true;
    window.location.reload();
  });
})();
