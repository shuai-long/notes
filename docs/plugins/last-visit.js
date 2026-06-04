(function () {
  var DEFAULT_CONFIG = {
    manifestPath: "./pwa-cache-manifest.json",
    restoreSessionKey: "docsify-last-visit-restored",
    storageKey: "docsify-last-visit",
    restoreAtRoot: true,
    validateRestore: true,
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.lastVisit) || {});
  var storeTimer = 0;

  function normalizeHash(hash) {
    if (!hash || hash === "#") return "#/";

    return hash.charAt(0) === "#" ? hash : "#" + hash;
  }

  function getHashPath(hash) {
    var normalized = normalizeHash(hash);
    var path = normalized.replace(/^#/, "").split("?")[0].replace(/\/+$/, "");

    return path || "/";
  }

  function isRootHash(hash) {
    return getHashPath(hash) === "/";
  }

  function isStorableHash(hash) {
    var normalized = normalizeHash(hash);

    return normalized.indexOf("#/") === 0 && !isRootHash(normalized);
  }

  function safeDecode(text) {
    try {
      return decodeURIComponent(text);
    } catch (error) {
      return text;
    }
  }

  function hashToManifestPath(hash) {
    var path = normalizeHash(hash)
      .replace(/^#\/?/, "")
      .split("?")[0]
      .replace(/\/+$/, "");

    path = safeDecode(path);
    if (!path) return "./README.md";
    if (/\.(?:md|html?|pdf)$/i.test(path)) return "./" + path;

    return "./" + path + ".md";
  }

  function isNotFoundPage() {
    return Boolean(document.querySelector("[data-docsify-not-found]"));
  }

  function readLastHash() {
    try {
      return window.localStorage.getItem(config.storageKey) || "";
    } catch (error) {
      return "";
    }
  }

  function writeLastHash(hash) {
    try {
      window.localStorage.setItem(config.storageKey, hash);
    } catch (error) {
      return false;
    }

    return true;
  }

  function removeLastHash() {
    try {
      window.localStorage.removeItem(config.storageKey);
    } catch (error) {
      return false;
    }

    return true;
  }

  function hasRestoredInSession() {
    try {
      return window.sessionStorage.getItem(config.restoreSessionKey) === "1";
    } catch (error) {
      return false;
    }
  }

  function markRestoredInSession() {
    try {
      window.sessionStorage.setItem(config.restoreSessionKey, "1");
    } catch (error) {
      return false;
    }

    return true;
  }

  function validateStoredHash(hash) {
    var targetPath;

    if (config.validateRestore === false) return Promise.resolve(true);

    targetPath = hashToManifestPath(hash);

    return fetch(config.manifestPath, { cache: "no-store" })
      .then(function (response) {
        if (!response.ok) throw new Error("manifest request failed");

        return response.json();
      })
      .then(function (manifest) {
        var files = (manifest && manifest.files) || [];

        return files.indexOf(targetPath) >= 0;
      })
      .catch(function () {
        return false;
      });
  }

  function storeCurrentHash() {
    var hash = normalizeHash(window.location.hash);

    if (isNotFoundPage()) {
      if (normalizeHash(readLastHash()) === hash) {
        removeLastHash();
      }

      return;
    }

    if (!isStorableHash(hash)) return;

    writeLastHash(hash);
  }

  function scheduleStore() {
    window.clearTimeout(storeTimer);
    storeTimer = window.setTimeout(storeCurrentHash, 60);
  }

  function restoreLastVisit() {
    var storedHash = normalizeHash(readLastHash());

    if (config.restoreAtRoot === false) return;
    if (hasRestoredInSession()) return;
    if (!isRootHash(window.location.hash)) return;
    if (!isStorableHash(storedHash)) return;

    validateStoredHash(storedHash).then(function (isValid) {
      var nextUrl;

      if (!isValid) {
        removeLastHash();
        return;
      }

      if (!isRootHash(window.location.hash)) return;

      markRestoredInSession();
      nextUrl = window.location.pathname + window.location.search + storedHash;
      window.location.replace(nextUrl);
    });
  }

  restoreLastVisit();

  window.addEventListener("hashchange", scheduleStore);
  window.addEventListener("beforeunload", storeCurrentHash);

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(storeCurrentHash);
  });
})();
