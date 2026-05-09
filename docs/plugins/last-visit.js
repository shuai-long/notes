(function () {
  var DEFAULT_CONFIG = {
    storageKey: "docsify-last-visit",
    restoreAtRoot: true,
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

  function storeCurrentHash() {
    var hash = normalizeHash(window.location.hash);

    if (!isStorableHash(hash)) return;

    writeLastHash(hash);
  }

  function scheduleStore() {
    window.clearTimeout(storeTimer);
    storeTimer = window.setTimeout(storeCurrentHash, 60);
  }

  function restoreLastVisit() {
    var storedHash = normalizeHash(readLastHash());
    var nextUrl;

    if (config.restoreAtRoot === false) return;
    if (!isRootHash(window.location.hash)) return;
    if (!isStorableHash(storedHash)) return;

    nextUrl = window.location.pathname + window.location.search + storedHash;
    window.location.replace(nextUrl);
  }

  restoreLastVisit();

  window.addEventListener("hashchange", scheduleStore);
  window.addEventListener("beforeunload", storeCurrentHash);

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(storeCurrentHash);
  });
})();
