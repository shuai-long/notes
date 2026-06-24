(function () {
  var GITHUB_PAGES_HOST = "shuai-long.github.io";
  var GITHUB_PAGES_BASE = "/notes";

  function getHashPath() {
    return (window.location.hash || "#/")
      .replace(/^#/, "")
      .split("?")[0]
      .replace(/\/+$/, "") || "/";
  }

  function isGithubPagesHome() {
    var path = window.location.pathname.replace(/\/+$/, "") || "/";

    return window.location.hostname === GITHUB_PAGES_HOST && path === GITHUB_PAGES_BASE && isDocsHome();
  }

  function isDocsHome() {
    return getHashPath() === "/";
  }

  function syncPageState() {
    document.body.classList.toggle("is-docs-home", isDocsHome());
    document.body.classList.toggle("is-github-pages-home", isGithubPagesHome());
  }

  window.addEventListener("hashchange", syncPageState);
  window.addEventListener("popstate", syncPageState);

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", syncPageState);
  } else {
    syncPageState();
  }
})();
