window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  const DEFAULT_MAX_MATCH_BRACES_CHARS = 12000;

  function getConfig() {
    const config = (vm && vm.config && vm.config.prism) || {};
    return {
      maxMatchBracesChars:
        Number(config.maxMatchBracesChars) || DEFAULT_MAX_MATCH_BRACES_CHARS,
    };
  }

  hook.doneEach(function () {
    const config = getConfig();
    const codeBlocks = document.querySelectorAll(".content pre > code");

    codeBlocks.forEach((codeEl) => {
      if (codeEl.dataset.prismClassProcessed === "true") return;

      const preEl = codeEl.parentNode;
      const codeSize = codeEl.textContent.length;

      codeEl.dataset.prismClassProcessed = "true";

      if (codeSize > config.maxMatchBracesChars) {
        return;
      }

      codeEl.classList.add("match-braces");
      preEl.classList.add("match-braces");

      // Prism has already highlighted the block once through docsify. Re-highlight
      // only small blocks so match-braces can attach without freezing long pages.
      if (window.Prism && typeof Prism.highlightElement === "function") {
        Prism.highlightElement(codeEl);
      }
    });
  });
});
