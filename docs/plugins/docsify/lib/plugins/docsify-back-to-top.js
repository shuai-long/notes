(function () {
  const BUTTON_ID = "docsify-back-to-top";
  const STYLE_ID = "docsify-back-to-top-style";
  let initialized = false;

  function injectStyle() {
    if (document.getElementById(STYLE_ID)) return;

    const style = document.createElement("style");
    style.id = STYLE_ID;
    style.textContent = `
      #${BUTTON_ID} {
        position: fixed;
        right: 20px;
        bottom: 24px;
        z-index: 999;
        width: 42px;
        height: 42px;
        border: 1px solid rgba(66, 185, 131, 0.28);
        border-radius: 50%;
        background: var(--theme-color, #42b983);
        color: #fff;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.18);
        cursor: pointer;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        opacity: 0;
        pointer-events: none;
        transform: translateY(8px);
        transition: opacity 0.2s ease, transform 0.2s ease, box-shadow 0.2s ease;
      }

      #${BUTTON_ID}.visible {
        opacity: 0.94;
        pointer-events: auto;
        transform: translateY(0);
      }

      #${BUTTON_ID}:hover,
      #${BUTTON_ID}:focus-visible {
        opacity: 1;
        outline: none;
        transform: translateY(-2px);
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.24);
      }

      #${BUTTON_ID} i {
        font-size: 16px;
        line-height: 1;
      }

	      @media (prefers-reduced-motion: reduce) {
        #${BUTTON_ID} {
          transition: none;
        }
      }
    `;
    document.head.appendChild(style);
  }

  function getScrollTop() {
    const scrollingElement = document.scrollingElement || document.documentElement;
    return scrollingElement ? scrollingElement.scrollTop : window.pageYOffset || 0;
  }

  function updateVisibility(button) {
    const threshold = Math.max(320, window.innerHeight * 0.55);
    button.classList.toggle("visible", getScrollTop() > threshold);
  }

  function createButton() {
    let button = document.getElementById(BUTTON_ID);
    if (button) return button;

    button = document.createElement("button");
    button.id = BUTTON_ID;
    button.type = "button";
    button.setAttribute("aria-label", "返回顶部");
    button.title = "返回顶部";
    button.innerHTML = '<i class="fas fa-angle-up" aria-hidden="true"></i>';
    button.addEventListener("click", function () {
      try {
        window.scrollTo({ top: 0, behavior: "smooth" });
      } catch (e) {
        window.scrollTo(0, 0);
      }
    });
    document.body.appendChild(button);
    return button;
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.mounted(function () {
      if (initialized) return;
      initialized = true;

      injectStyle();
      const button = createButton();
      const sync = function () {
        updateVisibility(button);
      };

      window.addEventListener("scroll", sync, { passive: true });
      window.addEventListener("resize", sync);
      sync();
    });

    hook.doneEach(function () {
      const button = document.getElementById(BUTTON_ID);
      if (button) updateVisibility(button);
    });
  });
})();
