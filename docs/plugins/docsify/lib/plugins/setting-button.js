const LS_THEME_MANAGER_DOCSIFY_THEME = 'docsifyTheme';
const LS_PRISM_THEME_MANAGER_PRISM_THEME = "prism-theme";
const LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS = "docsify-css-vars";
const LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS = 'docsify-custom-css';

  (function () {
    // 页面加载时立即执行，读取本地存储或系统偏好确定主题
    var theme = localStorage.getItem(LS_THEME_MANAGER_DOCSIFY_THEME);
    if (!theme && window.matchMedia) {
      theme = window.matchMedia("(prefers-color-scheme: dark)").matches
        ? "dark"
        : "light";
    }
    if (theme) {
      var lightLink = document.querySelector('link[title="light"]');
      var darkLink = document.querySelector('link[title="dark"]');
      // 启用/禁用对应的样式表
      if (lightLink && darkLink) {
        if (theme === "dark") {
          lightLink.disabled = true;
          darkLink.disabled = false;
        } else {
          lightLink.disabled = false;
          darkLink.disabled = true;
        }
      }
      // 存储当前主题，保证下次加载时也能直接应用
      localStorage.setItem(LS_THEME_MANAGER_DOCSIFY_THEME, theme);
    }
  })();

window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (
  hook,
  vm
) {
  hook.mounted(function () {

    // 主题对象
    const themeManager = (() => {
      // 读取本地存储的主题设置（'light' 或 'dark'），如无则使用系统首选项
      let currentTheme;
      try {
        currentTheme = localStorage.getItem(LS_THEME_MANAGER_DOCSIFY_THEME);
      } catch (e) {
        currentTheme = null;
      }
      if (!currentTheme) {
        // 如果 localStorage 没有，则根据 prefers-color-scheme 判断
        currentTheme = window.matchMedia("(prefers-color-scheme: dark)").matches
          ? "dark"
          : "light";
      }

      // 将 <html> 的 data-theme 属性设置为当前主题，便于 CSS 根据其应用样式:contentReference[oaicite:4]{index=4}
      document.documentElement.setAttribute("data-theme", currentTheme);

      // 获取主题样式表的链接元素（假设在 <head> 中已有 light 和 dark 两个 <link>）
      const lightLink =
        document.getElementById("docsify-theme-light") ||
        document.querySelector('link[data-theme="light"][rel="stylesheet"]');
      const darkLink =
        document.getElementById("docsify-theme-dark") ||
        document.querySelector('link[data-theme="dark"][rel="stylesheet"]');

      // 根据当前主题立即启用对应样式表，禁用另一种，以避免闪烁:contentReference[oaicite:5]{index=5}
      if (lightLink) lightLink.disabled = currentTheme !== "light";
      if (darkLink) darkLink.disabled = currentTheme !== "dark";

      // 保存所有通过 registerThemeButton 注册的按钮引用，用于更新图标和文本
      const themeButtons = new Set();

      // 切换主题：启用/禁用样式表，更新属性、存储和事件
      function toggle() {
        // 切换主题名称
        currentTheme = currentTheme === "dark" ? "light" : "dark";

        // 删除 themeable-setting 设置的样式
        const element = document.getElementById("themeable-setting");
        if (element) {
          element.remove(); // 或使用传统方式
        }

        // 启用新主题的样式表，禁用旧主题的样式表
        if (lightLink) lightLink.disabled = currentTheme !== "light";
        if (darkLink) darkLink.disabled = currentTheme !== "dark";

        // 更新 <html> 的 data-theme 属性
        document.documentElement.setAttribute("data-theme", currentTheme);
        // 存储新的主题值到 localStorage
        try {
          localStorage.setItem(LS_THEME_MANAGER_DOCSIFY_THEME, currentTheme);
        } catch (e) {
          // 忽略不可用 localStorage 的错误
        }

        // 更新所有已注册按钮的图标和文本
        updateThemeElements();

        // 派发主题变化事件，detail 可携带主题名
        const event = new CustomEvent("theme-change", { detail: currentTheme });
        document.dispatchEvent(event);
      }

      // 注册一个主题切换按钮：绑定点击事件，并添加到按钮列表中
      // 新增注册按钮方法
      function registerThemeButton(buttonElement) {
        themeButtons.add(buttonElement);
        // 初始化按钮状态
        updateThemeElements();
      }

      // 更新所有已注册按钮的显示（图标和文本）以匹配当前主题
      // 更新所有主题相关元素
      function updateThemeElements() {
        const iconClass = currentTheme === "dark" ? "fa-sun" : "fa-moon";
        const labelText = currentTheme === "dark" ? "亮色主题" : "暗色主题";

        themeButtons.forEach((button) => {
          // 更新图标
          const icon = button.querySelector("i");
          if (icon) {
            icon.className = `fas ${iconClass}`;
          }

          // 更新标签
          const label = button.querySelector(".button-label");
          if (label) {
            label.textContent = labelText;
          }
        });
      }

      // 对外公开的方法和属性
      return {
        toggle,
        registerThemeButton,
        updateThemeElements,
        // isDark 为只读属性：当前主题是否为暗色
        get isDark() {
          return currentTheme === "dark";
        },
      };
    })();

    // 主题自定义属性修改
    const themeAbleManager = (function () {

      // css 样式切换
      const cssChange = (function () {

        // 创建样式隔离的模态框
        function createModal() {
          const modal = document.createElement('div');
          const shadow = modal.attachShadow({ mode: 'open' });

          // Shadow DOM内部样式
          const style = document.createElement('style');
          style.textContent = `
            :host {
              display: none;
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background: rgba(0,0,0,0.5);
              z-index: 10000;
            }
      
            :host(.active) {
              display: flex;
              justify-content: center;
              align-items: center;
            }
            .container {
              position: relative;  /* 新增定位上下文 */
              background: white;
              padding: 30px 20px 20px; /* 调整顶部内边距 */
              border-radius: 8px;
              width: 60%;
              max-width: 600px;
            }
            .close-btn {
              position: absolute;
              top: 10px;
              right: 10px;
              background: none;
              border: none;
              font-size: 1.2em;
              cursor: pointer;
              color: #666;
            }
            .close-btn:hover {
              color: #333;
            }
            textarea {
              width: 100%;
              height: 300px;
              margin: 10px 0;
              font-family: monospace;
            }
            .button-group {
              text-align: right;
            }
            button {
              padding: 6px 12px;
              margin-left: 8px;
            }
          `;

          // 模态框内容
          const container = document.createElement('div');
          container.className = 'container';
          container.innerHTML = `
            <button class="close-btn">×</button>
            <h3>自定义CSS</h3>
            <textarea placeholder="输入CSS样式..."></textarea>
            <div class="button-group">
              <button class="reset">重置</button>
              <button class="save">保存</button>
            </div>
          `;

          shadow.appendChild(style);
          shadow.appendChild(container);
          document.body.appendChild(modal);
          return modal;
        }

        // 应用保存的样式
        function applySavedStyles() {
          const savedCss = localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS);
          if (savedCss) {
            // 移除旧样式
            const oldStyle = document.getElementById(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS);
            if (oldStyle) oldStyle.remove();

            // 添加新样式
            const style = document.createElement('style');
            style.id = LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS;
            style.textContent = savedCss;
            document.head.appendChild(style);
          }
        }

        function show() {
          modal.classList.add('active')
        }

        function init() {
          addEvents();
          applySavedStyles();
        }

        // 初始化操作
        let modal, textarea;
        function addEvents() {
          // 创建界面元素
          modal = createModal();
          textarea = modal.shadowRoot.querySelector('textarea');

          // 加载已保存的CSS
          textarea.value = localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS) || '';

          // 更新后的Tab键处理逻辑
          textarea.addEventListener('keydown', function (e) {
            if (e.key === 'Tab' || e.keyCode === 9) {
              e.preventDefault();
              const start = this.selectionStart;
              const end = this.selectionEnd;
              const value = this.value;

              if (e.shiftKey) {
                // Shift+Tab 删除缩进
                const linesBefore = value.substring(0, start).split('\n');
                const linesAfter = value.substring(end).split('\n');
                const selection = value.substring(start, end);

                // 删除每行开头的两个空格
                const modified = selection.replace(/^ /gm, '');
                this.value = [
                  ...linesBefore.slice(0, -1),
                  linesBefore[linesBefore.length - 1] + modified + linesAfter[0],
                  ...linesAfter.slice(1)
                ].join('\n');

                // 调整光标位置
                const diff = selection.length - modified.length;
                this.selectionStart = Math.max(start - diff, 0);
                this.selectionEnd = end - diff;
              } else {
                // 普通Tab 插入两个空格
                const linesBefore = value.substring(0, start).split('\n');
                const linesAfter = value.substring(end).split('\n');
                const selection = value.substring(start, end);

                // 添加两个空格缩进
                const modified = selection.replace(/^/gm, '  '); // 改为两个空格
                this.value = [
                  ...linesBefore.slice(0, -1),
                  linesBefore[linesBefore.length - 1] + modified + linesAfter[0],
                  ...linesAfter.slice(1)
                ].join('\n');

                // 调整光标位置
                const addedSpace = modified.split('\n').length * 2;
                this.selectionStart = start + (start === end ? 2 : 0);
                this.selectionEnd = end + modified.length - selection.length;
              }
            }
          });

          // 关闭按钮事件
          modal.shadowRoot.querySelector('.close-btn').addEventListener('click', () => {
            modal.classList.remove('active');
          });

          // 新增重置按钮事件
          modal.shadowRoot.querySelector('.reset').addEventListener('click', () => {
            if (confirm('确定要重置所有自定义样式吗？')) {
              // 清除本地存储
              localStorage.removeItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS);
              // 移除页面样式
              const oldStyle = document.getElementById(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS);
              if (oldStyle) oldStyle.remove();
              // 清空编辑器内容
              textarea.value = '';
              // 关闭模态框
              modal.classList.remove('active');
            }
          });

          modal.shadowRoot.querySelector('.save').addEventListener('click', () => {
            try {
              // 验证CSS有效性
              const css = textarea.value;
              const style = document.createElement('style');
              style.textContent = css;
              document.head.appendChild(style);
              style.remove();

              // 保存到localStorage
              localStorage.setItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CUSTOM_CSS, css);
              modal.classList.remove('active');
              applySavedStyles();
            } catch (e) {
              alert('CSS语法错误,请检查后重试');
            }
          });
        };

        return {
          init,
          show,
        }
      })();

      // 配置参数（只需维护变量名列表）
      const config = {
        customVars: [
          "--mono-hue",
          "--mono-saturation",
          "--mono-shade3",
          "--mono-shade2",
          "--mono-shade1",
          "--mono-base",
          "--mono-tint1",
          "--mono-tint2",
          "--mono-tint3",
          "--theme-hue",
          "--theme-saturation",
          "--theme-lightness",
          "--theme-color",
          "--modular-scale",
          "--modular-scale--2",
          "--modular-scale--1",
          "--modular-scale-1",
          "--modular-scale-2",
          "--modular-scale-3",
          "--modular-scale-4",
          "--modular-scale-5",
          "--font-size-xxxl",
          "--font-size-xxl",
          "--font-size-xl",
          "--font-size-l",
          "--font-size-m",
          "--font-size-s",
          "--font-size-xs",
          "--border-radius-s",
          "--border-radius-m",
          "--border-radius-l",
          "--duration-slow",
          "--duration-medium",
          "--duration-fast",
          "--spinner-size",
          "--spinner-track-width",
          "--spinner-track-color",
          "--spinner-transition-duration",
          "--base-background-color",
          "--base-color",
          '--base-font-family',
          "--base-font-size",
          "--base-font-weight",
          "--base-letter-spacing",
          "--base-line-height",
          "--emoji-size",
          "--hr-border",
          "--mark-background",
          "--mark-color",
          '--pre-font-family',
          "--pre-font-size",
          "--pre-font-weight",
          "--pre-line-height",
          "--selection-color",
          "--small-font-size",
          "--strong-color",
          "--strong-font-weight",
          "--subsup-font-size",
          "--content-max-width",
          "--blockquote-background",
          "--blockquote-border-color",
          "--blockquote-border-style",
          "--blockquote-border-width",
          "--blockquote-border-radius",
          "--blockquote-color",
          '--blockquote-em-font-family',
          "--blockquote-em-font-size",
          "--blockquote-em-font-style",
          "--blockquote-em-font-weight",
          '--blockquote-font-family',
          "--blockquote-font-size",
          "--blockquote-font-style",
          "--blockquote-font-weight",
          "--blockquote-quotes-close",
          "--blockquote-quotes-color",
          '--blockquote-quotes-font-family',
          "--blockquote-quotes-font-size",
          "--blockquote-quotes-open",
          "--blockquote-padding",
          '--code-font-family',
          "--code-font-size",
          "--code-font-weight",
          "--code-tab-size",
          "--code-block-border-radius",
          "--code-block-line-height",
          "--code-block-margin",
          "--code-block-padding",
          "--code-inline-background",
          "--code-inline-border-radius",
          "--code-inline-color",
          "--code-inline-margin",
          "--code-inline-padding",
          "--code-theme-background",
          "--code-theme-comment",
          "--code-theme-function",
          "--code-theme-keyword",
          "--code-theme-operator",
          "--code-theme-punctuation",
          "--code-theme-selection",
          "--code-theme-selector",
          "--code-theme-tag",
          "--code-theme-text",
          "--code-theme-variable",
          "--heading-color",
          '--heading-font-family',
          "--heading-font-weight",
          "--heading-margin",
          "--heading-padding",
          "--heading-h1-border-color",
          "--heading-h1-border-style",
          "--heading-h1-border-width",
          "--heading-h1-color",
          '--heading-h1-font-family',
          "--heading-h1-font-size",
          "--heading-h1-font-weight",
          "--heading-h1-margin",
          "--heading-h1-padding",
          "--heading-h2-border-color",
          "--heading-h2-border-style",
          "--heading-h2-border-width",
          "--heading-h2-color",
          '--heading-h2-font-family',
          "--heading-h2-font-size",
          "--heading-h2-font-weight",
          "--heading-h2-margin",
          "--heading-h2-padding",
          "--heading-h3-border-color",
          "--heading-h3-border-style",
          "--heading-h3-border-width",
          "--heading-h3-color",
          '--heading-h3-font-family',
          "--heading-h3-font-size",
          "--heading-h3-font-weight",
          "--heading-h3-margin",
          "--heading-h3-padding",
          "--heading-h4-border-color",
          "--heading-h4-border-style",
          "--heading-h4-border-width",
          "--heading-h4-color",
          '--heading-h4-font-family',
          "--heading-h4-font-size",
          "--heading-h4-font-weight",
          "--heading-h4-margin",
          "--heading-h4-padding",
          "--heading-h5-border-color",
          "--heading-h5-border-style",
          "--heading-h5-border-width",
          "--heading-h5-color",
          '--heading-h5-font-family',
          "--heading-h5-font-size",
          "--heading-h5-font-weight",
          "--heading-h5-margin",
          "--heading-h5-padding",
          "--heading-h6-border-color",
          "--heading-h6-border-style",
          "--heading-h6-border-width",
          "--heading-h6-color",
          '--heading-h6-font-family',
          "--heading-h6-font-size",
          "--heading-h6-font-weight",
          "--heading-h6-margin",
          "--heading-h6-padding",
          "--kbd-background",
          "--kbd-border",
          "--kbd-border-radius",
          "--kbd-color",
          "--kbd-font-size",
          "--kbd-margin",
          "--kbd-min-width",
          "--kbd-padding",
          "--link-border-bottom",
          "--link-border-bottom--hover",
          "--link-color",
          "--link-color--hover",
          "--link-text-decoration",
          "--link-text-decoration--hover",
          "--link-text-decoration-color",
          "--link-text-decoration-color--hover",
          "--notice-background",
          "--notice-border-color",
          "--notice-border-radius",
          "--notice-border-style",
          "--notice-border-width",
          "--notice-color",
          '--notice-font-family',
          "--notice-font-weight",
          "--notice-padding",
          "--notice-before-background",
          "--notice-before-border-radius",
          "--notice-before-color",
          "--notice-before-content",
          '--notice-before-font-family',
          "--notice-before-font-size",
          "--notice-before-font-weight",
          "--notice-before-height",
          "--notice-before-left",
          "--notice-before-line-height",
          "--notice-before-margin",
          "--notice-before-padding",
          "--notice-before-position",
          "--notice-before-top",
          "--notice-before-width",
          "--notice-important-background",
          "--notice-important-border-color",
          "--notice-important-border-style",
          "--notice-important-border-width",
          "--notice-important-color",
          "--notice-important-before-background",
          "--notice-important-before-color",
          "--notice-important-before-content",
          "--notice-tip-background",
          "--notice-tip-border-color",
          "--notice-tip-border-style",
          "--notice-tip-border-width",
          "--notice-tip-color",
          "--notice-tip-before-background",
          "--notice-tip-before-color",
          "--notice-tip-before-content",
          "--table-body-border-color",
          "--table-body-border-width",
          "--table-cell-border-color",
          "--table-cell-border-width",
          "--table-cell-padding",
          "--table-head-background",
          "--table-head-border-color",
          "--table-head-border-width",
          "--table-head-font-weight",
          "--table-row-even-background",
          "--table-row-odd-background",
          "--cover-color",
          "--cover-margin",
          "--cover-max-width",
          "--cover-text-align",
          "--cover-background-blend-mode",
          "--cover-background-color",
          "--cover-background-image",
          "--cover-background-mask-color",
          "--cover-background-mask-opacity",
          "--cover-background-mask-visibility",
          "--cover-background-position",
          "--cover-background-repeat",
          "--cover-background-size",
          "--cover-blockquote-color",
          "--cover-blockquote-font-size",
          "--cover-border-inset",
          "--cover-border-color",
          "--cover-border-width",
          "--cover-button-background",
          "--cover-button-background--hover",
          "--cover-button-border",
          "--cover-button-border--hover",
          "--cover-button-border-radius",
          "--cover-button-box-shadow",
          "--cover-button-box-shadow--hover",
          "--cover-button-color",
          "--cover-button-color--hover",
          "--cover-button-padding",
          "--cover-button-text-decoration",
          "--cover-button-text-decoration--hover",
          "--cover-button-text-decoration-color",
          "--cover-button-text-decoration-color--hover",
          "--cover-button-transition",
          "--cover-button-primary-background",
          "--cover-button-primary-background--hover",
          "--cover-button-primary-border",
          "--cover-button-primary-border--hover",
          "--cover-button-primary-box-shadow",
          "--cover-button-primary-box-shadow--hover",
          "--cover-button-primary-color",
          "--cover-button-primary-color--hover",
          "--cover-button-primary-text-decoration",
          "--cover-button-primary-text-decoration--hover",
          "--cover-button-primary-text-decoration-color",
          "--cover-button-primary-text-decoration-color--hover",
          "--cover-heading-color",
          "--cover-heading-font-size",
          "--cover-heading-font-size-min",
          "--cover-heading-font-size-max",
          "--cover-heading-font-weight",
          "--cover-link-border-bottom",
          "--cover-link-border-bottom--hover",
          "--cover-link-color",
          "--cover-link-color--hover",
          "--cover-link-text-decoration",
          "--cover-link-text-decoration--hover",
          "--cover-link-text-decoration-color",
          "--cover-link-text-decoration-color--hover",
          "--navbar-root-background",
          "--navbar-root-background--active",
          "--navbar-root-background--hover",
          "--navbar-root-border-color",
          "--navbar-root-border-color--active",
          "--navbar-root-border-color--hover",
          "--navbar-root-border-radius",
          "--navbar-root-border-style",
          "--navbar-root-border-style--active",
          "--navbar-root-border-style--hover",
          "--navbar-root-border-width",
          "--navbar-root-color",
          "--navbar-root-color--active",
          "--navbar-root-color--hover",
          "--navbar-root-margin",
          "--navbar-root-padding",
          "--navbar-root-transition",
          "--navbar-root-text-decoration",
          "--navbar-root-text-decoration--active",
          "--navbar-root-text-decoration--hover",
          "--navbar-root-text-decoration-color",
          "--navbar-root-text-decoration-color--active",
          "--navbar-root-text-decoration-color--hover",
          "--navbar-menu-background",
          "--navbar-menu-border-color",
          "--navbar-menu-border-radius",
          "--navbar-menu-border-width",
          "--navbar-menu-box-shadow",
          "--navbar-menu-padding",
          "--navbar-menu-transition",
          "--navbar-menu-root-background",
          "--navbar-menu-root-background--active",
          "--navbar-menu-root-background--hover",
          "--navbar-menu-root-padding",
          "--navbar-menu-link-background",
          "--navbar-menu-link-background--active",
          "--navbar-menu-link-background--hover",
          "--navbar-menu-link-border-color",
          "--navbar-menu-link-border-color--active",
          "--navbar-menu-link-border-color--hover",
          "--navbar-menu-link-border-radius",
          "--navbar-menu-link-border-style",
          "--navbar-menu-link-border-style--active",
          "--navbar-menu-link-border-style--hover",
          "--navbar-menu-link-border-width",
          "--navbar-menu-link-color",
          "--navbar-menu-link-color--active",
          "--navbar-menu-link-color--hover",
          "--navbar-menu-link-margin",
          "--navbar-menu-link-padding",
          "--navbar-menu-link-text-decoration",
          "--navbar-menu-link-text-decoration--active",
          "--navbar-menu-link-text-decoration--hover",
          "--navbar-menu-link-text-decoration-color",
          "--navbar-menu-link-text-decoration-color--active",
          "--navbar-menu-link-text-decoration-color--hover",
          "--sidebar-background",
          "--sidebar-border-color",
          "--sidebar-border-width",
          "--sidebar-padding",
          "--sidebar-transition-duration",
          "--sidebar-width",
          "--sidebar-name-background",
          "--sidebar-name-color",
          '--sidebar-name-font-family',
          "--sidebar-name-font-size",
          "--sidebar-name-font-weight",
          "--sidebar-name-margin",
          "--sidebar-name-padding",
          "--sidebar-name-text-align",
          "--sidebar-nav-strong-border-color",
          "--sidebar-nav-strong-border-width",
          "--sidebar-nav-strong-color",
          "--sidebar-nav-strong-font-size",
          "--sidebar-nav-strong-font-weight",
          "--sidebar-nav-strong-margin",
          "--sidebar-nav-strong-padding",
          "--sidebar-nav-strong-text-transform",
          "--sidebar-nav-background",
          "--sidebar-nav-indent",
          "--sidebar-nav-margin",
          "--sidebar-nav-padding",
          "--sidebar-nav-link-background",
          "--sidebar-nav-link-background--active",
          "--sidebar-nav-link-background--hover",
          "--sidebar-nav-link-border-color",
          "--sidebar-nav-link-border-color--active",
          "--sidebar-nav-link-border-color--hover",
          "--sidebar-nav-link-border-radius",
          "--sidebar-nav-link-border-style",
          "--sidebar-nav-link-border-style--active",
          "--sidebar-nav-link-border-style--hover",
          "--sidebar-nav-link-border-width",
          "--sidebar-nav-link-border-width--active",
          "--sidebar-nav-link-border-width--hover",
          "--sidebar-nav-link-color",
          "--sidebar-nav-link-color--active",
          "--sidebar-nav-link-color--hover",
          "--sidebar-nav-link-font-weight",
          "--sidebar-nav-link-font-weight--active",
          "--sidebar-nav-link-font-weight--hover",
          "--sidebar-nav-link-margin",
          "--sidebar-nav-link-padding",
          "--sidebar-nav-link-text-decoration",
          "--sidebar-nav-link-text-decoration--active",
          "--sidebar-nav-link-text-decoration--hover",
          "--sidebar-nav-link-text-decoration-color",
          "--sidebar-nav-link-transition",
          "--sidebar-nav-link-before-content",
          "--sidebar-nav-link-before-content--active",
          "--sidebar-nav-link-before-content-l1",
          "--sidebar-nav-link-before-content-l1--active",
          "--sidebar-nav-link-before-content-l2",
          "--sidebar-nav-link-before-content-l2--active",
          "--sidebar-nav-link-before-content-l3",
          "--sidebar-nav-link-before-content-l3--active",
          "--sidebar-nav-link-before-content-l4",
          "--sidebar-nav-link-before-content-l4--active",
          "--sidebar-nav-link-before-color",
          "--sidebar-nav-link-before-color--active",
          "--sidebar-nav-link-before-color-l1",
          "--sidebar-nav-link-before-color-l1--active",
          "--sidebar-nav-link-before-color-l2",
          "--sidebar-nav-link-before-color-l2--active",
          "--sidebar-nav-link-before-color-l3",
          "--sidebar-nav-link-before-color-l3--active",
          "--sidebar-nav-link-before-color-l4",
          "--sidebar-nav-link-before-color-l4--active",
          "--sidebar-nav-link-before-margin",
          "--sidebar-nav-link-before-margin-l1",
          "--sidebar-nav-link-before-margin-l2",
          "--sidebar-nav-link-before-margin-l3",
          "--sidebar-nav-link-before-margin-l4",
          "--sidebar-nav-pagelink-background",
          "--sidebar-nav-pagelink-background--active",
          "--sidebar-nav-pagelink-background--collapse",
          "--sidebar-nav-pagelink-background--loaded",
          "--sidebar-nav-pagelink-padding",
          "--sidebar-nav-pagelink-transition",
          "--sidebar-toggle-background",
          "--sidebar-toggle-border-color",
          "--sidebar-toggle-border-radius",
          "--sidebar-toggle-border-style",
          "--sidebar-toggle-border-width",
          "--sidebar-toggle-height",
          "--sidebar-toggle-icon-color",
          "--sidebar-toggle-icon-height",
          "--sidebar-toggle-icon-stroke-width",
          "--sidebar-toggle-icon-width",
          "--sidebar-toggle-offset-left",
          "--sidebar-toggle-offset-top",
          "--sidebar-toggle-width",
          "--copycode-background",
          "--copycode-color",
          "--pagination-border-top",
          "--pagination-chevron-height",
          "--pagination-chevron-stroke",
          "--pagination-chevron-stroke-linecap",
          "--pagination-chevron-stroke-width",
          "--pagination-label-color",
          "--pagination-label-font-size",
          "--pagination-title-color",
          "--pagination-title-font-size",
          "--search-background",
          "--search-margin",
          "--search-padding",
          "--search-clear-icon-color1",
          "--search-clear-icon-color2",
          "--search-input-background-color",
          "--search-input-background-color--focus",
          "--search-input-background-image",
          "--search-input-background-image--focus",
          "--search-input-background-position",
          "--search-input-background-position--focus",
          "--search-input-background-repeat",
          "--search-input-background-size",
          "--search-input-background-size--focus",
          "--search-input-border-color",
          "--search-input-border-radius",
          "--search-input-border-width",
          "--search-input-color",
          "--search-input-font-size",
          "--search-input-margin",
          "--search-input-padding",
          "--search-input-placeholder-color",
          "--search-input-transition",
          "--search-flex-order",
          "--search-result-heading-color",
          "--search-result-heading-font-size",
          "--search-result-heading-font-weight",
          "--search-result-heading-margin",
          "--search-result-item-border-color",
          "--search-result-item-border-style",
          "--search-result-item-border-width",
          "--search-result-item-color",
          "--search-result-item-font-size",
          "--search-result-item-font-weight",
          "--search-result-item-margin",
          "--search-result-item-padding",
          "--search-result-keyword-background",
          "--search-result-keyword-border-radius",
          "--search-result-keyword-color",
          "--search-result-keyword-font-weight",
          "--search-result-keyword-margin",
          "--search-result-keyword-padding",
          "--docsifytabs-border-color",
          "--docsifytabs-border-radius-px",
          "--docsifytabs-tab-background",
          "--docsifytabs-tab-color",
          "--zoomimage-overlay-background",
          "--code-font-size",
          "--code-font-weight",
          "--code-block-border-radius",
          "--code-block-line-height",
          "--code-block-margin",
          "--code-block-padding",
        ],
      };

      const STYLE_ID = "themeable-setting";

      // 新增安全解析函数
      function safeJsonParse(str, defaultValue = {}) {
        try {
          return JSON.parse(str) || defaultValue;
        } catch {
          return defaultValue;
        }
      }

      function init() {
        applyStyles();
        createControlPanel();
        cssChange.init();
      }

      // 获取初始默认值
      let defaultValues = {};
      config.customVars.forEach((varName) => {
        defaultValues[varName] = getComputedStyle(document.documentElement)
          .getPropertyValue(varName)
          .trim();
      });

      // 应用CSS变量
      // 应用CSS变量
      function applyStyles() {
        const savedStyles = safeJsonParse(localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS), {});

        // 处理当本地存储为空的情况
        if (Object.keys(savedStyles).length === 0) {
          const existingStyle = document.getElementById(STYLE_ID);
          if (existingStyle) existingStyle.remove();
          return;
        }

        // 创建或获取现有的style标签
        const style = document.getElementById(STYLE_ID) || document.createElement("style");
        style.id = STYLE_ID;

        // 获取所有可能的变量键（default和saved的并集）
        const keys = new Set([...Object.keys(defaultValues), ...Object.keys(savedStyles)]);

        // 生成合并后的样式，优先使用非空的saved值，否则使用default值
        const mergedStyles = {};
        keys.forEach(key => {
          const savedValue = savedStyles[key];
          const defaultValue = defaultValues[key];
          mergedStyles[key] = (savedValue !== null && savedValue !== undefined && savedValue !== '')
            ? savedValue
            : defaultValue;
        });

        // 过滤掉saved和default同时为空的变量
        const filteredEntries = Object.entries(mergedStyles).filter(([key]) => {
          const savedValue = savedStyles[key];
          const defaultValue = defaultValues[key];
          const isSavedEmpty = savedValue === null || savedValue === undefined || savedValue === '';
          const isDefaultEmpty = defaultValue === null || defaultValue === undefined || defaultValue === '';
          return !(isSavedEmpty && isDefaultEmpty);
        });

        // 生成CSS变量字符串
        const cssVariables = filteredEntries
          .map(([key, value]) => `${key}: ${value};`)
          .join("");

        // 更新或创建样式标签
        style.textContent = `:root {${cssVariables}}`;

        if (!document.getElementById(STYLE_ID)) {
          document.head.appendChild(style);
        }
      }

      // 获取当前生效的CSS变量值
      function getCurrentValue(varName) {
        return getComputedStyle(document.documentElement)
          .getPropertyValue(varName)
          .trim();
      }

      // 颜色工具函数
      const ColorUtils = {
        // 颜色标准化
        normalize: (value) => {
          const div = document.createElement("div");
          div.style.color = value;
          document.body.appendChild(div);
          const color = getComputedStyle(div).color;
          document.body.removeChild(div);
          return color || value;
        },

        luminance(color) {
          // 新增防御性检查
          if (!color || typeof color !== "string") return 0;

          const rgbValues = this.extractRGB(color);
          if (!rgbValues) return 0;

          const [r, g, b] = rgbValues.map((v) => {
            const normalized = v / 255;
            return normalized <= 0.03928
              ? normalized / 12.92
              : Math.pow((normalized + 0.055) / 1.055, 2.4);
          });

          return 0.2126 * r + 0.7152 * g + 0.0722 * b;
        },

        // 新增RGB提取方法
        extractRGB(color) {
          const rgbRegex = /rgba?\((\d+),\s*(\d+),\s*(\d+)/i;
          const hexRegex = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i;

          // 处理rgb格式
          if (rgbRegex.test(color)) {
            const match = color.match(rgbRegex);
            return match ? [match[1], match[2], match[3]].map(Number) : null;
          }

          // 处理hex格式
          if (hexRegex.test(color)) {
            const hex = color.replace("#", "");
            const fullHex =
              hex.length === 3
                ? hex
                  .split("")
                  .map((c) => c + c)
                  .join("")
                : hex;
            const result = hexRegex.exec(fullHex);
            return result
              ? [
                parseInt(result[1], 16),
                parseInt(result[2], 16),
                parseInt(result[3], 16),
              ]
              : null;
          }

          return null;
        },

        contrast(color1, color2 = "#ffffff") {
          try {
            const lum1 = this.luminance(color1) + 0.05;
            const lum2 = this.luminance(color2) + 0.05;
            return (Math.max(lum1, lum2) / Math.min(lum1, lum2)).toFixed(2);
          } catch {
            return 0;
          }
        },
      };

      // 创建控制面板
      function createControlPanel() {
        // 在控制面板创建部分修改如下
        const panelHTML = `
          <div class="modal-mask">
            <div id="css-var-control-panel" class="modal-wrapper">
              <div class="modal-container">
                <div class="modal-header">
                  <h3>页面变量配置(操作有危险,更改需谨慎)</h3>   
                  <div class="header-left">
                    <div class="clear-all-wrapper">
                      <div class="search-box">
                        <input type="text" placeholder="搜索变量..." class="search-input">
                        <div class="search-history"></div>
                          <svg class="search-icon" viewBox="0 0 24 24" width="18">
                            <path d="M15.5 14h-.79l-.28-.27A6.47 6.47 0 0 0 16 9.5 6.5 6.5 0 1 0 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"/>
                          </svg>
                        </div>
                      </div>
                    </div>
                    <div>
                      <button class="fa fa-trash-can clear-all"> 删除</button>
                      <button class="fa fa-circle-xmark close-btn"></button>
                    </div>
                  </div>
                  <div class="modal-body">
                    <div class="table-wrapper"></div>
                  </div>
                  <div class="modal-footer">
                    <button class="fa fa-plus add-row">  修改CSS</button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        `;

        // 在样式部分添加以下优化代码
        const style = document.createElement("style");
        style.textContent = `
        /* 使用固定单位避免受CSS变量影响 */
        #css-var-control-panel {
          --local-font-size: 14px;
          font-size: 14px !important;
        }

        .modal-container {
          width: 100%;
          min-width: 800px !important;
          max-width: 90vw !important;
          max-height: 90vh;
          background: #fff;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.33);
          display: flex;
          flex-direction: column;
        }

        .modal-header h3 {
          margin: 0;
          font-size: 18px !important;
        }

        #css-var-control-panel td {
          padding: 10px 15px !important;
          background: #fff;
          vertical-align: middle;
          border-bottom: 1px solid #e9ecef;
          font-size: var(--local-font-size);
        }

        #css-var-control-panel input {
          height: 32px !important;
          padding: 6px 8px;
          border: 1px solid #ced4da;
          border-radius: 4px;
          font-size: 14px !important;
          transition: border 0.2s;
        }

        .modal-mask {
          position: fixed;
          z-index: 9998;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(0, 0, 0, 0.5);
          display: none;
          justify-content: center;
          align-items: center;
        }

        .modal-wrapper {
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 100%;
          padding: 20px;
        }

        .modal-header {
          padding: 20px;
          border-bottom: 1px solid #e8e8e8;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }

        .modal-body {
          padding: 20px;
          overflow: auto;
          flex: 1;
        }

        .modal-footer {
          padding: 15px 20px;
          border-top: 1px solid #e8e8e8;
          text-align: right;
        }

        .close-btn {
          font-size: 14px;
          line-height: 1;
          margin-left: 15px;
        }

        #css-var-control-panel .table-wrapper {
          overflow: auto;
          max-height: 50vh;
          border: 1px solid #e8e8e8;
          border-radius: 4px;
          margin: 12px 0;
        }

        #css-var-control-panel table {
          width: 100%;
          min-width: 600px;
          border-collapse: separate;
          border-spacing: 0;
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
        }

        #css-var-control-panel thead th {
          position: sticky;
          top: 0;
          background: #f8f9fa;
          color: #495057;
          font-weight: 600;
          padding: 12px 15px;
          border-bottom: 2px solid #dee2e6;
          z-index: 1;
        }

        #css-var-control-panel tbody tr:nth-child(even) td {
          background-color: #f8f9fa;
        }

        #css-var-control-panel tbody tr:hover td {
          background-color: #e9f5ff;
          transition: background 0.2s ease;
        }

        #css-var-control-panel td:last-child {
          white-space: nowrap;
        }

        #css-var-control-panel button {
          padding: 5px 12px;
          margin: 2px;
          border: 1px solid #ced4da;
          border-radius: 4px;
          background: #fff;
          cursor: pointer;
          transition: all 0.2s;
        }

        #css-var-control-panel button:hover {
          background: #4dabf7;
          border-color: #4dabf7;
          color: white;
          transform: translateY(-1px);
        }

        #css-var-control-panel input:focus {
          border-color: #4dabf7;
          outline: none;
          box-shadow: 0 0 0 2px rgba(77, 171, 247, 0.2);
        }

        #css-var-control-panel input[type="color"] {
          height: 34px;
          padding: 3px;
          cursor: pointer;
        }

        #css-var-control-panel .search-box {
          position: relative;
          margin-left: 20px;
          flex: 1;
          max-width: 300px;
        }

        #css-var-control-panel .search-input {
          width: 100%;
          padding: 8px 30px 8px 12px !important;
          border: 1px solid #ddd !important;
          border-radius: 20px !important;
          font-size: 14px !important;
          background: #f5f5f5 !important;
          transition: all 0.3s !important;
        }

        #css-var-control-panel .search-input:focus {
          background: #fff !important;
          box-shadow: 0 0 0 2px rgba(77, 171, 247, 0.2) !important;
        }

        #css-var-control-panel .search-icon {
          position: absolute;
          right: 12px;
          top: 50%;
          transform: translateY(-50%);
          fill: #666;
        }

        #css-var-control-panel .header-left {
          display: flex;
          align-items: center;
          gap: 20px;
          flex: 1;
        }

        #css-var-control-panel .search-history {
          position: absolute;
          top: 100%;
          left: 0;
          right: 0;
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 4px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-top: 5px;
          max-height: 200px;
          overflow-y: auto;
          z-index: 10000;
          display: none;
        }

        #css-var-control-panel .history-item {
          padding: 8px 12px;
          cursor: pointer;
          transition: background 0.2s;
        }

        #css-var-control-panel .history-item:hover {
          background: #f5f5f5;
        }

        #css-var-control-panel .color-preview {
          display: inline-block;
          width: 20px;
          height: 20px;
          border: 1px solid #ddd;
          border-radius: 4px;
          margin-left: 8px;
          vertical-align: middle;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        #css-var-control-panel .color-input-group {
          display: flex;
          align-items: center;
          gap: 8px;
        }

        #css-var-control-panel .color-input {
          width: 100% !important;
          min-width: 10em !important;
        }

        #css-var-control-panel .contrast-warning {
          color: #ff4444;
          cursor: help;
          position: relative;
        }

        #css-var-control-panel .contrast-warning:hover::after {
          content: "对比度不足（最小建议4.5）";
          position: absolute;
          background: #fff;
          border: 1px solid #ddd;
          padding: 4px;
          border-radius: 4px;
          top: 100%;
          left: 0;
          white-space: nowrap;
        }

        #css-var-control-panel .color-display {
          display: inline-flex;
          align-items: center;
          gap: 8px;
        }

        #css-var-control-panel .color-value {
          font-family: monospace;
          font-size: 0.9em;
        }

        @media screen and (max-width: 768px) {
          #css-var-control-panel {
            --local-font-size: 12px;
          }

          .modal-container {
            min-width: 100% !important;
            max-width: 100vw !important;
            border-radius: 0;
            max-height: 100vh;
            margin: 0;
          }

          .modal-wrapper {
            padding: 0;
            align-items: flex-end;
          }

          .modal-header {
            padding: 12px !important;
            flex-wrap: wrap;
          }

          .modal-header h3 {
            font-size: 16px !important;
            width: 100%;
            margin-bottom: 8px !important;
          }

          .search-box {
            margin-left: 0 !important;
            max-width: 100% !important;
          }

          #css-var-control-panel td {
            padding: 8px 10px !important;
            min-width: 80px;
          }

          #css-var-control-panel input {
            height: 36px !important;
            padding: 8px !important;
          }

          .table-wrapper {
            height: 60vh !important;
            margin: 8px 0 !important;
          }

          #css-var-control-panel table {
            min-width: 100%;
          }

          #css-var-control-panel td:last-child {
            white-space: normal;
          }

          #css-var-control-panel button {
            padding: 8px 12px !important;
            margin: 4px !important;
            width: 100%;
          }

          .color-input-group {
            flex-direction: column;
          }

          .color-input {
            width: 100% !important;
          }

          .contrast-warning {
            display: none;
          }

          .modal-footer {
            padding: 12px !important;
          }

          .modal-body {
            padding: 12px !important;
          }

          input[type="color"] {
            height: 40px !important;
            width: 40px !important;
          }

          /* 表格移动端适配 */
          #css-var-control-panel table,
          #css-var-control-panel thead,
          #css-var-control-panel tbody,
          #css-var-control-panel th,
          #css-var-control-panel td,
          #css-var-control-panel tr {
            display: block;
            width: 100% !important;
            min-width: 0 !important;
          }

          #css-var-control-panel thead {
            position: absolute;
            opacity: 0;
            pointer-events: none;
          }

          #css-var-control-panel tbody tr {
            display: flex;
            flex-direction: column;
            margin: 12px 0;
            padding: 16px;
            border: 1px solid #eee;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.05);
            background: #fff;
          }

          #css-var-control-panel td {
            position: relative;
            padding: 12px 8px 8px 25% !important;
            border: none !important;
            min-height: 48px;
          }

          #css-var-control-panel td::before {
            content: attr(data-label);
            position: absolute;
            left: 8px;
            top: 50%;
            transform: translateY(-50%);
            width: 20%;
            padding-right: 10px;
            font-weight: 500;
            color: #666;
            font-size: 0.9em;
          }

          #css-var-control-panel td:last-child {
            border-top: 1px solid #eee !important;
            padding-top: 16px !important;
            margin-top: 8px;
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
          }

          #css-var-control-panel td:last-child::before {
            content: "操作";
            width: 100%;
            transform: translateY(-50%);
            margin-bottom: 8px;
          }

          .color-input-group {
            flex-direction: row !important;
          }

          #css-var-control-panel button {
            width: auto !important;
            flex: 1;
            min-width: 80px;
          }

          #css-var-control-panel td:last-child {
            flex-direction: row !important;
            gap: 8px;
            padding-top: 12px !important;
          }

          #css-var-control-panel button[data-action] {
            flex: none !important;
            width: auto !important;
            min-width: 70px;
            padding: 8px 12px !important;
          }

          /* 横屏优化 */
          @media (orientation: landscape) {
            .table-wrapper {
              max-height: 50vh !important;
            }
          }
        }`;
        document.head.appendChild(style);

        const panel = document.createElement("div");
        panel.innerHTML = panelHTML;

        // 搜索功能实现
        let searchTimer;
        function handleSearch() {
          const searchValue = panel.querySelector(".search-input").value;
          updateHistory(searchValue);
          const conditions = parseSearch(searchValue);

          panel.querySelectorAll("tbody tr").forEach((tr) => {
            tr.style.display = matchRow(tr, conditions) ? "" : "none";
          });
        }

        // 在handleSearch函数前添加以下代码
        function parseSearch(input) {
          const conditions = [];
          const parts = input.toLowerCase().split(/\s+/);

          parts.forEach((part) => {
            if (part.includes(":")) {
              const [key, value] = part.split(":").map((s) => s.trim());
              if (key && value) {
                conditions.push({ type: "field", key, value });
              }
            } else if (part) {
              conditions.push({ type: "global", value: part });
            }
          });

          return conditions;
        }

        function matchRow(row, conditions) {
          const data = JSON.parse(row.dataset.original);

          return conditions.every((condition) => {
            if (condition.type === "field") {
              switch (condition.key) {
                case "name":
                  return data.name.includes(condition.value);
                case "value":
                  return (
                    data.current.includes(condition.value) ||
                    data.new.includes(condition.value)
                  );
                case "default":
                  return data.default.includes(condition.value);
                default:
                  return false;
              }
            }
            return [data.name, data.current, data.default, data.new].some((v) =>
              v.includes(condition.value)
            );
          });
        }

        // 键盘导航支持
        panel
          .querySelector(".search-input")
          .addEventListener("keydown", (e) => {
            const items = panel.querySelectorAll(".history-item");
            let current = [...items].findIndex((item) =>
              item.classList.contains("selected")
            );

            if (e.key === "ArrowDown") {
              current = Math.min(current + 1, items.length - 1);
            } else if (e.key === "ArrowUp") {
              current = Math.max(current - 1, -1);
            } else if (e.key === "Enter" && current >= 0) {
              panel.querySelector(".search-input").value =
                items[current].textContent;
              handleSearch();
            }

            items.forEach((item) => item.classList.remove("selected"));
            if (current >= 0) items[current].classList.add("selected");
          });

        // 新增历史记录功能
        const MAX_HISTORY = 5;
        let history = safeJsonParse(
          localStorage.getItem("cssVarSearchHistory"),
          []
        );

        function updateHistory(searchValue) {
          if (!searchValue) return;

          // 去重并保持最新
          history = [
            searchValue,
            ...history.filter((v) => v !== searchValue),
          ].slice(0, MAX_HISTORY);

          localStorage.setItem("cssVarSearchHistory", JSON.stringify(history));
          renderHistory();
        }

        function renderHistory() {
          const historyBox = panel.querySelector(".search-history");
          historyBox.innerHTML = history
            .map(
              (item) => `
        <div class="history-item">${item}</div>
      `
            )
            .join("");
        }

        // 新增排序方法
        function getSortedVariables(defaultVars, savedVars) {
          const savedKeys = Object.keys(savedVars);

          // 分离已保存和未保存变量
          const [savedVariables, unsavedVariables] = [
            [...new Set([...defaultVars, ...savedKeys])] // 合并所有变量
              .filter((varName) => savedKeys.includes(varName)), // 已保存
            defaultVars.filter((varName) => !savedKeys.includes(varName)), // 未保存
          ];

          // 合并并去重（保持顺序）
          return [...new Set([...savedVariables, ...unsavedVariables])];
        }

        // 颜色值检测函数
        function isColor(value) {
          const colorRegex =
            /^(#([0-9a-f]{3}){1,2}|rgb\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\)|rgba\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*[\d.]+\s*\)|hsl\(\s*\d+\s*,\s*[\d.]+%,\s*[\d.]+%\)|hsla\(\s*\d+\s*,\s*[\d.]+%,\s*[\d.]+%,\s*[\d.]+\))$/i;
          return colorRegex.test(value.trim());
        }

        // 生成颜色预览块
        function createColorPreview(colorValue) {
          return isColor(colorValue)
            ? `<span class="color-preview" style="background:${colorValue}"></span>`
            : "";
        }

        function renderTable() {
          const savedVars = safeJsonParse(
            localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS),
            {}
          );
          // 新增排序逻辑
          const currentVars = getSortedVariables(config.customVars, savedVars);

          const tableContent = `
              <table>
                <thead>
                  <tr>
                    <th>变量名</th>
                    <th>当前值</th>
                    <th>默认值</th>
                    <th>新值</th>
                    <th>操作</th>
                  </tr>
                </thead>
                <tbody>
                  ${currentVars
              .map((varName) => {
                const currentValue = getComputedStyle(document.documentElement)
                  .getPropertyValue(varName)
                  .trim();
                const defaultValue = defaultValues[varName] || currentValue;
                const savedValue = savedVars[varName] || "";

                // 标准化颜色
                const normalizedCurrent = ColorUtils.normalize(currentValue);
                const normalizedDefault = ColorUtils.normalize(defaultValue);
                const normalizedSaved = ColorUtils.normalize(savedValue);

                // 判断是否为颜色值
                const isColorCurrent = isColor(currentValue);
                const isColorDefault = isColor(defaultValue);
                const isColorSaved = isColor(currentValue);

                // 对比度警告
                const showWarning =
                  isColorSaved && ColorUtils.contrast(normalizedSaved) < 4.5;

                return `
                      <tr>
                        <td data-label="变量名">${varName}</td>
                        <td data-label="当前值">
                          ${isColorCurrent
                    ? colorPreview(normalizedCurrent)
                    : currentValue
                  }
                        </td>
                        <td td data-label="默认值">
                          ${isColorDefault
                    ? colorPreview(normalizedDefault)
                    : defaultValue
                  }
                        </td>
                        <td data-label="新值">
                          <div class="color-input-group">
                            ${isColorSaved
                    ? `
                              <input 
                                type="color" 
                                value="${savedValue}"
                                data-var="${varName}"
                                title="点击选择颜色"
                              >
                              <input
                                type="text"
                                class="color-input"
                                value="${savedValue}"
                                data-var="${varName}"
                              >
                            `
                    : `
                              <input
                                type="text"
                                class="color-input"
                                value="${savedValue}"
                                data-var="${varName}"
                              >
                            `
                  }
                            ${showWarning
                    ? '<span class="contrast-warning">⚠</span>'
                    : ""
                  }
                          </div>
                        </td>
                        <td data-label="操作">
                          <div class="button-group">
                            <button class = "fa fa-floppy-disk" data-action="save" data-var="${varName}"> 保存</button>
                            <button class = "fa fa-repeat" data-action="reset" data-var="${varName}"> 重置</button>
                          <div class="button-group">
                        </td>
                      </tr>
                    `;
              })
              .join("")}
                </tbody>
              </table>
            `;

          panel.querySelector(".table-wrapper").innerHTML = tableContent;

          // 同步颜色输入
          panel.querySelectorAll(".color-input-group").forEach((group) => {
            const colorPicker = group.querySelector('input[type="color"]');
            const textInput = group.querySelector('input[type="text"]');

            if (colorPicker && textInput) {
              const syncValues = (source, target) => {
                target.value = source.value;
                const varName = source.dataset.var;
                // document.documentElement.style.setProperty(varName, source.value);
              };

              colorPicker.addEventListener("input", () =>
                syncValues(colorPicker, textInput)
              );
              textInput.addEventListener("input", () =>
                syncValues(textInput, colorPicker)
              );
            }
          });

          // 添加数据属性保存原始值
          currentVars.forEach((varName) => {
            const row = panel
              .querySelector(`[data-var="${varName}"]`)
              .closest("tr");
            row.dataset.original = JSON.stringify({
              name: varName,
              current: getCurrentValue(varName),
              default: defaultValues[varName],
              new: savedVars[varName] || "",
            });
          });

          handleSearch(); // 初始化时应用搜索过滤
        }

        // 辅助函数
        function isColor(value) {
          return /^(#|rgb|hsl)/.test(value);
        }

        function colorPreview(color) {
          return `<span style="display:inline-flex;align-items:center;gap:4px">
                ${color}
                <span class="color-preview" style="background:${color}"></span>
              </span>
            `;
        }

        function displayTable() {
          document.querySelector(".modal-mask").style.display = "flex";
          renderTable();
        };

        // 事件绑定
        panel.querySelector(".clear-all").addEventListener("click", () => {
          localStorage.removeItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS);
          applyStyles();
          renderTable();
        });

        // 搜索
        panel.querySelector(".search-input").addEventListener("input", () => {
          clearTimeout(searchTimer);
          searchTimer = setTimeout(handleSearch, 300);
        });

        // 事件绑定
        panel.querySelector(".search-input").addEventListener("focus", () => {
          panel.querySelector(".search-history").style.display = "block";
        });

        document.addEventListener("click", (e) => {
          if (!e.target.closest(".search-box")) {
            panel.querySelector(".search-history").style.display = "none";
          }
        });

        panel
          .querySelector(".search-history")
          .addEventListener("click", (e) => {
            if (e.target.classList.contains("history-item")) {
              const value = e.target.textContent;
              panel.querySelector(".search-input").value = value;
              handleSearch();
            }
          });

        // 点击关闭按钮
        panel.querySelector(".close-btn").addEventListener("click", () => {
          document.querySelector(".modal-mask").style.display = "none";
        });

        // 事件监听修复：确保元素存在
        setTimeout(() => {
          const modalMask = document.querySelector(".modal-mask");
          if (modalMask) {
            // 点击遮罩层关闭
            modalMask.addEventListener("click", (e) => {
              if (e.target === modalMask) {
                modalMask.style.display = "none";
              }
            });

            // ESC键关闭
            document.addEventListener("keydown", (e) => {
              if (e.key === "Escape" && modalMask.style.display === "flex") {
                modalMask.style.display = "none";
              }
            });
          }
        }, 100); // 等待DOM渲染完成

        panel.querySelector(".add-row").addEventListener("click", () => {
          cssChange.show();
          // const newVarName = prompt(
          //   "请输入新的CSS变量名（例如：--new-variable）"
          // );

          // if (config.customVars.includes(newVarName)) return

          // if (newVarName && newVarName.startsWith("--")) {
          //   config.customVars.push(newVarName);
          //   defaultValues[newVarName] = getComputedStyle(
          //     document.documentElement
          //   )
          //     .getPropertyValue(newVarName)
          //     .trim();
          //   renderTable();
          // }
        });

        panel.addEventListener("click", (e) => {
          if (e.target.dataset.action === "save") {
            const varName = e.target.dataset.var;
            const newValue = e.target
              .closest("tr")
              .querySelector("input").value;
            const saved = JSON.parse(
              localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS) || "{}"
            );
            saved[varName] = newValue;
            localStorage.setItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS, JSON.stringify(saved));
            applyStyles();
            renderTable();
          }

          if (e.target.dataset.action === "reset") {
            const varName = e.target.dataset.var;
            const saved = JSON.parse(
              localStorage.getItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS) || "{}"
            );
            delete saved[varName];
            localStorage.setItem(LS_THEME_ABLE_MANAGER_DOCSIFY_CSS_VARS, JSON.stringify(saved));
            applyStyles();
            renderTable();
          }
        });

        document.body.appendChild(panel);

        const btnCreateControlPanel = document.getElementById('createControlPanel');
        if (btnCreateControlPanel) {
          btnCreateControlPanel.addEventListener("click", () => {
            displayTable();
          });
        }
      }

      return {
        init,
      }
    })();

    // 亮暗主题
    const prismThemeManager = (function () {
      const themes = [
        "prism.min.css",
        "prism-coy.min.css",
        "prism-dark.min.css",
        "prism-funky.min.css",
        "prism-okaidia.min.css",
        "prism-solarizedlight.min.css",
        "prism-tomorrow.min.css",
        "prism-twilight.min.css",
      ];

      let currentIndex = 0;
      let styleElement = null;

      function init() {
        const savedTheme = localStorage.getItem("LS_PRISM_THEME_MANAGER_PRISM_THEME");
        currentIndex = themes.findIndex((t) => t === savedTheme);
        if (currentIndex === -1) currentIndex = 0;
        applyTheme(currentIndex);
      }

      function nextTheme() {
        applyTheme(currentIndex + 1);
      }

      function applyTheme(index) {
        currentIndex = (index + themes.length) % themes.length;
        const themeUrl = `//cdn.jsdelivr.net/npm/prismjs/themes/${themes[currentIndex]}`;

        if (!styleElement) {
          styleElement = document.createElement("link");
          styleElement.rel = "stylesheet";
          document.head.appendChild(styleElement);
        }

        styleElement.href = themeUrl;
        localStorage.setItem("LS_PRISM_THEME_MANAGER_PRISM_THEME", themes[currentIndex]);
      }

      return {
        init,
        nextTheme,
        applyTheme,
        getCurrentTheme: () => themes[currentIndex],
        getThemes: () => [...themes],
      };
    })();

    // 按钮组配置
    const buttons = [
      {
        icon: "fas fa-gears",
        text: "设置",
        id: 'createControlPanel',
        // 在移动端隐藏这个按钮
        hideOnTouch: false,
      },
      {
        get icon() {
          return `fas ${themeManager.isDark ? "fa-sun" : "fa-moon"}`;
        },
        get text() {
          return themeManager.isDark ? "亮色主题" : "暗色主题";
        },
        action: () => themeManager.toggle(),
        isThemeButton: true,
      },
      {
        icon: "fas fa-palette",
        text: "代码主题",
        action: () => prismThemeManager.nextTheme(),
      },
      {
        icon: "fas fa-angle-up",
        text: "顶部",
        action: () => window.scrollTo(0, 0),
      },
      {
        icon: "fas fa-home",
        text: "首页",
        action: () => (window.location.href = "/notes/#/"),
      },
    ];

    const isTouchDevice = 'ontouchstart' in window || navigator.maxTouchPoints > 0;

    // 创建按钮容器
    const container = document.createElement("div");
    container.className = "flat-button-container";
    document.body.appendChild(container);

    // 创建触发区域
    const trigger = document.createElement("div");
    trigger.className = "button-trigger";
    document.body.appendChild(trigger);

    // 创建按钮
    buttons.filter(btn => !(isTouchDevice && btn.hideOnTouch)).
      forEach((btn, index) => {
        const btnElement = document.createElement("button");
        btnElement.className = "flat-button";
        if (btn.id) btnElement.id = btn.id;

        btnElement.innerHTML = `<i class="${typeof btn.icon === "function" ? btn.icon() : btn.icon}"></i>`;

        if (btn.action) btnElement.addEventListener("click", btn.action);

        const label = document.createElement("span");
        label.className = "button-label";
        label.textContent = typeof btn.text === "function" ? btn.text() : btn.text;
        btnElement.appendChild(label);

        if (btn.isThemeButton) themeManager.registerThemeButton(btnElement);
        container.appendChild(btnElement);
      });

    // 样式修改
    const style = document.createElement("style");
    style.textContent = `
      .button-trigger {
        position: fixed;
        bottom: 0px;
        right: 0px;
        width: 40px;
        height: 200px;
        background: transparent;
        z-index: 1000;
        cursor: pointer;
        transition: opacity 0.3s;
      }

      .flat-button-container {
        position: fixed;
        bottom: 20px;
        right: 20px;
        display: flex;
        gap: 15px;
        z-index: 999;
        flex-direction: column-reverse;
        align-items: flex-end;
        opacity: 0;
        pointer-events: none;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }

      .flat-button-container.visible {
        opacity: 1;
        pointer-events: auto;
      }

      .flat-button {
        width: 40px;
        height: 40px;
        border-radius: 20px;
        background: #42b983;
        border: none;
        cursor: pointer;
        box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        color: white;
      }

      /* 保持原有悬停效果 */
      .flat-button:hover {
        width: auto;
        padding: 0 15px;
        border-radius: 20px;
        transform: translateX(-8px);
        background: #42b983dd;
      }

      .button-label {
        max-width: 0;
        overflow: hidden;
        opacity: 0;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        white-space: nowrap;
        pointer-events: none;
        position: absolute;
        right: calc(100% + 10px); /* 初始位置在按钮左侧外 */
        top: 50%;
        transform: translateY(-50%);
        background: rgba(0, 0, 0, 0.6); /* 提高透明度 */
        padding: 5px 12px;
        border-radius: 4px;
        color: white;
        font-size: 14px;
        backdrop-filter: blur(8px); /* 增强模糊效果 */
        border: 1px solid rgba(255,255,255,0.15);
        box-shadow: 0 4px 12px rgba(0,0,0,0.25);

     }
      .flat-button:hover .button-label {
        max-width: 200px;
        opacity: 0.9; /* 悬停时稍提高可见性 */
        right: calc(100% + 10px); /* 最终停留在按钮左侧 */
      }

      /* 箭头调整到右侧 */
      .button-label::after {
        content: '';
        position: absolute;
        left: 100%;
        top: 50%;
        transform: translateY(-50%);
        border-width: 5px;
        border-style: solid;
        border-color: transparent transparent transparent rgba(0, 0, 0, 0.6);
      }

      /* 按钮悬停动画优化 */
      .flat-button:hover {
        transform: translateX(-8px); /* 向左移动留出标签空间 */
        background: #42b983dd; /* 带透明度的背景色 */
      }
`;
    document.head.appendChild(style);

    // 事件处理
    trigger.addEventListener('mouseenter', () => {
      container.classList.add('visible');
      trigger.style.display = 'none';
    });

    container.addEventListener('mouseleave', () => {
      container.classList.remove('visible');
      trigger.style.display = 'block';
    });

    // 2. 如果是触摸设备，则用 tap 事件代替 hover
    if (isTouchDevice) {
      // 点击 trigger 显示按钮
      trigger.addEventListener('touchstart', (e) => {
        e.stopPropagation();
        container.classList.add('visible');
        trigger.style.display = 'none';
      });

      // 点击任何其他地方收起按钮
      document.addEventListener('touchstart', (e) => {
        // 如果点击目标在 container 内就忽略
        if (!container.contains(e.target)) {
          container.classList.remove('visible');
          trigger.style.display = 'block';
        }
      });

      // 阻止 container 内部 touch 事件冒泡到 document
      container.addEventListener('touchstart', (e) => {
        e.stopPropagation();
      });
    }
    prismThemeManager.init(); //代码样式
    themeAbleManager.init(); //自定义CSS样式
  });
});
