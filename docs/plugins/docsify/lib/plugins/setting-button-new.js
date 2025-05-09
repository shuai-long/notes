const LS_THEME_MANAGER_DOCSIFY_THEME = 'docsifyTheme';
const LS_PRISM_THEME_MANAGER_PRISM_THEME = "prism-theme";
const LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS = 'docsify-custom-css';
const LS_CSS_CHANGE_DOCSIFY_CSS_VAR_SEARCH_HISTORY = 'cssVarSearchHistory';

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

    // css 样式切换
    const cssChange = (function () {

      let modal, textarea;
      let config = { cssVariables: [] }; // 存储配置信息

      // 新增配置文件加载方法
      async function loadConfig() {
        try {
          const response = await fetch('/notes/metadata/css-vars-config.json'); // 配置文件路径
          config = await response.json();
          if (!Array.isArray(config.cssVariables)) {
            console.error('配置文件格式错误，应为 { "cssVariables": [...] }');
            config.cssVariables = [];
          }
        } catch (error) {
          console.error('配置文件加载失败:', error);
          config.cssVariables = [];
        }
      }


      // 创建样式隔离的模态框
      function createModal() {
        const modal = document.createElement('div');
        const shadow = modal.attachShadow({ mode: 'open' });

        // Shadow DOM内部样式
        const style = document.createElement('style');
        style.textContent = `
        :host {
          display: none !important;
          justify-content: center;
          align-items: flex-start;
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(0,0,0,0.6);
          backdrop-filter: blur(3px);
          z-index: 10000;
          transition: opacity 0.3s ease;
          overflow: auto;
          padding: 40px 20px;
          opacity: 0;
        }
      
        :host(.active) {
          display: flex !important;
          justify-content: center;
          align-items: flex-start;
          opacity: 1;
        }
      
        .container {
          display: flex;
          flex-direction: row;
          background: #f8f9fa;
          border-radius: 12px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.15);
          width: 90%;
          max-width: 90%
          min-height: 60vh;
          max-height: 90vh;
          transform: translateY(-20px);
          transition: transform 0.3s ease;
          position: relative;
        }
      
        :host(.active) .container {
          transform: translateY(0);
        }
      
        /* 左侧面板 */
        .left-panel {
          flex: 0 0 40%;
          display: flex;
          flex-direction: column;
          border-right: 1px solid rgba(0,0,0,0.08);
          background: linear-gradient(145deg, #ffffff 0%, #f8f9fa 100%);
          padding: 25px;
        }
      
        /* 右侧面板 */
        .right-panel {
          flex: 1;
          display: flex;
          flex-direction: column;
          padding: 25px;
          position: relative;
          min-width: 0; /* 修复内容溢出 */
        }
        .color-block{
          width: 1em;
          height: 1em;
          margin-left: .5em;
        }

        .vars-container {
            height: 78%;
            overflow-y: auto;
            overflow-x: hidden; /* 新增：隐藏横向溢出 */
            border-radius: 8px;
            border: 1px solid #e0e3e7;
            background: #fff;
            margin: 10px auto;
            width: 100%;
            max-width: 800px;
            box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);
        }

        .vertical-pro-table {
            width: 100%;
            border-collapse: collapse;
            table-layout: fixed; /* 新增：固定表格布局 */
        }

        /* 核心布局 */
        .vertical-pro-table tr {
            display: flex;
            align-items: center;
            padding: 12px 20px;
            min-height: 52px;
            box-sizing: border-box;
            width: 100%; /* 新增：确保宽度不溢出 */
        }

        .vertical-pro-table th {
            flex: 0 0 18%; /* 固定30%宽度 */
            color: #1a365d;
            font-weight: 600;
            font-size: 14px;
            padding-right: 15px;
            border-right: 2px solid #edf2f7;
            box-sizing: border-box; /* 新增：盒模型计算 */
            min-width: 0; /* 新增：允许收缩 */
            text-align: left;
        }

        .vertical-pro-table td {
            flex: 1; /* 自动填充剩余70% */
            display: flex;
            align-items:center;
            position: relative;
            color: #4a5568;
            font-size: 14px;
            padding-left: 10px;
            line-height: 1.5;
            box-sizing: border-box; /* 新增：盒模型计算 */
            min-width: 0; /* 新增：允许收缩 */
            overflow-wrap: break-word; /* 新增：强制换行 */
        }

        /* 双行斑马纹 */
        .vertical-pro-table tr:nth-child(4n+1),
        .vertical-pro-table tr:nth-child(4n+2) {
            background: #d2dbe5;
        }

        .vertical-pro-table tr:nth-child(4n+3),
        .vertical-pro-table tr:nth-child(4n+4) {
            background: #fff;
        }

        /* 滚动条美化 */
        .vars-container::-webkit-scrollbar {
            width: 8px;
            background: #f1f5f9;
        }

        .vars-container::-webkit-scrollbar-thumb {
            background: #cbd5e1;
            border-radius: 4px;
        }

        /* 分隔线优化 */
        .vertical-pro-table tr:not(:last-child) {
            border-bottom: 1px solid #e2e8f0;
        }

        /* 搜索容器 */
        .search-box{
          padding-top: .1em;
        }

        /* 搜索输入框 */
        .search-input {
          width: 70%;
          padding: 5px 20px;
          font-size: 15px;
          border: 2px solid #e2e8f0;
          border-radius: 8px;
          background: white;
          box-shadow: 0 2px 4px rgba(160,174,192,0.1);
          transition: all 0.3s ease;
          box-sizing: border-box;
          margin-left: 1em;
        }

        .search-input:focus {
          outline: none;
          border-color: #4299e1;
          box-shadow: 0 2px 8px rgba(66,153,225,0.2);
        }

        /* 搜索历史容器 */
        .search-history {
          margin-top: 12px;
          display: flex;
          align-items: center;
          flex-wrap: wrap;
          gap: 8px;
          color: #718096;
          font-size: 13px;
        }

        /* 历史记录项 */
        .history-item {
          display: inline-flex;
          align-items: center;
          background: #f7fafc;
          border-radius: 6px;
          cursor: pointer;
          transition: all 0.2s ease;
          border: 1px solid #e2e8f0;
          color: #4a5568;
        }

        .history-item:hover {
          background: #ebf8ff;
          border-color: #bee3f8;
          transform: translateY(-1px);
        }

        .history-item::before {
          content: "#";
          margin-right: 4px;
          color: #a0aec0;
        }

        .close-btn {
          position: absolute;
          top: 10px;
          right: 10px;
          width: 32px;
          height: 32px;
          border: none;
          border-radius: 50%;
          background: #f0f0f0;
          cursor: pointer;
          font-size: 24px;
          display: flex;
          align-items: center;
          justify-content: center;
          transition: all 0.2s ease;
        }

        .close-btn:hover {
          background: #e0e0e0;
          transform: rotate(90deg);
        }

        .button-group {
          margin-top: 15px;
          display: flex;
          gap: 12px;
          justify-content: flex-end;
        }

        button {
          padding: 10px 24px;
          border: none;
          border-radius: 6px;
          font-weight: 500;
          cursor: pointer;
          transition: 
            transform 0.1s ease,
            box-shadow 0.2s ease,
            background 0.2s ease;
        }

        button:active {
          transform: translateY(1px);
        }

        .reset {
          background: #f5f5f5;
          color: #666;
        }

        .reset:hover {
          background: #eee;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        .save {
          background: #2196F3;
          color: white;
        }

        .save:hover {
          background: #1976D2;
          box-shadow: 0 2px 8px rgba(33,150,243,0.3);
        }

        textarea {
          min-height: 400px;
          margin: 15px 0;
          padding: 12px;
          border: 1px solid #ddd;
          border-radius: 4px;
          resize: vertical;
          font-family: monospace;
        }            
      `;
        // 模态框内容
        const container = document.createElement('div');
        container.className = 'container';
        container.innerHTML = `
        <div class="left-panel">
          <div class="search-box">
            <h3 style="display:inline-block;margin-bottom: 0;">CSS变量列表</h3>
            <input type="text" placeholder="搜索变量..." class="search-input">
          </div>
          <div class="search-history"></div>
          <div class="vars-container"> <!-- 新增滚动容器 -->
            <table class="vertical-pro-table">
              <tbody class="vars-body"></tbody>
            </table>
          </div>
        </div>
        <div class="right-panel">
          <button class="close-btn">×</button>
          <h3>自定义CSS</h3>
          <textarea placeholder="输入CSS样式..."></textarea>
          <div class="button-group">
            <button class="reset">重置</button>
            <button class="save">保存</button>
          </div>
        </div>
      `;

        shadow.appendChild(style);
        shadow.appendChild(container);
        document.body.appendChild(modal);
        return modal;
      }

      // 修改后的获取CSS变量方法
      function getCSSVariables() {
        const styles = getComputedStyle(document.documentElement);
        return config.cssVariables.map(varName => {
          const value = styles.getPropertyValue(varName).trim();
          return {
            name: varName,
            value: value,
            isColor: /^(#|rgb|hsl)/.test(value)
          };
        });
      }

      // 生成变量表格
      function renderVariables(searchTerm = '') {
        const variables = getCSSVariables();
        const hasSearch = searchTerm.trim().length > 0;

        // 始终显示所有变量，搜索时进行过滤
        const filtered = hasSearch ?
          variables.filter(v =>
            v.name.toLowerCase().includes(searchTerm) ||
            v.value.toLowerCase().includes(searchTerm)
          ) : variables;

        const tbody = modal.shadowRoot.querySelector('.vars-body');
        tbody.innerHTML = filtered.map(v => `
          <tr>
            <th>变量名</th>
            <td>${v.name}</td>
          <tr>
            <th>变量值</th>
            <td>
              ${v.value}
              ${v.isColor ? `<div class="color-block" style="background:${v.value}"></div>` : ''}
            </td>
          </tr>
        `).join('');

        // 添加空状态提示
        if (filtered.length === 0) {
          tbody.innerHTML = `
            <tr>
              <td colspan="2" style="text-align:center;color:#999">
                ${hasSearch ? '没有找到匹配的变量' : '暂无配置变量'}
              </td>
            </tr>
          `;
        }
      }

      // 处理搜索历史
      function updateSearchHistory(term) {
        const history = JSON.parse(localStorage.getItem(LS_CSS_CHANGE_DOCSIFY_CSS_VAR_SEARCH_HISTORY) || '[]');
        if (!term || history.includes(term)) return;

        history.unshift(term);
        if (history.length > 5) history.pop();
        localStorage.setItem(LS_CSS_CHANGE_DOCSIFY_CSS_VAR_SEARCH_HISTORY, JSON.stringify(history));

        const historyEl = modal.shadowRoot.querySelector('.search-history');
        historyEl.innerHTML = '最近搜索: ' + history.map(item =>
          `<span class="history-item">${item}</span>`
        ).join('');

        // 添加历史点击事件
        historyEl.querySelectorAll('.history-item').forEach(item => {
          item.addEventListener('click', () => {
            const input = modal.shadowRoot.querySelector('.search-input');
            input.value = item.textContent;
            renderVariables(item.textContent);
          });
        });
      }

      // 初始化搜索功能
      function initSearch() {
        // const searchBtn = modal.shadowRoot.querySelector('.search-btn');
        const searchInput = modal.shadowRoot.querySelector('.search-input');
        const term = searchInput.value.toLowerCase();
        const handleSearch = () => {
          renderVariables(term);
        };

        // 初始化时显示所有变量
        renderVariables();

        // searchBtn.addEventListener('click', handleSearch);
        searchInput.addEventListener('input', handleSearch); // 改为实时搜索
        searchInput.addEventListener('keypress', e => {
          if (e.key === 'Enter') handleSearch();
        });
        searchInput.addEventListener('blur', function () {
          if (term !== '') {
            updateSearchHistory(term);
          }
        });
      }

      // 应用保存的样式
      function applySavedStyles() {
        const savedCss = localStorage.getItem(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS);
        if (savedCss) {
          // 移除旧样式
          const oldStyle = document.getElementById(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS);
          if (oldStyle) oldStyle.remove();

          // 添加新样式
          const style = document.createElement('style');
          style.id = LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS;
          style.textContent = savedCss;
          document.head.appendChild(style);
        }
      }

      function show() {
        modal.classList.add('active')
        document.body.style.overflow = 'hidden';
      }

      function init() {
        loadConfig(); // 先加载配置
        addEvents();
        applySavedStyles();
        initSearch();
      }

      // 初始化操作
      function addEvents() {
        // 创建界面元素
        modal = createModal();
        textarea = modal.shadowRoot.querySelector('textarea');

        // 加载已保存的CSS
        textarea.value = localStorage.getItem(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS) || '';

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
          document.body.style.overflow = 'auto';
        });

        // 新增重置按钮事件
        modal.shadowRoot.querySelector('.reset').addEventListener('click', () => {
          if (confirm('确定要重置所有自定义样式吗？')) {
            // 清除本地存储
            localStorage.removeItem(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS);
            // 移除页面样式
            const oldStyle = document.getElementById(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS);
            if (oldStyle) oldStyle.remove();
            // 清空编辑器内容
            textarea.value = '';
            // 关闭模态框
            // modal.classList.remove('active');
            // document.body.style.overflow = 'auto';
          }
        });

        // 验证CSS有效性
        modal.shadowRoot.querySelector('.save').addEventListener('click', () => {
          try {
            const css = textarea.value;
            const style = document.createElement('style');
            style.textContent = css;
            document.head.appendChild(style);
            style.remove();

            // 保存到localStorage
            localStorage.setItem(LS_CSS_CHANGE_DOCSIFY_CUSTOM_CSS, css);
            // modal.classList.remove('active');
            // document.body.style.overflow = 'auto';
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
        const savedTheme = localStorage.getItem(LS_PRISM_THEME_MANAGER_PRISM_THEME);
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
        localStorage.setItem(LS_PRISM_THEME_MANAGER_PRISM_THEME, themes[currentIndex]);
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
        hideOnTouch: true,
        action: () => cssChange.show(),
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
    cssChange.init();
  });
});
