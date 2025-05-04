window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  hook.mounted(function () {
    // 加载FontAwesome（保持不变）
    if (!document.querySelector('#font-awesome-css')) {
      const link = document.createElement('link');
      link.rel = 'stylesheet';
      link.href = 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css';
      link.id = 'font-awesome-css';
      document.head.appendChild(link);
    }

    // 按钮组配置
    const buttons = [
      {
        icon: 'fas fa-palette',
        text: '代码主题',
        action: () => prismThemeManager.nextTheme()
      },
      {
        get icon() {
          return `fas ${themeManager.isDark ? 'fa-sun' : 'fa-moon'}`
        },
        get text() {  // 新增动态文本
          return themeManager.isDark ? '亮色主题' : '暗色主题';
        },
        action: () => themeManager.toggle()
      },
      {
        icon: 'fas fa-angle-up',
        text: '顶部',
        action: () => window.scrollTo(0, 0)
      },
      {
        icon: 'fas fa-home',
        text: '首页',
        action: () => window.location.href = '/'
      },

    ]

    const themeManager = (function () {
      // 创建主题控制器对象（使用构造函数模式）
      function ThemeSwitcher() {
        this.isDark = false;
        this.lightTheme = null;
        this.darkTheme = null;
        this.themeButtons = new Set();
      }

      ThemeSwitcher.prototype = {
        init() {
          this.lightTheme = document.querySelector('link[title="light"]');
          this.darkTheme = document.querySelector('link[title="dark"]');
          this.isDark = localStorage.getItem("docsifyTheme") === "dark";

          // 增强的系统主题检测逻辑
          const systemIsDark = window.matchMedia("(prefers-color-scheme: dark)").matches;
          if (!localStorage.getItem("docsifyTheme")) {
            this.isDark = systemIsDark;
          }

          this.applyTheme();
          this.updateThemeElements(); // 重命名方法
          return this;
        },

        toggle() {
          this.isDark = !this.isDark;
          this.applyTheme();
          this.updateThemeElements(); // 重命名方法
          // 触发自定义事件
          const event = new CustomEvent('theme-change', {
            detail: { isDark: this.isDark }
          });
          document.dispatchEvent(event);
        },

        // 更新所有主题相关元素
        updateThemeElements() {
          const iconClass = this.isDark ? 'fa-sun' : 'fa-moon';
          const labelText = this.isDark ? '亮色主题' : '暗色主题';

          this.themeButtons.forEach(button => {
            // 更新图标
            const icon = button.querySelector('i');
            if (icon) {
              icon.className = `fas ${iconClass}`;
            }

            // 更新标签
            const label = button.querySelector('.button-label');
            if (label) {
              label.textContent = labelText;
            }
          });
        },

        // 新增注册按钮方法
        registerThemeButton(buttonElement) {
          this.themeButtons.add(buttonElement);
          // 初始化按钮状态
          this.updateThemeElements();
        },

        applyTheme() {
          if (!this.lightTheme || !this.darkTheme) {
            console.error('Theme stylesheets not found');
            return;
          }

          this.darkTheme.disabled = !this.isDark;
          this.lightTheme.disabled = this.isDark;
          localStorage.setItem("docsifyTheme", this.isDark ? "dark" : "light");

          // 更新文档属性
          document.documentElement.setAttribute(
            'data-theme',
            this.isDark ? 'dark' : 'light'
          );
        }
      };

      // 创建单例实例
      const instance = new ThemeSwitcher().init();

      return {
        get isDark() { return instance.isDark; },
        toggle: () => instance.toggle(),
        applyTheme: (isDark) => instance.applyTheme(),
        registerThemeButton: (element) => instance.registerThemeButton(element),
      };
    })();

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
        const savedTheme = localStorage.getItem('prism-theme');
        currentIndex = themes.findIndex(t => t === savedTheme);
        if (currentIndex === -1) currentIndex = 0;
        applyTheme(currentIndex);
      }

      function nextTheme() {
        applyTheme(currentIndex + 1);
      }

      function applyTheme(index) {
        currentIndex = (index + themes.length) % themes.length;
        const themeUrl = `//cdn.jsdelivr.net/npm/prismjs/themes/${themes[currentIndex]}`;

        https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-numbers/prism-line-numbers.min.css

        if (!styleElement) {
          styleElement = document.createElement('link');
          styleElement.rel = 'stylesheet';
          document.head.appendChild(styleElement);
        }

        styleElement.href = themeUrl;
        localStorage.setItem('prism-theme', themes[currentIndex]);
      }

      return {
        init,
        nextTheme,
        applyTheme,
        getCurrentTheme: () => themes[currentIndex],
        getThemes: () => [...themes]
      };

    })();


    // 创建容器（保持不变）
    const container = document.createElement('div');
    container.className = 'flat-button-container';
    document.body.appendChild(container);

    // 创建按钮（简化逻辑）
    buttons.forEach((btn, index) => {
      const btnElement = document.createElement('button');
      btnElement.className = 'flat-button';
      // 动态获取图标
      btnElement.innerHTML = `<i class="${typeof btn.icon === 'function' ? btn.icon() : btn.icon}"></i>`;
      btnElement.addEventListener('click', btn.action);

      // 添加文字标签（支持动态文本）
      const label = document.createElement('span');
      label.className = 'button-label';
      label.textContent = typeof btn.text === 'function' ? btn.text() : btn.text;
      btnElement.appendChild(label);

      // 注册主题按钮
      if (btn.action.toString().includes('themeManager.toggle')) {
        themeManager.registerThemeButton(btnElement);
      }

      container.appendChild(btnElement);
    });

    // 修改后的样式
    const style = document.createElement('style');
    style.textContent = `
    .flat-button-container {
      position: fixed;
      bottom: 20px;
      right: 20px;
      display: flex;
      gap: 15px;
      z-index: 999;
      flex-direction: column-reverse;
      align-items: flex-end;
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

    .flat-button:hover {
      width: auto;
      padding: 0 15px;
      border-radius: 20px;
      transform: scale(1.05) translateX(-5px); /* 添加向左微移 */
    }

    .flat-button:hover .button-label {
      max-width: 200px;
      opacity: 1;
      margin-left: 10px;
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

  });
});