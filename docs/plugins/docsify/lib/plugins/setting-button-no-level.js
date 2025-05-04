; (function () {
  // 页面加载时立即执行，读取本地存储或系统偏好确定主题
  var theme = localStorage.getItem('docsifyTheme');
  if (!theme && window.matchMedia) {
    theme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  }
  if (theme) {
    var lightLink = document.querySelector('link[title="light"]');
    var darkLink = document.querySelector('link[title="dark"]');
    // 启用/禁用对应的样式表
    if (lightLink && darkLink) {
      if (theme === 'dark') {
        lightLink.disabled = true;
        darkLink.disabled = false;
      } else {
        lightLink.disabled = false;
        darkLink.disabled = true;
      }
    }
    // 存储当前主题，保证下次加载时也能直接应用
    localStorage.setItem('docsifyTheme', theme);
  }
})();

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
        action: () => window.location.href = '/notes/#/'
      },

    ]

    const themeManager = (() => {
      // 读取本地存储的主题设置（'light' 或 'dark'），如无则使用系统首选项
      let currentTheme;
      try {
        currentTheme = localStorage.getItem('docsifyTheme');
      } catch (e) {
        currentTheme = null;
      }
      if (!currentTheme) {
        // 如果 localStorage 没有，则根据 prefers-color-scheme 判断
        currentTheme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
      }

      // 将 <html> 的 data-theme 属性设置为当前主题，便于 CSS 根据其应用样式:contentReference[oaicite:4]{index=4}
      document.documentElement.setAttribute('data-theme', currentTheme);

      // 获取主题样式表的链接元素（假设在 <head> 中已有 light 和 dark 两个 <link>）
      const lightLink = document.getElementById('docsify-theme-light')
        || document.querySelector('link[data-theme="light"][rel="stylesheet"]');
      const darkLink = document.getElementById('docsify-theme-dark')
        || document.querySelector('link[data-theme="dark"][rel="stylesheet"]');

      // 根据当前主题立即启用对应样式表，禁用另一种，以避免闪烁:contentReference[oaicite:5]{index=5}
      if (lightLink) lightLink.disabled = (currentTheme !== 'light');
      if (darkLink) darkLink.disabled = (currentTheme !== 'dark');

      // 保存所有通过 registerThemeButton 注册的按钮引用，用于更新图标和文本
      const themeButtons =  new Set();

      // 切换主题：启用/禁用样式表，更新属性、存储和事件
      function toggle() {
        // 切换主题名称
        currentTheme = (currentTheme === 'dark' ? 'light' : 'dark');

        // 启用新主题的样式表，禁用旧主题的样式表
        if (lightLink) lightLink.disabled = (currentTheme !== 'light');
        if (darkLink) darkLink.disabled = (currentTheme !== 'dark');

        // 更新 <html> 的 data-theme 属性
        document.documentElement.setAttribute('data-theme', currentTheme);
        // 存储新的主题值到 localStorage
        try {
          localStorage.setItem('docsifyTheme', currentTheme);
        } catch (e) {
          // 忽略不可用 localStorage 的错误
        }

        // 更新所有已注册按钮的图标和文本
        updateThemeElements();

        // 派发主题变化事件，detail 可携带主题名
        const event = new CustomEvent('theme-change', { detail: currentTheme });
        document.dispatchEvent(event);
      }

      // 注册一个主题切换按钮：绑定点击事件，并添加到按钮列表中
      // 新增注册按钮方法
      function registerThemeButton(buttonElement) {
        themeButtons.add(buttonElement);
        // 初始化按钮状态
        updateThemeElements();
      }

      // function registerThemeButton(button) {
      //   if (!button) return;
      //   // 避免重复注册
      //   if (!themeButtons.includes(button)) {
      //     themeButtons.push(button);
      //     // 点击时触发主题切换
      //     button.addEventListener('click', toggle);
      //   }
      //   // 注册时立即更新按钮状态
      //   updateThemeElements();
      // }

      // 更新所有已注册按钮的显示（图标和文本）以匹配当前主题
      // 更新所有主题相关元素
      function updateThemeElements() {
        const iconClass = currentTheme === 'dark' ? 'fa-sun' : 'fa-moon';
        const labelText = currentTheme === 'dark' ? '亮色主题' : '暗色主题';

        themeButtons.forEach(button => {
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
      }


      // function updateThemeElements() {
      //   themeButtons.forEach(button => {
      //     const lightIcon = button.getAttribute('data-theme-light-icon') || '';
      //     const darkIcon = button.getAttribute('data-theme-dark-icon') || '';
      //     const lightText = button.getAttribute('data-theme-light-text') || '';
      //     const darkText = button.getAttribute('data-theme-dark-text') || '';

      //     // 试图查找定义好的图标和文本元素
      //     const iconElem = button.querySelector('.theme-icon');
      //     const textElem = button.querySelector('.theme-text');

      //     if (iconElem && textElem) {
      //       // 如果按钮内有相应元素，就分别更新它们
      //       if (currentTheme === 'dark') {
      //         iconElem.innerHTML = darkIcon;
      //         textElem.textContent = darkText;
      //       } else {
      //         iconElem.innerHTML = lightIcon;
      //         textElem.textContent = lightText;
      //       }
      //     } else {
      //       // 否则直接设置按钮的内容为“图标 + 文本”
      //       if (currentTheme === 'dark') {
      //         button.innerHTML = `${darkIcon}${darkText}`;
      //       } else {
      //         button.innerHTML = `${lightIcon}${lightText}`;
      //       }
      //     }
      //   });
      // }

      // 对外公开的方法和属性
      return {
        toggle,
        registerThemeButton,
        updateThemeElements,
        // isDark 为只读属性：当前主题是否为暗色
        get isDark() {
          return currentTheme === 'dark';
        }
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