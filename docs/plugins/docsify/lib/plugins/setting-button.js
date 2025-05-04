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

    const themeManager = (function () {
      // 创建主题控制器对象（使用构造函数模式）
      function ThemeSwitcher() {
        this.isDark = false;
        this.lightTheme = null;
        this.darkTheme = null;
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
          return this;
        },

        toggle() {
          this.isDark = !this.isDark;
          this.applyTheme();
          // 触发自定义事件
          const event = new CustomEvent('theme-change', {
            detail: { isDark: this.isDark }
          });
          document.dispatchEvent(event);
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
        applyTheme: (isDark) => {
          instance.isDark = !!isDark;
          instance.applyTheme();
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

    // 按钮组配置（保持不变）
    const buttonGroups = [
      {
        icon: 'fa-solid fa-gears',
        subButtons: [
          {
            icon: 'fas fa-palette',
            text: '代码主题',
            action: () => prismThemeManager.nextTheme()
          },
          {
            get icon() {
              return `fas fa-${themeManager.isDark ? 'moon' : 'sun'}`
            },
            text: '页面主题',
            action: () => themeManager.toggle()
          },

        ]
      },
      {
        icon: 'fa-solid fa-house',
        subButtons: [
          { icon: 'fas fa-home', text: '首页', action: () => window.location.href = '/' },
          { icon: 'fa-solid fa-angle-up', text: '顶部', action: () => window.scrollTo(0, 0) },
        ]
      }
    ];

    // 创建容器（保持不变）
    const container = document.createElement('div');
    container.className = 'multi-button-container';
    document.body.appendChild(container);

    // 创建按钮组（修改定位逻辑）
    buttonGroups.forEach((group, index) => {
      const groupElement = document.createElement('div');
      groupElement.className = 'button-group';

      // 主按钮（保持不变）
      const mainBtn = document.createElement('button');
      mainBtn.className = 'main-button';
      mainBtn.innerHTML = `<i class="${group.icon}"></i>`;

      // 子按钮容器（修改定位方式）
      const subContainer = document.createElement('div');
      subContainer.className = 'sub-buttons';

      // 创建子按钮（保持不变）
      group.subButtons.forEach((sub, subIndex) => {
        const subBtn = document.createElement('button');
        subBtn.className = 'sub-button';
        subBtn.innerHTML = `
          <i class="${sub.icon}"></i>`;
        subBtn.addEventListener('click', sub.action);
        subContainer.appendChild(subBtn);
      });

      // 修改后的点击事件处理
      let hideTimer;
      mainBtn.addEventListener('mouseenter', (e) => {
        e.stopPropagation();

        // 清除可能存在的计时器
        clearHide();

        // 强制显示子菜单以获取实际尺寸（隐藏状态）
        subContainer.style.display = 'flex';
        subContainer.style.visibility = 'hidden';

        // 获取主按钮位置
        const rect = mainBtn.getBoundingClientRect();

        // 动态计算子菜单总高度
        let totalHeight = 0;
        const subButtons = subContainer.querySelectorAll('.sub-button');

        if (subButtons.length > 0) {
          // 获取第一个子按钮高度
          const firstButtonHeight = subButtons[0].offsetHeight;

          // 计算实际间隙（兼容gap和margin方案）
          let actualGap = 0;
          if (subButtons.length >= 2) {
            const rect1 = subButtons[0].getBoundingClientRect();
            const rect2 = subButtons[1].getBoundingClientRect();
            actualGap = rect2.top - rect1.bottom;
          }

          // 计算总高度：首按钮高度 + (按钮数量-1)*(按钮高度+间隙)
          totalHeight = firstButtonHeight;
          for (let i = 1; i < subButtons.length; i++) {
            const btnHeight = subButtons[i].offsetHeight;
            totalHeight += btnHeight + actualGap;
          }
        }

        // 计算理论定位坐标
        let targetTop = rect.top - totalHeight;
        const targetRight = window.innerWidth - rect.right;

        // 边界检测（保持最小顶部间距）
        const minTopMargin = 10;
        if (targetTop < minTopMargin) {
          // 当空间不足时改为向下展开
          targetTop = rect.bottom;
        }

        // 应用最终定位
        subContainer.style.top = `${targetTop + 15}px`;
        subContainer.style.right = `${targetRight + 40}px`; // 60px为右侧间距

        // 显示元素
        subContainer.style.visibility = 'visible';
        mainBtn.classList.add('rotating');

        // 添加自动隐藏延时（可选）
        const setHide = () => {
          hideTimer = setTimeout(() => {
            subContainer.style.display = 'none';
          }, 1000);
        };

        // 主按钮移出处理
        const handleMainLeave = (e) => {
          // 检查是否移动到子菜单
          if (!subContainer.contains(e.relatedTarget)) {
            setHide();
          }
        };

        // 子菜单移出处理
        const handleSubLeave = (e) => {
          // 检查是否移动回主按钮
          if (!mainBtn.contains(e.relatedTarget)) {
            setHide();
          }
        };

        // 主按钮和子菜单的移出控制
        mainBtn.addEventListener('mouseleave', handleMainLeave);
        subContainer.addEventListener('mouseenter', clearHide);
        subContainer.addEventListener('mouseleave', handleSubLeave);

        // 旋转动画（保持不变）
        mainBtn.classList.add('rotating');
        setTimeout(() => mainBtn.classList.remove('rotating'), 500);

        closeAllSubMenus();

        subContainer.style.display = 'flex';
        // 添加边界检测（防止超出视口顶部）
        const subContainerRect = subContainer.getBoundingClientRect();
        if (subContainerRect.top < 0) {
          subContainer.style.top = '10px'; // 保持与视口顶部的最小间距
        }

      });

      // 公共方法
      const clearHide = () => {
        if (hideTimer) {
          clearTimeout(hideTimer);
          hideTimer = null;
        }
      };

      // 子菜单添加移入保护
      subContainer.addEventListener('mouseenter', clearHide);

      groupElement.appendChild(mainBtn);
      groupElement.appendChild(subContainer);
      container.appendChild(groupElement);
    });

    // 关闭所有子菜单（保持不变）
    function closeAllSubMenus() {
      document.querySelectorAll('.sub-buttons').forEach(el => {
        el.style.display = 'none';
      });
    }

    // 动态定位（简化版本）
    function updatePositions() {
      const baseBottom = 30;
      const baseRight = 20;
      const groupSpacing = 50;

      document.querySelectorAll('.button-group').forEach((group, index) => {
        group.style.bottom = `${baseBottom + index * groupSpacing}px`;
        group.style.right = `${baseRight}px`;
      });
    }

    // 点击外部关闭（保持不变）
    document.addEventListener('click', closeAllSubMenus);

    // 修改后的样式
    const style = document.createElement('style');
    style.textContent = `
      .multi-button-container {
        position: fixed;
        bottom: 0;
        right: 0;
        z-index: 999;
      }

      .button-group {
        position: fixed;
        transition: all 0.3s;
      }

      .main-button {
        width: 30px;
        height: 30px;
        border-radius: 50%;
        background: #42b983;
        color: white;
        border: none;
        cursor: pointer;
        box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.3s;
      }

      .main-button:hover {
        transform: scale(1.1);
      }

      /* 旋转动画 */
      .main-button.rotating {
        animation: rotate 0.5s ease-in-out;
      }

      @keyframes rotate {
        from { transform: rotate(0deg) scale(1.1); }
        to { transform: rotate(180deg) scale(1.1); }
      }

      .sub-buttons {
        display: none;
        position: fixed;
        flex-direction: column;
        gap: 15px;          /* 保持间隙 */
        z-index: 1000;
        transform-origin: bottom center;  /* 添加展开动画锚点 */
        animation: menuSlide 0.3s ease;   /* 添加展开动画 */
      }

      @keyframes menuSlide {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
        
      .sub-button {
        width: 25px;
        height: 25px;
        border-radius: 50%;
        background: #fff;
        border: none;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        transition: all 0.3s;
        display: flex;
        align-items: center;
        justify-content: center;
        position: relative;
      }
    `;
    document.head.appendChild(style);

    // 初始化主按钮位置
    updatePositions();
    window.addEventListener('resize', updatePositions);
    window.addEventListener('scroll', closeAllSubMenus);
  });
});