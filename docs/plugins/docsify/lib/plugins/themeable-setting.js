window.$docsify.plugins = [].concat(function (hook, vm) {
  // 配置参数（只需维护变量名列表）
  const config = {
    customVars: [
      '--mono-hue',
      '--mono-saturation',
      '--mono-shade3',
      '--mono-shade2',
      '--mono-shade1',
      '--mono-base',
      '--mono-tint1',
      '--mono-tint2',
      '--mono-tint3',
      '--theme-hue',
      '--theme-saturation',
      '--theme-lightness',
      '--theme-color',
      '--modular-scale',
      '--modular-scale--2',
      '--modular-scale--1',
      '--modular-scale-1',
      '--modular-scale-2',
      '--modular-scale-3',
      '--modular-scale-4',
      '--modular-scale-5',
      '--font-size-xxxl',
      '--font-size-xxl',
      '--font-size-xl',
      '--font-size-l',
      '--font-size-m',
      '--font-size-s',
      '--font-size-xs',
      '--border-radius-s',
      '--border-radius-m',
      '--border-radius-l',
      '--duration-slow',
      '--duration-medium',
      '--duration-fast',
      '--spinner-size',
      '--spinner-track-width',
      '--spinner-track-color',
      '--spinner-transition-duration',
      '--base-background-color',
      '--base-color',
      // '--base-font-family',
      '--base-font-size',
      '--base-font-weight',
      '--base-letter-spacing',
      '--base-line-height',
      '--emoji-size',
      '--hr-border',
      '--mark-background',
      '--mark-color',
      // '--pre-font-family',
      '--pre-font-size',
      '--pre-font-weight',
      '--pre-line-height',
      '--selection-color',
      '--small-font-size',
      '--strong-color',
      '--strong-font-weight',
      '--subsup-font-size',
      '--content-max-width',
      '--blockquote-background',
      '--blockquote-border-color',
      '--blockquote-border-style',
      '--blockquote-border-width',
      '--blockquote-border-radius',
      '--blockquote-color',
      // '--blockquote-em-font-family',
      '--blockquote-em-font-size',
      '--blockquote-em-font-style',
      '--blockquote-em-font-weight',
      // '--blockquote-font-family',
      '--blockquote-font-size',
      '--blockquote-font-style',
      '--blockquote-font-weight',
      '--blockquote-quotes-close',
      '--blockquote-quotes-color',
      // '--blockquote-quotes-font-family',
      '--blockquote-quotes-font-size',
      '--blockquote-quotes-open',
      '--blockquote-padding',
      // '--code-font-family',
      '--code-font-size',
      '--code-font-weight',
      '--code-tab-size',
      '--code-block-border-radius',
      '--code-block-line-height',
      '--code-block-margin',
      '--code-block-padding',
      '--code-inline-background',
      '--code-inline-border-radius',
      '--code-inline-color',
      '--code-inline-margin',
      '--code-inline-padding',
      '--code-theme-background',
      '--code-theme-comment',
      '--code-theme-function',
      '--code-theme-keyword',
      '--code-theme-operator',
      '--code-theme-punctuation',
      '--code-theme-selection',
      '--code-theme-selector',
      '--code-theme-tag',
      '--code-theme-text',
      '--code-theme-variable',
      '--heading-color',
      // '--heading-font-family',
      '--heading-font-weight',
      '--heading-margin',
      '--heading-padding',
      '--heading-h1-border-color',
      '--heading-h1-border-style',
      '--heading-h1-border-width',
      '--heading-h1-color',
      // '--heading-h1-font-family',
      '--heading-h1-font-size',
      '--heading-h1-font-weight',
      '--heading-h1-margin',
      '--heading-h1-padding',
      '--heading-h2-border-color',
      '--heading-h2-border-style',
      '--heading-h2-border-width',
      '--heading-h2-color',
      // '--heading-h2-font-family',
      '--heading-h2-font-size',
      '--heading-h2-font-weight',
      '--heading-h2-margin',
      '--heading-h2-padding',
      '--heading-h3-border-color',
      '--heading-h3-border-style',
      '--heading-h3-border-width',
      '--heading-h3-color',
      // '--heading-h3-font-family',
      '--heading-h3-font-size',
      '--heading-h3-font-weight',
      '--heading-h3-margin',
      '--heading-h3-padding',
      '--heading-h4-border-color',
      '--heading-h4-border-style',
      '--heading-h4-border-width',
      '--heading-h4-color',
      // '--heading-h4-font-family',
      '--heading-h4-font-size',
      '--heading-h4-font-weight',
      '--heading-h4-margin',
      '--heading-h4-padding',
      '--heading-h5-border-color',
      '--heading-h5-border-style',
      '--heading-h5-border-width',
      '--heading-h5-color',
      // '--heading-h5-font-family',
      '--heading-h5-font-size',
      '--heading-h5-font-weight',
      '--heading-h5-margin',
      '--heading-h5-padding',
      '--heading-h6-border-color',
      '--heading-h6-border-style',
      '--heading-h6-border-width',
      '--heading-h6-color',
      // '--heading-h6-font-family',
      '--heading-h6-font-size',
      '--heading-h6-font-weight',
      '--heading-h6-margin',
      '--heading-h6-padding',
      '--kbd-background',
      '--kbd-border',
      '--kbd-border-radius',
      '--kbd-color',
      '--kbd-font-size',
      '--kbd-margin',
      '--kbd-min-width',
      '--kbd-padding',
      '--link-border-bottom',
      '--link-border-bottom--hover',
      '--link-color',
      '--link-color--hover',
      '--link-text-decoration',
      '--link-text-decoration--hover',
      '--link-text-decoration-color',
      '--link-text-decoration-color--hover',
      '--notice-background',
      '--notice-border-color',
      '--notice-border-radius',
      '--notice-border-style',
      '--notice-border-width',
      '--notice-color',
      // '--notice-font-family',
      '--notice-font-weight',
      '--notice-padding',
      '--notice-before-background',
      '--notice-before-border-radius',
      '--notice-before-color',
      '--notice-before-content',
      // '--notice-before-font-family',
      '--notice-before-font-size',
      '--notice-before-font-weight',
      '--notice-before-height',
      '--notice-before-left',
      '--notice-before-line-height',
      '--notice-before-margin',
      '--notice-before-padding',
      '--notice-before-position',
      '--notice-before-top',
      '--notice-before-width',
      '--notice-important-background',
      '--notice-important-border-color',
      '--notice-important-border-style',
      '--notice-important-border-width',
      '--notice-important-color',
      '--notice-important-before-background',
      '--notice-important-before-color',
      '--notice-important-before-content',
      '--notice-tip-background',
      '--notice-tip-border-color',
      '--notice-tip-border-style',
      '--notice-tip-border-width',
      '--notice-tip-color',
      '--notice-tip-before-background',
      '--notice-tip-before-color',
      '--notice-tip-before-content',
      '--table-body-border-color',
      '--table-body-border-width',
      '--table-cell-border-color',
      '--table-cell-border-width',
      '--table-cell-padding',
      '--table-head-background',
      '--table-head-border-color',
      '--table-head-border-width',
      '--table-head-font-weight',
      '--table-row-even-background',
      '--table-row-odd-background',
      '--cover-color',
      '--cover-margin',
      '--cover-max-width',
      '--cover-text-align',
      '--cover-background-blend-mode',
      '--cover-background-color',
      '--cover-background-image',
      '--cover-background-mask-color',
      '--cover-background-mask-opacity',
      '--cover-background-mask-visibility',
      '--cover-background-position',
      '--cover-background-repeat',
      '--cover-background-size',
      '--cover-blockquote-color',
      '--cover-blockquote-font-size',
      '--cover-border-inset',
      '--cover-border-color',
      '--cover-border-width',
      '--cover-button-background',
      '--cover-button-background--hover',
      '--cover-button-border',
      '--cover-button-border--hover',
      '--cover-button-border-radius',
      '--cover-button-box-shadow',
      '--cover-button-box-shadow--hover',
      '--cover-button-color',
      '--cover-button-color--hover',
      '--cover-button-padding',
      '--cover-button-text-decoration',
      '--cover-button-text-decoration--hover',
      '--cover-button-text-decoration-color',
      '--cover-button-text-decoration-color--hover',
      '--cover-button-transition',
      '--cover-button-primary-background',
      '--cover-button-primary-background--hover',
      '--cover-button-primary-border',
      '--cover-button-primary-border--hover',
      '--cover-button-primary-box-shadow',
      '--cover-button-primary-box-shadow--hover',
      '--cover-button-primary-color',
      '--cover-button-primary-color--hover',
      '--cover-button-primary-text-decoration',
      '--cover-button-primary-text-decoration--hover',
      '--cover-button-primary-text-decoration-color',
      '--cover-button-primary-text-decoration-color--hover',
      '--cover-heading-color',
      '--cover-heading-font-size',
      '--cover-heading-font-size-min',
      '--cover-heading-font-size-max',
      '--cover-heading-font-weight',
      '--cover-link-border-bottom',
      '--cover-link-border-bottom--hover',
      '--cover-link-color',
      '--cover-link-color--hover',
      '--cover-link-text-decoration',
      '--cover-link-text-decoration--hover',
      '--cover-link-text-decoration-color',
      '--cover-link-text-decoration-color--hover',
      '--navbar-root-background',
      '--navbar-root-background--active',
      '--navbar-root-background--hover',
      '--navbar-root-border-color',
      '--navbar-root-border-color--active',
      '--navbar-root-border-color--hover',
      '--navbar-root-border-radius',
      '--navbar-root-border-style',
      '--navbar-root-border-style--active',
      '--navbar-root-border-style--hover',
      '--navbar-root-border-width',
      '--navbar-root-color',
      '--navbar-root-color--active',
      '--navbar-root-color--hover',
      '--navbar-root-margin',
      '--navbar-root-padding',
      '--navbar-root-transition',
      '--navbar-root-text-decoration',
      '--navbar-root-text-decoration--active',
      '--navbar-root-text-decoration--hover',
      '--navbar-root-text-decoration-color',
      '--navbar-root-text-decoration-color--active',
      '--navbar-root-text-decoration-color--hover',
      '--navbar-menu-background',
      '--navbar-menu-border-color',
      '--navbar-menu-border-radius',
      '--navbar-menu-border-width',
      '--navbar-menu-box-shadow',
      '--navbar-menu-padding',
      '--navbar-menu-transition',
      '--navbar-menu-root-background',
      '--navbar-menu-root-background--active',
      '--navbar-menu-root-background--hover',
      '--navbar-menu-root-padding',
      '--navbar-menu-link-background',
      '--navbar-menu-link-background--active',
      '--navbar-menu-link-background--hover',
      '--navbar-menu-link-border-color',
      '--navbar-menu-link-border-color--active',
      '--navbar-menu-link-border-color--hover',
      '--navbar-menu-link-border-radius',
      '--navbar-menu-link-border-style',
      '--navbar-menu-link-border-style--active',
      '--navbar-menu-link-border-style--hover',
      '--navbar-menu-link-border-width',
      '--navbar-menu-link-color',
      '--navbar-menu-link-color--active',
      '--navbar-menu-link-color--hover',
      '--navbar-menu-link-margin',
      '--navbar-menu-link-padding',
      '--navbar-menu-link-text-decoration',
      '--navbar-menu-link-text-decoration--active',
      '--navbar-menu-link-text-decoration--hover',
      '--navbar-menu-link-text-decoration-color',
      '--navbar-menu-link-text-decoration-color--active',
      '--navbar-menu-link-text-decoration-color--hover',
      '--sidebar-background',
      '--sidebar-border-color',
      '--sidebar-border-width',
      '--sidebar-padding',
      '--sidebar-transition-duration',
      '--sidebar-width',
      '--sidebar-name-background',
      '--sidebar-name-color',
      // '--sidebar-name-font-family',
      '--sidebar-name-font-size',
      '--sidebar-name-font-weight',
      '--sidebar-name-margin',
      '--sidebar-name-padding',
      '--sidebar-name-text-align',
      '--sidebar-nav-strong-border-color',
      '--sidebar-nav-strong-border-width',
      '--sidebar-nav-strong-color',
      '--sidebar-nav-strong-font-size',
      '--sidebar-nav-strong-font-weight',
      '--sidebar-nav-strong-margin',
      '--sidebar-nav-strong-padding',
      '--sidebar-nav-strong-text-transform',
      '--sidebar-nav-background',
      '--sidebar-nav-indent',
      '--sidebar-nav-margin',
      '--sidebar-nav-padding',
      '--sidebar-nav-link-background',
      '--sidebar-nav-link-background--active',
      '--sidebar-nav-link-background--hover',
      '--sidebar-nav-link-border-color',
      '--sidebar-nav-link-border-color--active',
      '--sidebar-nav-link-border-color--hover',
      '--sidebar-nav-link-border-radius',
      '--sidebar-nav-link-border-style',
      '--sidebar-nav-link-border-style--active',
      '--sidebar-nav-link-border-style--hover',
      '--sidebar-nav-link-border-width',
      '--sidebar-nav-link-border-width--active',
      '--sidebar-nav-link-border-width--hover',
      '--sidebar-nav-link-color',
      '--sidebar-nav-link-color--active',
      '--sidebar-nav-link-color--hover',
      '--sidebar-nav-link-font-weight',
      '--sidebar-nav-link-font-weight--active',
      '--sidebar-nav-link-font-weight--hover',
      '--sidebar-nav-link-margin',
      '--sidebar-nav-link-padding',
      '--sidebar-nav-link-text-decoration',
      '--sidebar-nav-link-text-decoration--active',
      '--sidebar-nav-link-text-decoration--hover',
      '--sidebar-nav-link-text-decoration-color',
      '--sidebar-nav-link-transition',
      '--sidebar-nav-link-before-content',
      '--sidebar-nav-link-before-content--active',
      '--sidebar-nav-link-before-content-l1',
      '--sidebar-nav-link-before-content-l1--active',
      '--sidebar-nav-link-before-content-l2',
      '--sidebar-nav-link-before-content-l2--active',
      '--sidebar-nav-link-before-content-l3',
      '--sidebar-nav-link-before-content-l3--active',
      '--sidebar-nav-link-before-content-l4',
      '--sidebar-nav-link-before-content-l4--active',
      '--sidebar-nav-link-before-color',
      '--sidebar-nav-link-before-color--active',
      '--sidebar-nav-link-before-color-l1',
      '--sidebar-nav-link-before-color-l1--active',
      '--sidebar-nav-link-before-color-l2',
      '--sidebar-nav-link-before-color-l2--active',
      '--sidebar-nav-link-before-color-l3',
      '--sidebar-nav-link-before-color-l3--active',
      '--sidebar-nav-link-before-color-l4',
      '--sidebar-nav-link-before-color-l4--active',
      '--sidebar-nav-link-before-margin',
      '--sidebar-nav-link-before-margin-l1',
      '--sidebar-nav-link-before-margin-l2',
      '--sidebar-nav-link-before-margin-l3',
      '--sidebar-nav-link-before-margin-l4',
      '--sidebar-nav-pagelink-background',
      '--sidebar-nav-pagelink-background--active',
      '--sidebar-nav-pagelink-background--collapse',
      '--sidebar-nav-pagelink-background--loaded',
      '--sidebar-nav-pagelink-padding',
      '--sidebar-nav-pagelink-transition',
      '--sidebar-toggle-background',
      '--sidebar-toggle-border-color',
      '--sidebar-toggle-border-radius',
      '--sidebar-toggle-border-style',
      '--sidebar-toggle-border-width',
      '--sidebar-toggle-height',
      '--sidebar-toggle-icon-color',
      '--sidebar-toggle-icon-height',
      '--sidebar-toggle-icon-stroke-width',
      '--sidebar-toggle-icon-width',
      '--sidebar-toggle-offset-left',
      '--sidebar-toggle-offset-top',
      '--sidebar-toggle-width',
      '--copycode-background',
      '--copycode-color',
      '--pagination-border-top',
      '--pagination-chevron-height',
      '--pagination-chevron-stroke',
      '--pagination-chevron-stroke-linecap',
      '--pagination-chevron-stroke-width',
      '--pagination-label-color',
      '--pagination-label-font-size',
      '--pagination-title-color',
      '--pagination-title-font-size',
      '--search-background',
      '--search-margin',
      '--search-padding',
      '--search-clear-icon-color1',
      '--search-clear-icon-color2',
      '--search-input-background-color',
      '--search-input-background-color--focus',
      '--search-input-background-image',
      '--search-input-background-image--focus',
      '--search-input-background-position',
      '--search-input-background-position--focus',
      '--search-input-background-repeat',
      '--search-input-background-size',
      '--search-input-background-size--focus',
      '--search-input-border-color',
      '--search-input-border-radius',
      '--search-input-border-width',
      '--search-input-color',
      '--search-input-font-size',
      '--search-input-margin',
      '--search-input-padding',
      '--search-input-placeholder-color',
      '--search-input-transition',
      '--search-flex-order',
      '--search-result-heading-color',
      '--search-result-heading-font-size',
      '--search-result-heading-font-weight',
      '--search-result-heading-margin',
      '--search-result-item-border-color',
      '--search-result-item-border-style',
      '--search-result-item-border-width',
      '--search-result-item-color',
      '--search-result-item-font-size',
      '--search-result-item-font-weight',
      '--search-result-item-margin',
      '--search-result-item-padding',
      '--search-result-keyword-background',
      '--search-result-keyword-border-radius',
      '--search-result-keyword-color',
      '--search-result-keyword-font-weight',
      '--search-result-keyword-margin',
      '--search-result-keyword-padding',
      '--docsifytabs-border-color',
      '--docsifytabs-border-radius-px',
      '--docsifytabs-tab-background',
      '--docsifytabs-tab-color',
      '--zoomimage-overlay-background',
      '--code-font-size',
      '--code-font-weight',
      '--code-block-border-radius',
      '--code-block-line-height',
      '--code-block-margin',
      '--code-block-padding'
    ]

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
  }

  // 获取初始默认值
  let defaultValues = {};
  config.customVars.forEach((varName) => {
    defaultValues[varName] = getComputedStyle(document.documentElement)
      .getPropertyValue(varName)
      .trim();
  });

  // 应用CSS变量
  function applyStyles() {
    const style =
      document.getElementById(STYLE_ID) ||
      document.createElement("style");
    style.id = STYLE_ID;

    const savedStyles = safeJsonParse(
      localStorage.getItem("docsify-css-vars"),
      {}
    );

    const mergedStyles = { ...defaultValues, ...savedStyles };

    style.textContent = `:root {${Object.entries(mergedStyles)
      .map(([key, value]) => `${key}: ${value};`)
      .join("")}}`;

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
      const div = document.createElement('div');
      div.style.color = value;
      document.body.appendChild(div);
      const color = getComputedStyle(div).color;
      document.body.removeChild(div);
      return color || value;
    },

    luminance(color) {
      // 新增防御性检查
      if (!color || typeof color !== 'string') return 0;

      const rgbValues = this.extractRGB(color);
      if (!rgbValues) return 0;

      const [r, g, b] = rgbValues.map(v => {
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
        const hex = color.replace('#', '');
        const fullHex = hex.length === 3
          ? hex.split('').map(c => c + c).join('')
          : hex;
        const result = hexRegex.exec(fullHex);
        return result
          ? [parseInt(result[1], 16), parseInt(result[2], 16), parseInt(result[3], 16)]
          : null;
      }

      return null;
    },

    contrast(color1, color2 = '#ffffff') {
      try {
        const lum1 = this.luminance(color1) + 0.05;
        const lum2 = this.luminance(color2) + 0.05;
        return (Math.max(lum1, lum2) / Math.min(lum1, lum2)).toFixed(2);
      } catch {
        return 0;
      }
    }
  };

  // 创建控制面板
  function createControlPanel() {
    // 在控制面板创建部分修改如下
    const panelHTML = `
    <div class="modal-mask">
      <div id="css-var-control-panel" class="modal-wrapper">
        <div class="modal-container">
          <div class="modal-header">
              <div class="header-left">
                <h3>CSS变量配置</h3>
                <div class="search-box">
                  <input type="text" placeholder="搜索变量..." class="search-input">
                   <div class="search-history"></div>
                  <svg class="search-icon" viewBox="0 0 24 24" width="18">
                    <path d="M15.5 14h-.79l-.28-.27A6.47 6.47 0 0 0 16 9.5 6.5 6.5 0 1 0 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"/>
                  </svg>
                </div>
              </div>
            <div>
              <button class="fa fa-trash-can clear-all"> 清除所有</button>
              <button class="fa fa-circle-xmark close-btn"></button>
            </div>
          </div>
          <div class="modal-body">
            <div class="table-wrapper"></div>
          </div>
          <div class="modal-footer">
            <button class="fa fa-plus add-row">  新增变量</button>
          </div>
        </div>
      </div>
    </div>
    `;

    // 在样式部分添加以下优化代码
    const style = document.createElement("style");
    style.textContent = `
        .control-btn {
          position: fixed;
          bottom: 20px;
          right: 20px;
          padding: 10px 20px;
          background: var(--theme-color);
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
          z-index: 9999;
        }

        /* 使用固定单位避免受CSS变量影响 */
        #css-var-control-panel {
          font-size: 14px !important; /* 固定字体大小 */
          --local-font-size: 14px;    /* 定义局部变量 */
        }

        /* 尺寸单位全部使用px */
        .modal-container {
          min-width: 800px !important;
          max-width: 90vw !important;
        }

        .modal-header h3 {
          font-size: 18px !important;
          margin: 0;
        }

        /* 表格单元格使用固定单位 */
        #css-var-control-panel td {
          padding: 10px 15px !important;
          font-size: var(--local-font-size);
        }

        /* 输入框固定尺寸 */
        #css-var-control-panel input {
          font-size: 14px !important;
          height: 32px !important;
        }

        /* 新增遮罩层 */
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

        /* 模态框容器 */
        .modal-wrapper {
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 100%;
          padding: 20px;
        }

        /* 模态框主体 */
        .modal-container {
          background: #fff;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.33);
          width: 100%;
          max-width: 1600px;
          max-height: 90vh;
          display: flex;
          flex-direction: column;
        }

        /* 头部样式 */
        .modal-header {
          padding: 20px;
          border-bottom: 1px solid #e8e8e8;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }

        /* 内容区域 */
        .modal-body {
          padding: 20px;
          overflow: auto;
          flex: 1;
        }

        /* 底部区域 */
        .modal-footer {
          padding: 15px 20px;
          border-top: 1px solid #e8e8e8;
          text-align: right;
        }

        /* 关闭按钮样式 */
        .close-btn {
          font-size: 14px;
          line-height: 1;
          margin-left: 15px;
        }          

        /* 表格容器 */
        #css-var-control-panel .table-wrapper {
          overflow: auto;
          max-height: 50vh;
          border: 1px solid #e8e8e8;
          border-radius: 4px;
          margin: 12px 0;
        }

        /* 表格主体 */
        #css-var-control-panel table {
          width: 100%;
          min-width: 600px;
          border-collapse: separate;
          border-spacing: 0;
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
        }

        /* 表头样式 */
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

        /* 表格单元格 */
        #css-var-control-panel td {
          padding: 10px 15px;
          border-bottom: 1px solid #e9ecef;
          background: #fff;
          vertical-align: middle;
        }

        /* 斑马纹效果 */
        #css-var-control-panel tbody tr:nth-child(even) td {
          background-color: #f8f9fa;
        }

        /* 悬停效果 */
        #css-var-control-panel tbody tr:hover td {
          background-color: #e9f5ff;
          transition: background 0.2s ease;
        }

        /* 操作按钮容器 */
        #css-var-control-panel td:last-child {
          white-space: nowrap;
        }

        /* 按钮样式 */
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

        /* 输入框增强样式 */
        #css-var-control-panel input {
          border: 1px solid #ced4da;
          border-radius: 4px;
          padding: 6px 8px;
          transition: border 0.2s;
        }

        #css-var-control-panel input:focus {
          border-color: #4dabf7;
          outline: none;
          box-shadow: 0 0 0 2px rgba(77, 171, 247, 0.2);
        }

        /* 颜色选择器特殊样式 */
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
        #css-var-control-panel   .color-preview {
          display: inline-block;
          width: 20px;
          height: 20px;
          border: 1px solid #ddd;
          border-radius: 4px;
          margin-left: 8px;
          vertical-align: middle;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        #css-var-control-panel  .color-input-group {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        #css-var-control-panel  .color-input {
          width: 120px !important;
        }
        
        #css-var-control-panel  .contrast-warning {
          color: #ff4444;
          cursor: help;
          position: relative;
        }
        
        #css-var-control-panel  .contrast-warning:hover::after {
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

        #css-var-control-panel  .color-display {
          display: inline-flex;
          align-items: center;
          gap: 8px;
        }
        
        #css-var-control-panel  .color-value {
          font-family: monospace;
          font-size: 0.9em;
        }
        
        #css-var-control-panel  .color-preview {
          width: 20px;
          height: 20px;
          border: 1px solid #ddd;
          border-radius: 4px;
          flex-shrink: 0;
        }
        
        /* 保持输入组件样式一致 */
        #css-var-control-panel  .color-input-group {
          display: flex;
          align-items: center;
          gap: 8px;
        }
              
        /* 响应式处理 */
        @media (max-width: 768px) {
          #css-var-control-panel {
            width: 95%;
            padding: 15px;
          }
          
          #css-var-control-panel .table-wrapper {
            max-height: 60vh;
          }
          
          #css-var-control-panel button {
            padding: 4px 8px;
            font-size: 0.9em;
          }
        }

`;
    document.head.appendChild(style);

    const btn = document.createElement("button");
    btn.className = "control-btn";
    btn.textContent = "高级样式调整（操作有风险，保存需谨慎）";

    const panel = document.createElement("div");
    panel.innerHTML = panelHTML;

    // 输入类型检测函数
    function detectInputType(value) {
      return "text";
    }

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
    panel.querySelector(".search-input").addEventListener("keydown", (e) => {
      const items = panel.querySelectorAll(".history-item");
      let current = [...items].findIndex((item) =>
        item.classList.contains("selected")
      );

      if (e.key === "ArrowDown") {
        current = Math.min(current + 1, items.length - 1);
      } else if (e.key === "ArrowUp") {
        current = Math.max(current - 1, -1);
      } else if (e.key === "Enter" && current >= 0) {
        panel.querySelector(".search-input").value = items[current].textContent;
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
          .filter(varName => savedKeys.includes(varName)), // 已保存
        defaultVars.filter(varName => !savedKeys.includes(varName)) // 未保存
      ];

      // 合并并去重（保持顺序）
      return [...new Set([...savedVariables, ...unsavedVariables])];

    }

    // 颜色值检测函数
    function isColor(value) {
      const colorRegex = /^(#([0-9a-f]{3}){1,2}|rgb\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\)|rgba\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*[\d.]+\s*\)|hsl\(\s*\d+\s*,\s*[\d.]+%,\s*[\d.]+%\)|hsla\(\s*\d+\s*,\s*[\d.]+%,\s*[\d.]+%,\s*[\d.]+\))$/i;
      return colorRegex.test(value.trim());
    }

    // 生成颜色预览块
    function createColorPreview(colorValue) {
      return isColor(colorValue)
        ? `<span class="color-preview" style="background:${colorValue}"></span>`
        : '';
    }

    function renderTable() {
      const savedVars = safeJsonParse(localStorage.getItem("docsify-css-vars"), {});
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
          ${currentVars.map((varName) => {
        const currentValue = getComputedStyle(document.documentElement).getPropertyValue(varName).trim();
        const defaultValue = defaultValues[varName] || currentValue;
        const savedValue = savedVars[varName] || "";

        // 标准化颜色
        const normalizedCurrent = ColorUtils.normalize(currentValue);
        const normalizedDefault = ColorUtils.normalize(defaultValue);
        const normalizedSaved = ColorUtils.normalize(savedValue);

        // 判断是否为颜色值
        const isColorCurrent = isColor(normalizedCurrent);
        const isColorDefault = isColor(normalizedDefault);
        const isColorSaved = isColor(normalizedSaved);

        // 对比度警告
        const showWarning = isColorSaved &&
          ColorUtils.contrast(normalizedSaved) < 4.5;

        return `
              <tr>
                <td>${varName}</td>
                <td>
                  ${isColorCurrent ? colorPreview(normalizedCurrent) : currentValue}
                </td>
                <td>
                  ${isColorDefault ? colorPreview(normalizedDefault) : defaultValue}
                </td>
                <td>
                  <div class="color-input-group">
                    ${isColorSaved ? `
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
                    ` : `
                      <input
                        type="text"
                        value="${savedValue}"
                        data-var="${varName}"
                      >
                    `}
                    ${showWarning ? '<span class="contrast-warning">⚠</span>' : ''}
                  </div>
                </td>
                <td>
                  <button class = "fa fa-floppy-disk" data-action="save" data-var="${varName}"> 保存</button>
                  <button class = "fa fa-repeat" data-action="reset" data-var="${varName}"> 重置</button>
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
      panel.querySelectorAll('.color-input-group').forEach(group => {
        const colorPicker = group.querySelector('input[type="color"]');
        const textInput = group.querySelector('input[type="text"]');

        if (colorPicker && textInput) {
          const syncValues = (source, target) => {
            target.value = source.value;
            const varName = source.dataset.var;
            // document.documentElement.style.setProperty(varName, source.value);
          };

          colorPicker.addEventListener('input', () => syncValues(colorPicker, textInput));
          textInput.addEventListener('input', () => syncValues(textInput, colorPicker));
        }
      });

      // 添加数据属性保存原始值
      currentVars.forEach((varName) => {
        const row = panel.querySelector(`[data-var="${varName}"]`).closest("tr");
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
      return `
      <span style="display:inline-flex;align-items:center;gap:4px">
        ${color}
        <span class="color-preview" style="background:${color}"></span>
      </span>
    `;
    }

    // 事件绑定
    panel.querySelector(".clear-all").addEventListener("click", () => {
      localStorage.removeItem("docsify-css-vars");
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

    panel.querySelector(".search-history").addEventListener("click", (e) => {
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
      const newVarName = prompt("请输入新的CSS变量名（例如：--new-variable）");
      if (newVarName && newVarName.startsWith("--")) {
        config.customVars.push(newVarName);
        defaultValues[newVarName] = getComputedStyle(document.documentElement)
          .getPropertyValue(newVarName)
          .trim();
        renderTable();
      }
    });

    panel.addEventListener("click", (e) => {
      if (e.target.dataset.action === "save") {
        const varName = e.target.dataset.var;
        const newValue = e.target.closest("tr").querySelector("input").value;
        const saved = JSON.parse(
          localStorage.getItem("docsify-css-vars") || "{}"
        );
        saved[varName] = newValue;
        localStorage.setItem("docsify-css-vars", JSON.stringify(saved));
        applyStyles();
        renderTable();
      }

      if (e.target.dataset.action === "reset") {
        const varName = e.target.dataset.var;
        const saved = JSON.parse(
          localStorage.getItem("docsify-css-vars") || "{}"
        );
        delete saved[varName];
        localStorage.setItem("docsify-css-vars", JSON.stringify(saved));
        applyStyles();
        renderTable();
      }
    });

    function displayTable() {
      document.querySelector(".modal-mask").style.display = "flex";
      renderTable();
    }

    btn.addEventListener("click", () => {
      displayTable();
    });

    document.body.appendChild(btn);
    document.body.appendChild(panel);
  }

  hook.mounted(function () {
    init()
  });
}, window.$docsify.plugins);
