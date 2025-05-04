; (function () {
  // Prism 插件名称到文件名的映射
  const pluginFiles = {
    'abap': {
      js: 'components/prism-abap.min.js'
    },
    'line-numbers': {
      js: 'plugins/line-numbers/prism-line-numbers.min.js',
      css: 'plugins/line-numbers/prism-line-numbers.min.css'
    },
    'toolbar': {
      js: 'plugins/toolbar/prism-toolbar.min.js',
      css: 'plugins/toolbar/prism-toolbar.css'
    },
    'copy-to-clipboard': {
      js: 'plugins/copy-to-clipboard/prism-copy-to-clipboard.min.js',
      css: null
    },
  };

  function loadCSS(href) {
    const link = document.createElement('link');
    link.rel = 'stylesheet';
    link.href = href;
    document.head.appendChild(link);
  }

  function loadJS(src, callback) {
    const script = document.createElement('script');
    script.src = src;
    // 禁用异步以保持加载顺序
    script.async = false;
    if (callback) {
      script.onload = callback;
    }
    document.head.appendChild(script);
  }

  function prismEnhancer(hook, vm) {
    // 默认配置
    const cfg = window.$docsify || {};
    const enabled = cfg.prismPlugins || [];
    const opts = cfg.prism || {};
    const showLangCfg = opts.showLanguage || {};

    // CDN 基础路径（可根据需要调整版本号）
    const PRISM_BASE_CSS = '//cdn.jsdelivr.net/npm/prismjs/themes/prism.css';
    const PRISM_BASE_JS = '//cdn.jsdelivr.net/npm/prismjs/prism.min.js';

    // 首先加载 Prism 核心（CSS & JS）
    loadCSS(PRISM_BASE_CSS);
    loadJS(PRISM_BASE_JS, function () {
      // 核心加载完成后，按需加载插件资源
      enabled.forEach(name => {
        const file = pluginFiles[name];
        if (!file) return;
        if (file.css) {
          loadCSS(`//cdn.jsdelivr.net/npm/prismjs/${file.css}`);
        }
        if (file.js) {
          loadJS(`//cdn.jsdelivr.net/npm/prismjs/${file.js}`);
        }
      });
    });

    // 钩子：每次渲染完成后处理代码块并高亮
    hook.doneEach(function () {
      // 处理所有 <pre><code> 元素
      document.querySelectorAll('pre > code').forEach(codeEl => {
        const preEl = codeEl.parentNode;
        const langClass = Array.from(codeEl.classList).find(cls => cls.startsWith('language-'));
        const lang = langClass ? langClass.replace('language-', '') : '';
        // 添加行号插件需要的类
        if (enabled.includes('line-numbers')) {
          preEl.classList.add('line-numbers');
        }
        // 设置语言标签（Show Language）
        if (enabled.includes('show-language')) {
          const useData = showLangCfg.useDataAttribute === true;
          if (useData && lang) {
            const label = (showLangCfg.mapping && showLangCfg.mapping[lang]) || lang;
            preEl.setAttribute('data-language', label);
          }
        }
        // 自定义复制按钮提示
        if (enabled.includes('copy-to-clipboard') && opts.copySuccessText) {
          // 使用 Prism Copy 插件的 data 属性
          codeEl.setAttribute('data-prismjs-copy-success', opts.copySuccessText);
        }
      });
      // 调用 Prism 进行高亮
      if (window.Prism) {
        Prism.highlightAll();
      }
    });
  }

  // 注册插件
  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(prismEnhancer);
})();
