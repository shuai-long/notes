; (function () {

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
    const prismPlugins = cfg.prismPlugins || {};
    const enabled = prismPlugins.enabled || [];
    const pluginFiles = prismPlugins.pluginFiles || {};
    const opts = cfg.prism || {};
    const showLangCfg = opts.showLanguage || {};

    // CDN 基础路径（可根据需要调整版本号）
    const PRISM_BASE_CSS = '//cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism.css';
    const PRISM_BASE_JS = '//cdn.jsdelivr.net/npm/prismjs@1.29.0/prism.min.js';

    // 首先加载 Prism 核心（CSS & JS）
    loadCSS(PRISM_BASE_CSS);
    loadJS(PRISM_BASE_JS, function () {
      // 核心加载完成后，按需加载插件资源
      enabled.forEach(name => {
        const file = pluginFiles[name];
        if (!file) return;
        if (file.css) {
          loadCSS(`//cdn.jsdelivr.net/npm/prismjs@1.29.0/${file.css}`);
        }
        if (file.js) {
          loadJS(`//cdn.jsdelivr.net/npm/prismjs@1.29.0/${file.js}`);
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

        enabled.forEach(name => {
          const pluginLine = pluginFiles[name];
          if (!pluginLine) return;

          // 添加类名
          const classList = pluginLine.class_name || [];
          classList.forEach(class_name => {
            codeEl.classList.add(class_name);
            preEl.classList.add(class_name);
          })

          // 添加属性
          const attrList = pluginLine.attribute || [];
          attrList.forEach(attr_line => {
            if (attr_line.key) {
              if (attr_line.value) {
                // 如果 key 和 value 都不为空，添加属性 key 和 value
                preEl.setAttribute(attr_line.key, attr_line.value);
              } else {
                // 如果只有 key 不为空，添加属性 key（值设为空字符串）
                preEl.setAttribute(attr_line.key, '');
              }
            }
          });

          // 设置语言标签（Show Language）
          if (name === 'show-language') {
            const useData = showLangCfg.useDataAttribute === true;
            if (useData && lang) {
              const label = (showLangCfg.mapping && showLangCfg.mapping[lang]) || lang;
              preEl.setAttribute('data-language', label);
            }
          }
          // 自定义复制按钮提示
          if (name === 'copy-to-clipboard' && opts.copySuccessText) {
            // 使用 Prism Copy 插件的 data 属性
            codeEl.setAttribute('data-prismjs-copy-success', opts.copySuccessText);
          }
        });



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
