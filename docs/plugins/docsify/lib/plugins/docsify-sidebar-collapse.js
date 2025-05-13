window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function(hook, vm) {
  hook.doneEach(function() {
    const storageKey = 'docsify-collapse-state';
    const stateStorage = JSON.parse(localStorage.getItem(storageKey) || '{}');

    const processLiElements = () => {
      document.querySelectorAll('.sidebar-nav li:not(.has-link)').forEach(li => {
        if (li.dataset.processed) return;

        const childList = li.querySelector('ul, ol');
        if (childList) {
          li.style.cursor = 'pointer';
          li.classList.add('collapsible');

          // 添加独立操作的开关按钮
          const toggle = document.createElement('span');
          toggle.className = 'collapse-toggle';
          toggle.innerHTML = '▶';
          li.insertBefore(toggle, li.firstChild);

          // 从存储恢复状态
          const path = getItemPath(li);
          const isCollapsed = stateStorage[path] || false;
          childList.style.display = isCollapsed ? 'none' : 'block';
          toggle.style.transform = isCollapsed ? 'rotate(0deg)' : 'rotate(90deg)';

          // 独立点击区域处理
          const handler = (e) => {
            // 仅响应按钮或整个LI的点击
            if (e.target.closest('a')) return;
            
            // 阻止事件冒泡
            e.stopPropagation();
            
            const newState = !isCollapsed;
            childList.style.display = newState ? 'none' : 'block';
            toggle.style.transform = newState ? 'rotate(0deg)' : 'rotate(90deg)';
            
            // 更新存储状态
            stateStorage[path] = newState;
            localStorage.setItem(storageKey, JSON.stringify(stateStorage));
          };

          // 单独给开关按钮绑定事件
          toggle.addEventListener('click', handler);
          // 给LI绑定需要排除子列表区域的点击
          li.addEventListener('click', (e) => {
            if (!e.target.closest('ul, ol')) handler(e);
          });

          li.dataset.processed = true;
        }
      });
    };

    // 生成唯一路径标识
    const getItemPath = (element) => {
      const path = [];
      let el = element;
      while (el && !el.matches('.sidebar-nav')) {
        const parent = el.parentNode;
        const index = Array.from(parent.children).indexOf(el);
        path.unshift(index);
        el = parent.closest('li');
      }
      return path.join('-');
    };


    // 初始处理
    processLiElements();

    // 添加CSS样式
    const style = document.createElement('style');
    style.textContent = `
      .collapsible .collapse-toggle {
        display: inline-block;
        transition: transform 0.2s;
        font-size: 0.8em;
      }
      .collapsible ul,
      .collapsible ol {
        transition: all 0.2s;
      }
    `;
    document.head.appendChild(style);

    // 使用MutationObserver监听DOM变化
    const observer = new MutationObserver((mutations) => {
      processLiElements();
    });

    observer.observe(document.querySelector('.sidebar-nav'), {
      childList: true,
      subtree: true
    });
  });
});