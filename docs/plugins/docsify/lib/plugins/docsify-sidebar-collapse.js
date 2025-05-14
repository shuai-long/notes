window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function(hook, vm) {
  hook.doneEach(function() {
    const storageKey = 'docsify-collapse-state-v2';
    let stateStorage = JSON.parse(localStorage.getItem(storageKey) || '{}');

    const processLiElements = () => {
      document.querySelectorAll('.sidebar-nav li:not(.has-link)').forEach(li => {
        if (li.dataset.processed) return;

        const childList = li.querySelector('ul, ol');
        if (childList) {
          li.style.cursor = 'pointer';
          li.classList.add('collapsible');

          // 创建独立开关控件
          const toggle = document.createElement('span');
          toggle.className = 'collapse-toggle';
          toggle.innerHTML = '▶';
          li.insertBefore(toggle, li.firstChild);

          // 初始化状态
          const path = getItemPath(li);
          const storedState = stateStorage[path];
          const isCollapsed = storedState !== undefined ? storedState : false;
          
          // 设置初始状态
          childList.style.display = isCollapsed ? 'none' : 'block';
          toggle.style.transform = isCollapsed ? 'rotate(0deg)' : 'rotate(90deg)';

          // 点击处理函数
          const toggleState = (targetState) => {
            const newState = targetState !== undefined ? targetState : !isCollapsed;
            
            // 更新DOM状态
            childList.style.display = newState ? 'none' : 'block';
            toggle.style.transform = newState ? 'rotate(0deg)' : 'rotate(90deg)';
            
            // 更新存储状态
            stateStorage[path] = newState;
            localStorage.setItem(storageKey, JSON.stringify(stateStorage));
            
            // 更新内存状态
            isCollapsed = newState;
          };

          // 箭头点击事件
          toggle.addEventListener('click', (e) => {
            e.stopPropagation();
            toggleState();
          });

          // LI主体点击事件
          li.addEventListener('click', (e) => {
            if (!e.target.closest('ul, ol') && !e.target.closest('a')) {
              toggleState();
            }
          });

          li.dataset.processed = true;
        }
      });
    };

    // 生成唯一路径（改进版）
    const getItemPath = (element) => {
      const path = [];
      let el = element;
      while (el && !el.matches('.sidebar-nav')) {
        const parent = el.parentElement.closest('li, ul, ol');
        if (parent) {
          const index = Array.from(parent.children).indexOf(el.closest('li, ul, ol'));
          path.unshift(`${parent.tagName}-${index}`);
        }
        el = parent;
      }
      return path.join('|');
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