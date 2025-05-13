window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function(hook, vm) {
  hook.doneEach(function() {
    // 只处理包含子列表且没有链接的li项
    const processLiElements = () => {
      document.querySelectorAll('.sidebar-nav li:not(.has-link)').forEach(li => {
        // 跳过已经处理过的元素
        if (li.dataset.processed) return;
        
        // 检查是否包含子列表
        const childList = li.querySelector('ul, ol');
        if (childList) {
          // 添加可交互样式
          li.style.cursor = 'pointer';
          li.classList.add('collapsible');
          
          // 添加箭头图标
          const toggle = document.createElement('span');
          toggle.className = 'collapse-toggle';
          toggle.innerHTML = '▶';
          li.insertBefore(toggle, li.firstChild);

          // 初始化状态
          let isCollapsed = false;
          const toggleState = () => {
            isCollapsed = !isCollapsed;
            childList.style.display = isCollapsed ? 'none' : 'block';
            toggle.style.transform = isCollapsed ? 'rotate(0deg)' : 'rotate(90deg)';
          };

          // 绑定点击事件
          li.addEventListener('click', (e) => {
            // 跳过链接的点击事件
            if (e.target.tagName === 'A') return;
            toggleState();
          });

          // 标记已处理
          li.dataset.processed = true;
        }
      });
    };

    // 初始处理
    processLiElements();

    // 添加CSS样式
    const style = document.createElement('style');
    style.textContent = `
      .collapsible .collapse-toggle {
        display: inline-block;
        margin-right: 6px;
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