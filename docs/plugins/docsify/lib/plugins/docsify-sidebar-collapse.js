window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  hook.doneEach(function () {
    // 添加样式
    const style = document.createElement('style');
    style.textContent = `
      .sidebar-nav li.arrow::before {
        font-family: 'FontAwesome';
        content: '\\f07b';
        display: inline-block;
      }
      .sidebar-nav li.arrow.folder-expanded::before {
        content: '\\f07c';
      }
      .sidebar-nav li.arrow > ul {
        display: none;
      }
      .sidebar-nav li.arrow.folder-expanded > ul {
        display: block;
      }
    `;
    document.head.appendChild(style);

    const sidebarState = JSON.parse(localStorage.getItem('sidebarExpandedState')) || {};

    // 替代方案：使用相对位置路径作为唯一标识
    function generateElementPath(element) {
      const path = [];
      let currentElement = element;

      while (currentElement.parentNode) {
        const parent = currentElement.parentNode;
        const index = Array.prototype.indexOf.call(parent.children, currentElement);
        path.unshift(index);
        currentElement = parent;
      }

      return path.join('-');
    }

    // 展开当前地址对应的目录
    function expandCurrentPath() {
      const currentHash = window.location.hash;
      if (!currentHash) return;

      const sidebarState = JSON.parse(localStorage.getItem('sidebarExpandedState')) || {};

      document.querySelectorAll('.sidebar-nav a[href]').forEach(a => {
        const aUrl = new URL(a.href, window.location.href);
        if (aUrl.hash === currentHash) {
          let parentLi = a.closest('li');

          // 展开所有上级目录
          while (parentLi) {
            const elementId = parentLi.dataset.sidebarId;
            if (elementId) {
              parentLi.classList.add('folder-expanded');
              sidebarState[elementId] = true;
            }
            parentLi = parentLi.parentElement.closest('li');
          }
        }
      });

      localStorage.setItem('sidebarExpandedState', JSON.stringify(sidebarState));
    }


    document.querySelectorAll('.sidebar-nav li').forEach(li => {
      if (li.classList.contains('has-arrow')) return;

      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink) return;

      li.classList.add('arrow', 'has-arrow');
      // 使用方式：替换原来的elementId生成方式
      const elementId = `sidebar-path-${generateElementPath(li)}`;
      li.dataset.sidebarId = elementId; // 存储标识符到DOM

      // 应用存储状态或默认值
      const storedState = sidebarState[elementId];
      li.classList.toggle('folder-expanded', storedState || false);

      li.addEventListener('click', function (e) {
        // 判断是否为直接点击 li 元素（而非子元素冒泡）
        if (e.target === this) {

          // 切换展开状态
          const isExpanded = this.classList.toggle('folder-expanded');

          // 更新存储状态
          const updatedState = {
            ...JSON.parse(localStorage.getItem('sidebarExpandedState') || '{}'),
            [elementId]: isExpanded
          };
          localStorage.setItem('sidebarExpandedState', JSON.stringify(updatedState));

          e.stopPropagation();
          e.preventDefault();
        }
      });
    });

    // 监听地址变化
  window.addEventListener('hashchange', expandCurrentPath);
  window.addEventListener('popstate', expandCurrentPath);

  });
});