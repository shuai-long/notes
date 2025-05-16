!(function () {
  if (typeof window.$docsify === 'undefined') return;

  // 合并后的插件配置
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
    // ============================== 内容区域折叠逻辑 ==============================
    hook.doneEach(function () {
      // 初始化计数器数组
      let counters = [0, 0, 0, 0, 0];
      
      // 生成页面唯一标识
      const getPageKey = () => decodeURIComponent(window.location.hash.replace(/^#!?/, '')).split('?')[0];

      // 重置计数器并获取页面标识
      counters = [0, 0, 0, 0, 0];
      const pageKey = getPageKey();
      
      // 初始化折叠状态存储
      let collapsibleStates = JSON.parse(localStorage.getItem('headingExpandedState') || '{}');

      // 遍历所有标题元素
      document.querySelector('.content')?.querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        const tagLevel = parseInt(header.tagName.substring(1));
        if (tagLevel < 2) return;

        // 更新计数器
        const level = tagLevel - 2;
        counters[level]++;
        for (let i = level + 1; i < 5; i++) counters[i] = 0;

        // 创建序号元素
        const sectionNumber = counters.slice(0, level + 1).join('.');
        const storageKey = `${pageKey}|${sectionNumber}`;
        header.dataset.sectionKey = storageKey;

        // 创建折叠容器
        const content = document.createElement('div');
        content.className = 'collapsible-content';
        
        // 移动后续元素到折叠容器
        const nodesToMove = [];
        let nextElem = header.nextElementSibling;
        while (nextElem && !nextElem.matches('h1, h2, h3, h4, h5, h6')) {
          nodesToMove.push(nextElem);
          nextElem = nextElem.nextElementSibling;
        }
        nodesToMove.forEach(node => content.appendChild(node));
        header.after(content);

        // 设置初始状态
        content.style.display = collapsibleStates[storageKey] === 'collapsed' ? 'none' : 'block';
        
        // 添加点击交互
        header.classList.add('collapsible');
        header.addEventListener('click', () => {
          const isCollapsed = content.style.display === 'none';
          content.style.display = isCollapsed ? 'block' : 'none';
          collapsibleStates = {...collapsibleStates, [storageKey]: isCollapsed ? 'expanded' : 'collapsed'};
          localStorage.setItem('headingExpandedState', JSON.stringify(collapsibleStates));
        });
      });
    });

    // ============================== 侧边栏折叠逻辑 ==============================
    hook.doneEach(function () {
      // 添加组合样式
      const style = document.createElement('style');
      style.textContent = `
        /* 内容区域样式 */
        .collapsible { cursor: pointer; }
        .collapsible-content { margin-left: 15px; transition: height 0.3s; }
        .header-number { color: #b0abab !important; margin-right: 8px; }
        
        /* 侧边栏样式 */
        .sidebar-nav li.arrow::before {
          font-family: 'FontAwesome';
          content: '\\f07b';
          display: inline-block;
          margin-right: 8px;
        }
        .sidebar-nav li.arrow.folder-expanded::before { content: '\\f07c'; }
        .sidebar-nav li.arrow > ul { display: none; }
        .sidebar-nav li.arrow.folder-expanded > ul { display: block; }
      `;
      document.head.appendChild(style);

      // 侧边栏状态管理
      const sidebarState = JSON.parse(localStorage.getItem('sidebarExpandedState') ) || {};

      // 生成元素路径标识
      const generateElementPath = element => {
        const path = [];
        let current = element;
        while (current.parentNode) {
          const parent = current.parentNode;
          path.unshift(Array.from(parent.children).indexOf(current));
          current = parent;
        }
        return path.join('-');
      };

      // 自动展开当前路径
      const expandCurrentPath = () => {
        const currentHash = window.location.hash;
        document.querySelectorAll('.sidebar-nav a[href]').forEach(a => {
          if (new URL(a.href).hash === currentHash) {
            let parentLi = a.closest('li');
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
      };

      // 初始化侧边栏交互
      document.querySelectorAll('.sidebar-nav li').forEach(li => {
        if (li.classList.contains('has-arrow')) return;
        if (Array.from(li.children).some(c => c.tagName === 'A')) return;

        li.classList.add('arrow', 'has-arrow');
        const elementId = `sidebar-${generateElementPath(li)}`;
        li.dataset.sidebarId = elementId;
        
        // 应用存储状态
        li.classList.toggle('folder-expanded', sidebarState[elementId]);
        
        // 添加点击事件
        li.addEventListener('click', function(e) {
          if (e.target === this) {
            const isExpanded = this.classList.toggle('folder-expanded');
            localStorage.setItem('sidebarExpandedState', JSON.stringify({
              ...sidebarState,
              [elementId]: isExpanded
            }));
            e.stopPropagation();
            e.preventDefault();
          }
        });
      });

      // 绑定路由事件
      if (!window._sidebarEventsBound) {
        window.addEventListener('hashchange', expandCurrentPath);
        window.addEventListener('popstate', expandCurrentPath);
        window._sidebarEventsBound = true;
      }
      expandCurrentPath();
    });
  });
})();