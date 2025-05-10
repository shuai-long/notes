window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function(hook) {
  hook.doneEach(function() {
    // 为所有.content下的ul添加折叠功能
    document.querySelectorAll('.content ul').forEach(ul => {
      // 跳过已添加toggle的列表
      if (ul.previousElementSibling?.classList.contains('ul-toggle')) return;

      // 创建折叠开关
      const toggle = document.createElement('span');
      toggle.className = 'ul-toggle';
      toggle.innerHTML = '▸';
      
      // 点击切换状态
      toggle.addEventListener('click', function() {
        const isCollapsed = ul.style.display === 'none';
        ul.style.display = isCollapsed ? '' : 'none';
        toggle.innerHTML = isCollapsed ? '▸' : '▾';
      });

      // 初始状态为展开，添加开关
      ul.parentNode.insertBefore(toggle, ul);
    });
  });
});
