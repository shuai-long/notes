window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function(hook, vm) {
  hook.doneEach(function() {
    // 添加样式
    const style = document.createElement('style');
    style.textContent = `
      .sidebar-nav li.arrow::before {
        content: '▶';
        display: inline-block;
        margin-right: 6px;
        font-size: 10px;
        transition: transform 0.2s;
      }
      .sidebar-nav li.arrow.expanded::before {
        transform: rotate(90deg);
      }
      .sidebar-nav li.arrow ul {
        display: none;
        margin-left: 10px;
      }
      .sidebar-nav li.arrow.expanded ul {
        display: block;
      }
    `;
    document.head.appendChild(style);

    // 处理符合条件的li元素
    document.querySelectorAll('.sidebar-nav li').forEach(li => {
       // 修改点：使用 > 选择器判断直接子元素是否包含a标签
      const hasDirectLink = li.querySelector(':scope > a'); // 关键修改
      
      // 跳过已有直接子元素a标签或已处理的元素
      if (hasDirectLink || li.classList.contains('has-arrow')) return;
      
      // 添加标记类名和箭头
      li.classList.add('arrow', 'has-arrow');
      
      // 初始化折叠状态
      if (li.querySelector('ul')) {
        li.classList.add('expanded'); // 默认展开，改为remove('expanded')则默认折叠
      }

      // 添加点击事件
      li.addEventListener('click', function(e) {
        // 跳过包含a标签的点击
        if (e.target.tagName === 'A') return;
        
        // 切换展开状态
        this.classList.toggle('expanded');
        
        // 阻止事件冒泡到父li
        e.stopPropagation();
      });
    });
  });
});