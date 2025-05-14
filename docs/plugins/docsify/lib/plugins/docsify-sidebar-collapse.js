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
        margin-right: 6px;
      }
      .sidebar-nav li.arrow.expanded::before {
        content: '\\f07c';
      }
      .sidebar-nav li.arrow > ul {
        display: none;
        margin-left: 10px;
      }
      .sidebar-nav li.arrow.expanded > ul {
        display: block;
      }
    `;
    document.head.appendChild(style);

    // 处理符合条件的li元素
    document.querySelectorAll('.sidebar-nav li').forEach(li => {
    // 改进点：使用children遍历代替:scope选择器
      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink || li.classList.contains('has-arrow')) return;

      // 添加功能标记
      li.classList.add('arrow', 'has-arrow');
      
      // 初始化展开状态
      const hasNestedList = li.querySelector(':scope > ul'); // 关键修改点
      li.classList.toggle('expanded', hasNestedList);

      // 绑定点击事件（改进冒泡处理）
      li.addEventListener('click', function(e) {
        // 排除点击子元素的情况
        if (e.target !== li && e.target.closest('a')) return;
        
        // 仅切换当前列表项
        this.classList.toggle('expanded');
        
        // 强制阻止所有冒泡
        e.stopImmediatePropagation();
      }, true); // 使用捕获阶段确保执行顺序
    });
  });
});