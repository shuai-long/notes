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
      if (li.classList.contains('has-arrow')) return;

      // 改进点：使用children遍历代替:scope选择器
      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink) return;

      // 添加功能标记
      li.classList.add('arrow', 'has-arrow');

      // 初始化展开状态
      const hasNestedList = li.querySelector(':scope > ul'); // 关键修改点
      li.classList.toggle('expanded', false);

      // 绑定点击事件（改进冒泡处理）
      li.addEventListener('click', function (e) {
        // 只切换当前层级的展开状态
        li.classList.toggle('expanded');

        // 阻止所有事件传播
        e.stopPropagation();
        e.preventDefault();
      });

    });
  });
});