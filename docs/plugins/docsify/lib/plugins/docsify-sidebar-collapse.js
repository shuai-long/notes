window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  hook.doneEach(function () {
    // 添加样式
    const style = document.createElement('style');
    style.textContent = `
      .sidebar-nav li.arrow-folder::before {
        font-family: 'FontAwesome';
        content: '\\f07b';
        display: inline-block;
      }
      .sidebar-nav li.arrow-folder.expanded::before {
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

    document.querySelectorAll('.sidebar-nav li').forEach(li => {
      if (li.classList.contains('has-arrow')) return;

      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink) return;
        
      li.classList.add('arrow-folder');

      li.classList.add('arrow', 'has-arrow');

      li.classList.toggle('expanded', false);

      li.addEventListener('click', function (e) {

        li.classList.toggle('expanded');

        e.stopPropagation();
        e.preventDefault();
      });


    });
  });
});