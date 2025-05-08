window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (
  hook,
  vm
) {
  hook.doneEach(function () {

    const class_config = ['match-braces']; // 改为 const 防止修改

    document.querySelectorAll('pre > code').forEach(codeEl => {
      const preEl = codeEl.parentNode;
      
      class_config.forEach(class_name => {
        // 仅当类名不存在时添加
        if (!codeEl.classList.contains(class_name)) {
          codeEl.classList.add(class_name);
        }
        if (!preEl.classList.contains(class_name)) {
          preEl.classList.add(class_name);
        }
      });
    });

    if (window.Prism) {
      Prism.highlightAll();
    }
  });
});