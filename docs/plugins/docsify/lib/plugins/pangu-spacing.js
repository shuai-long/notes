(window.$docsify = window.$docsify || {}),
  (window.$docsify.plugins = [].concat(function (hook, vm) {
    let is_spacing = false;

    //break
    hook.doneEach(function () {
      if (!is_spacing) {
        is_spacing = true;
        const start = Date.now();
        pangu.spacingPage();
        console.log(`排版耗时：${Date.now() - start}ms`);
        is_spacing = false;
      }
    });

  }, window.$docsify.plugins));