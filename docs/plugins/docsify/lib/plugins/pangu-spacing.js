!(function () {
  if (typeof window.$docsify === 'undefined') return

  window.$docsify.plugins.push(function (hook) {

    hook.doneEach(() => {
      if (window.pangu) {
        pangu.spacingElementByTagName('main');
      }
    });

  })
})()