
function panguPlugin(hook) {
 
  hook.doneEach(() => {
    if (window.pangu) {
      window.pangu.spacingElementByTagName('main');
    }
  });
}
window.$docsify = window.$docsify || {};
$docsify.plugins = [...($docsify.plugins || []), panguPlugin];