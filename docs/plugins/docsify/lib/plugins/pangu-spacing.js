function appendScript() {
  const script = document.createElement('script');
  script.async = true;
  script.src = 'https://cdn.jsdelivr.net/npm/pangu';
  document.body.appendChild(script);
}

function panguPlugin(hook) {
  hook.init(() => {
    appendScript();
  });

  hook.doneEach(() => {

    if (window.pangu) {
      window.pangu.spacingElementByTagName('main');
    }
  });
}
window.$docsify = window.$docsify || {};
$docsify.plugins = [...($docsify.plugins || []), panguPlugin];