(window.$docsify = window.$docsify || {}),
  (window.$docsify.plugins = [].concat(function (hook, vm) {
    hook.doneEach(function () {
      // 中英文添加空格
      // pangu.spacingElementById('main');
      // pangu.spacingElementByClassName('comment');
      // pangu.spacingElementByTagName('p');

      // pangu.spacingElementByTagName('aside');
      // pangu.spacingElementByTagName('h2');
      // pangu.spacingElementByTagName('h3');
      // pangu.spacingElementByTagName('h4');
      // pangu.spacingElementByTagName('h5');
      // pangu.spacingElementByTagName('h6');
      // pangu.spacingElementByTagName('p');
      // pangu.spacingElementByTagName('button');
      pangu.autoSpacingPage();

    });
  }, window.$docsify.plugins));
