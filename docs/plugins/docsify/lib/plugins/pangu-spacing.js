// (window.$docsify = window.$docsify || {}),
//   (window.$docsify.plugins = [].concat(function (hook, vm) {
//     hook.ready(function () {
//       // 中英文添加空格
//       // pangu.spacingElementById('main');
//       // pangu.spacingElementByClassName('comment');
//       // pangu.spacingElementByTagName('p');

//       // pangu.spacingElementByTagName('aside');
//       // pangu.spacingElementByTagName('h2');
//       // pangu.spacingElementByTagName('h3');
//       // pangu.spacingElementByTagName('h4');
//       // pangu.spacingElementByTagName('h5');
//       // pangu.spacingElementByTagName('h6');
//       // pangu.spacingElementByTagName('p');
//       // pangu.spacingElementByTagName('button');
//       // pangu.autoSpacingPage();

//       let is_spacing = false;

//       is_spacing = true;
//       pangu.spacingPage();
//       is_spacing = false;

//       document.addEventListener('DOMContentLoaded', () => {
//         if (!is_spacing) {
//           is_spacing = true;
//           pangu.spacingNode(e.target);
//           is_spacing = false;
//         }
//       });

//     });
//   }, window.$docsify.plugins));


(window.$docsify = window.$docsify || {}),
  (window.$docsify.plugins = [].concat(function (hook, vm) {
    let is_spacing = false;

    //break
    hook.doneEach(function () {
      if (!is_spacing) {
        is_spacing = true;
        pangu.spacingPage();
        is_spacing = false;
      }
    });

  }, window.$docsify.plugins));