window.$docsify = window.$docsify || {};
window.$docsify.plugins = [].concat(function(hook, vm) {
  // 在每次路由切换完成后执行
  hook.doneEach(function() {
    // 给每个有子列表的 li 添加折叠功能
    const collapseLi = () => {
      document.querySelectorAll('.content li').forEach(li => {
        // 如果已经处理过则跳过
        if (li.classList.contains('collapsible')) return;
        
        // 检查是否包含子列表
        const childList = li.querySelector('ul, ol');
        if (childList) {
          // 添加可折叠标识
          li.classList.add('collapsible');
          
          // 默认折叠子列表
          childList.style.display = 'none';
          
          // 创建折叠图标
          const toggle = document.createElement('span');
          toggle.className = 'collapse-toggle';
          toggle.innerHTML = '▶';
          toggle.style.cssText = `
            cursor: pointer;
            margin-right: 6px;
            user-select: none;
          `;
          
          // 插入图标
          li.insertBefore(toggle, li.firstChild);
        }
      });

      // 添加点击事件
      document.querySelectorAll('.collapsible').forEach(li => {
        li.addEventListener('click', function(e) {
          // 防止点击链接时触发
          if (e.target.tagName === 'A') return;
          
          const toggle = this.querySelector('.collapse-toggle');
          const childList = this.querySelector('ul, ol');
          
          if (childList.style.display === 'none') {
            childList.style.display = 'block';
            toggle.innerHTML = '▼';
          } else {
            childList.style.display = 'none';
            toggle.innerHTML = '▶';
          }
        });
      });
    };

    // 使用微任务等待 DOM 更新
    setTimeout(collapseLi, 0);
  });
}, window.$docsify.plugins);