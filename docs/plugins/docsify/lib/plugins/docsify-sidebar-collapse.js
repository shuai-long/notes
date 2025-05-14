window.$docsify = window.$docsify || {};
window.$docsify.plugins = [].concat(function(hook) {
  hook.mounted(function() {
    const style = document.createElement('style');
    style.textContent = `
      .sidebar-nav li.arrow::before {
        content: '▶';
        font-size: 0.8em;
        margin-right: 5px;
        cursor: pointer;
      }
      .sidebar-nav li.arrow.open::before {
        content: '▼';
      }
    `;
    document.head.appendChild(style);

    function processSidebar() {
      document.querySelectorAll('.sidebar-nav li:not(:has(a))').forEach(li => {
        if (!li.classList.contains('has-arrow')) {
          li.classList.add('arrow');
          li.classList.add('has-arrow');
          
          const childUl = li.querySelector('ul');
          if (childUl) {
            childUl.style.display = 'none';
          }

          li.addEventListener('click', function(e) {
            if (e.target.tagName !== 'A') {
              this.classList.toggle('open');
              const ul = this.querySelector('ul');
              if (ul) {
                ul.style.display = ul.style.display === 'none' ? 'block' : 'none';
              }
            }
          });
        }
      });
    }

    // 初始化处理
    processSidebar();

    // 使用 MutationObserver 监听DOM变化
    const observer = new MutationObserver(mutations => {
      mutations.forEach(mutation => {
        if (mutation.addedNodes.length) {
          processSidebar();
        }
      });
    });

    observer.observe(document.querySelector('.sidebar-nav'), {
      childList: true,
      subtree: true
    });
  });
}, window.$docsify.plugins);