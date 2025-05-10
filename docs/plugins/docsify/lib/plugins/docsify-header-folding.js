!(function () {
  if (typeof window.$docsify === 'undefined') return

  window.$docsify.plugins.push(function (hook) {
    hook.doneEach(function () {
      document.querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        if (header.classList.contains('collapsible')) return

        // 创建折叠按钮
        const toggle = document.createElement('span')
        toggle.className = 'collapse-toggle'
        toggle.innerHTML = '▼'
        header.insertBefore(toggle, header.firstChild)
        
        header.classList.add('collapsible')

        // 创建内容容器
        const content = document.createElement('div')
        content.className = 'collapsible-content'
        
        // 先收集需要移动的节点
        const nodesToMove = []
        let nextElem = header.nextElementSibling
        while (nextElem && !nextElem.matches('h1, h2, h3, h4, h5, h6')) {
          nodesToMove.push(nextElem)
          nextElem = nextElem.nextElementSibling
        }

        // 插入容器并移动节点
        header.parentNode.insertBefore(content, header.nextSibling)
        nodesToMove.forEach(node => content.appendChild(node))

        // 绑定点击事件
        toggle.addEventListener('click', function () {
          const isCollapsed = content.style.display === 'none'
          content.style.display = isCollapsed ? 'block' : 'none'
          toggle.innerHTML = isCollapsed ? '▼' : '▶'
        })
      })
    })
  })

  // 优化后的样式
  const style = document.createElement('style')
  style.textContent = `
    .collapse-toggle {
      cursor: pointer;
      margin-right: 8px;
      transition: transform 0.2s;
      display: inline-block;
    }
    .collapsible-content {
      overflow: hidden;
      transition: height 0.3s ease-out;
    }
    .collapsible-content[style*="none"] {
      height: 0 !important;
      display: block !important;
    }
  `
  document.head.appendChild(style)
})()