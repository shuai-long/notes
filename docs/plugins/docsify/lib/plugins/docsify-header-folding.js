// 折叠插件核心代码
!(function () {
  // 判断是否支持 docsify
  if (typeof window.$docsify === 'undefined') return
  
  // 注册插件
  window.$docsify.plugins.push(function (hook) {
    // 在每次路由切换完成后执行
    hook.doneEach(function () {
      // 遍历所有标题
      document.querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        // 跳过已处理的标题
        if (header.classList.contains('collapsible')) return
        
        // 添加折叠按钮
        const toggle = document.createElement('span')
        toggle.className = 'collapse-toggle'
        toggle.innerHTML = '▼'
        header.insertBefore(toggle, header.firstChild)
        
        // 标记为可折叠
        header.classList.add('collapsible')
        
        // 添加点击事件
        toggle.addEventListener('click', function () {
          // 切换折叠状态
          const isCollapsed = header.classList.toggle('collapsed')
          toggle.innerHTML = isCollapsed ? '▶' : '▼'
          
          // 查找并切换内容显示
          let content = getSectionContent(header)
          content.style.display = isCollapsed ? 'none' : 'block'
        })
      })
    })
    
    // 获取标题对应的内容区域
    function getSectionContent(header) {
      const wrapper = document.createElement('div')
      let currElem = header.nextElementSibling
      const tagName = header.tagName.toLowerCase()
      
      // 收集后续元素直到下个同级或更高级标题
      while (currElem && !currElem.matches('h1, h2, h3, h4, h5, h6')) {
        wrapper.appendChild(currElem.cloneNode(true))
        const next = currElem.nextElementSibling
        currElem.remove()
        currElem = next
      }
      
      // 插入包裹容器
      header.parentNode.insertBefore(wrapper, currElem)
      return wrapper
    }
  })
  
  // 添加基础样式
  const style = document.createElement('style')
  style.textContent = `
    .collapse-toggle {
      cursor: pointer;
      margin-right: 8px;
      transition: transform 0.3s;
      user-select: none;
    }
    .collapsed .collapse-toggle {
      transform: rotate(-90deg);
    }
  `
  document.head.appendChild(style)
})()