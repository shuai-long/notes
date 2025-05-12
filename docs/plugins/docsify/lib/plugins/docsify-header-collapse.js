!(function () {
  if (typeof window.$docsify === 'undefined') return

  window.$docsify.plugins.push(function (hook) {

    // 初始化计数器数组
    let counters = [0, 0, 0, 0, 0]

    const getPageKey = () => {
      return decodeURIComponent(window.location.hash.replace(/^#!?/, '')).split('?')[0]
    }

    hook.doneEach(function () {

      // 重置计数器
      counters = [0, 0, 0, 0, 0]
      const pageKey = getPageKey()

      document.querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        const tagLevel = parseInt(header.tagName.substring(1))

        // 仅处理h2-h6
        if (tagLevel < 2) {
          // 移除h1可能存在的序号
          const existingNumber = header.querySelector('.header-number')
          if (existingNumber) existingNumber.remove()
          return
        }

        // 转换为数组索引（h2=0, h3=1...）
        const level = tagLevel - 2

        // 更新计数器逻辑
        counters[level]++
        // 重置子级计数器
        for (let i = level + 1; i < 5; i++) counters[i] = 0

        // 生成序号（从h2开始）
        const sectionNumber = counters.slice(0, level + 1).join('.')
        header.dataset.sectionKey = `${pageKey}|${sectionNumber}`

        // 创建/更新序号元素
        let numberSpan = header.querySelector('.header-number')
        if (!numberSpan) {
          numberSpan = document.createElement('span')
          numberSpan.className = 'header-number'
          numberSpan.style.color ='#b0abab'
          // 插入到折叠按钮之后或标题开头
          header.insertBefore(numberSpan, header.children[1] || header.firstChild)
        }
        numberSpan.textContent = `${sectionNumber} `

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

        // 恢复保存状态
        const storageKey = header.dataset.sectionKey
        const savedState = localStorage.getItem(storageKey)
        if (savedState === 'collapsed') {
          content.style.display = 'none'
        } else {
          content.style.display = 'block'
        }

        // 绑定点击事件
        header.addEventListener('click', function () {
          const isCollapsed = content.style.display === 'none'
          content.style.display = isCollapsed ? 'block' : 'none'
          localStorage.setItem(storageKey, isCollapsed ? 'expanded' : 'collapsed')
        })

        header.classList.add('collapsible')
      })
    })
  })

  // 优化后的样式
  const style = document.createElement('style')
  style.textContent = `
    .header-number {
      color: #b0abab;
    }
  `
  document.head.appendChild(style)
})()