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

      // 初始化折叠状态存储对象
      let collapsibleStates = JSON.parse(localStorage.getItem('headingExpandedState') || '{}');

      document.querySelector('.content').querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        const tagLevel = parseInt(header.tagName.substring(1))

        // 保持原有过滤逻辑
        if (tagLevel < 2) {
          const existingNumber = header.querySelector('.header-number')
          if (existingNumber) existingNumber.remove()
          return
        }

        // 保持原有计数器逻辑
        const level = tagLevel - 2
        counters[level]++
        for (let i = level + 1; i < 5; i++) counters[i] = 0

        // 生成唯一存储键
        const sectionNumber = counters.slice(0, level + 1).join('.')
        const storageKey = `${pageKey}|${sectionNumber}`
        header.dataset.sectionKey = storageKey

        // 创建序号元素（保持原有逻辑）
        let numberSpan = header.querySelector('.header-number')
        if (!numberSpan) {
          numberSpan = document.createElement('span')
          numberSpan.className = 'header-number'
          numberSpan.style.color = '#b0abab'
          header.insertBefore(numberSpan, header.children[1] || header.firstChild)
        }
        numberSpan.textContent = `${sectionNumber} `

        // 创建内容容器（保持原有逻辑）
        const content = document.createElement('div')
        content.className = 'collapsible-content'

        // 节点移动逻辑保持不变
        const nodesToMove = []
        let nextElem = header.nextElementSibling
        while (nextElem && !nextElem.matches('h1, h2, h3, h4, h5, h6')) {
          nodesToMove.push(nextElem)
          nextElem = nextElem.nextElementSibling
        }
        header.parentNode.insertBefore(content, header.nextSibling)
        nodesToMove.forEach(node => content.appendChild(node))

        // 从JSON存储恢复状态
        const savedState = collapsibleStates[storageKey]
        content.style.display = (savedState === 'collapsed') ? 'none' : 'block'

        // 修改后的点击事件处理
        header.addEventListener('click', function () {
          const isCollapsed = content.style.display === 'none'

          // 更新显示状态
          content.style.display = isCollapsed ? 'block' : 'none'

          // 更新JSON存储对象
          collapsibleStates = JSON.parse(localStorage.getItem('headingExpandedState') || '{}')
          collapsibleStates[storageKey] = isCollapsed ? 'expanded' : 'collapsed'

          // 保存整个JSON对象
          localStorage.setItem('headingExpandedState', JSON.stringify(collapsibleStates))
        })

        header.classList.add('collapsible')
      })
    })
  })
})()