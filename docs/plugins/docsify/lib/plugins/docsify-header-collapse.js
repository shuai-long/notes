!(function () {
  if (typeof window.$docsify === 'undefined') return

  const NAV_STATE_KEY = '__docsifyHeaderCollapseNavigation'
  const NAV_BOUND_KEY = '__docsifyHeaderCollapseNavigationBound'

  function normalizeValue(value) {
    const rawValue = (value || '').trim()

    if (!rawValue) return ''

    try {
      return decodeURIComponent(rawValue)
    } catch (error) {
      return rawValue
    }
  }

  function parseRoute(value) {
    const rawValue = value || window.location.hash || ''
    let hash = rawValue

    try {
      hash = new URL(rawValue, window.location.href).hash || rawValue
    } catch (error) {
      hash = rawValue
    }

    if (!hash.includes('#')) return { path: '', id: '' }

    const route = hash.slice(hash.indexOf('#') + 1).replace(/^!/, '')
    const parts = route.split('?')
    const query = parts.slice(1).join('?')
    const params = new URLSearchParams(query)

    return {
      path: normalizeValue(parts[0] || '/'),
      id: normalizeValue(params.get('id') || ''),
    }
  }

  function recordNavigation(source, href) {
    const route = parseRoute(href)
    window[NAV_STATE_KEY] = {
      source: source,
      path: route.path,
      id: route.id,
      createdAt: Date.now(),
    }
  }

  if (!window[NAV_BOUND_KEY]) {
    document.addEventListener(
      'click',
      function (event) {
        const link = event.target && event.target.closest && event.target.closest('a[href]')

        if (!link) return

        if (link.closest('.search .results-panel')) {
          recordNavigation('search', link.getAttribute('href'))
          return
        }

        if (link.closest('.sidebar-nav')) {
          recordNavigation('sidebar', link.getAttribute('href'))
        }
      },
      true
    )
    window[NAV_BOUND_KEY] = true
  }

  window.$docsify.plugins.push(function (hook) {
    let counters = [0, 0, 0, 0, 0]

    function getNavigationIntent() {
      const route = parseRoute()
      const state = window[NAV_STATE_KEY]

      if (!state || state.path !== route.path) {
        return { source: 'default', targetId: '' }
      }

      if (state.source === 'search') {
        return {
          source: 'search',
          targetId: state.id || route.id,
        }
      }

      return { source: state.source || 'default', targetId: '' }
    }

    function setCollapsed(content, collapsed) {
      content.style.display = collapsed ? 'none' : 'block'
    }

    hook.doneEach(function () {
      try {
        localStorage.removeItem('headingExpandedState')
      } catch (error) {
        // Ignore storage errors; heading state is intentionally not persisted.
      }

      counters = [0, 0, 0, 0, 0]
      const navigationIntent = getNavigationIntent()
      const shouldExpandSearchTarget =
        navigationIntent.source === 'search' && Boolean(navigationIntent.targetId)
      let targetHeader = null

      document.querySelector('.content').querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(header => {
        const tagLevel = parseInt(header.tagName.substring(1))
        const normalizedHeaderId = normalizeValue(header.id)

        if (tagLevel < 2) {
          const existingNumber = header.querySelector('.header-number')
          if (existingNumber) existingNumber.remove()
          return
        }

        const level = tagLevel - 2
        counters[level]++
        for (let i = level + 1; i < 5; i++) counters[i] = 0

        const sectionNumber = counters.slice(0, level + 1).join('.')
        header.dataset.sectionKey = sectionNumber

        let numberSpan = header.querySelector('.header-number')
        if (!numberSpan) {
          numberSpan = document.createElement('span')
          numberSpan.className = 'header-number'
          numberSpan.style.color = '#b0abab'
          header.insertBefore(numberSpan, header.children[1] || header.firstChild)
        }
        numberSpan.textContent = `${sectionNumber} `

        const content = document.createElement('div')
        content.className = 'collapsible-content'

        const nodesToMove = []
        let nextElem = header.nextElementSibling
        while (nextElem) {
          if (
            nextElem.matches('h1, h2, h3, h4, h5, h6') ||
            nextElem.classList.contains('docsify-pagination-container')
          ) {
            break
          }
          nodesToMove.push(nextElem)
          nextElem = nextElem.nextElementSibling
        }
        header.parentNode.insertBefore(content, header.nextSibling)
        nodesToMove.forEach(node => content.appendChild(node))

        const isSearchTarget =
          shouldExpandSearchTarget &&
          normalizedHeaderId &&
          normalizedHeaderId === navigationIntent.targetId
        setCollapsed(content, !isSearchTarget)

        if (isSearchTarget) {
          targetHeader = header
        }

        header.addEventListener('click', function () {
          const isCollapsed = content.style.display === 'none'
          setCollapsed(content, !isCollapsed)
        })

        header.classList.add('collapsible')
      })

      if (targetHeader) {
        setTimeout(function () {
          targetHeader.scrollIntoView({ block: 'start' })
        }, 0)
      }

      window[NAV_STATE_KEY] = null
    })
  })
})()
