window.$docsify = window.$docsify || {};
const docsifySidebarExpandedIds = new Set();

window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  hook.doneEach(function () {
    try {
      localStorage.removeItem('sidebarExpandedState');
    } catch (error) {
      // Ignore storage errors; sidebar state is intentionally not persisted.
    }

    const oldStyle = document.getElementById('docsify-sidebar-collapse-style');
    if (oldStyle) oldStyle.remove();

    const style = document.createElement('style');
    style.id = 'docsify-sidebar-collapse-style';
    style.textContent = `
      .sidebar-nav {
        padding: 8px 6px 24px;
      }
      .sidebar-nav ul {
        margin: 0;
        padding-left: 0;
      }
      .sidebar-nav li {
        list-style: none;
        margin: 2px 0;
        position: relative;
      }
      .sidebar-nav li ul {
        border-left: 1px solid rgba(148, 163, 184, 0.28);
        margin: 3px 0 5px 7px;
        padding-left: 6px;
      }
      .sidebar-nav li a {
        align-items: center;
        border-radius: 6px;
        color: #475569;
        display: flex;
        font-size: 13px;
        gap: 5px;
        line-height: 1.35;
        min-height: 28px;
        overflow: visible;
        padding: 4px 6px;
        text-overflow: clip;
        transition: background-color 0.16s ease, color 0.16s ease, transform 0.16s ease;
        white-space: normal;
      }
      .sidebar-nav li a:hover {
        color: #1f7a56;
        transform: translateX(1px);
      }
      .sidebar-nav li a::before {
        content: none !important;
        display: none !important;
      }
      .sidebar-nav li.active > a,
      .sidebar-nav li.open > a {
        color: var(--theme-color, #42b983);
        font-weight: 600;
      }
      .sidebar-nav .sidebar-chapter-number {
        align-items: center;
        color: #64748b;
        display: inline-flex;
        flex: 0 0 auto;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 10px;
        font-weight: 700;
        justify-content: center;
        letter-spacing: 0;
        line-height: 1;
        min-width: 18px;
        padding: 3px 5px;
        text-align: center;
      }
      .sidebar-nav li.active > a .sidebar-chapter-number,
      .sidebar-nav li.open > a .sidebar-chapter-number {
        color: var(--theme-color, #42b983);
      }
      .sidebar-nav .sidebar-chapter-title,
      .sidebar-nav .sidebar-folder-title {
        min-width: 0;
        overflow-wrap: anywhere;
      }
      .sidebar-nav li.arrow > ul {
        display: none;
      }
      .sidebar-nav li.arrow.folder-expanded > ul {
        display: block;
      }
      .sidebar-nav li.arrow {
        border-radius: 6px;
        color: #334155;
        cursor: pointer;
        font-size: 13px;
        font-weight: 650;
        line-height: 1.35;
        min-height: 28px;
        padding: 0;
        user-select: none;
      }
      .sidebar-nav li.arrow > .sidebar-folder-label {
        align-items: center;
        border-radius: 6px;
        display: flex;
        gap: 5px;
        min-height: 28px;
        padding: 4px 6px;
        white-space: normal;
      }
      .sidebar-nav li.arrow:hover {
        color: #1f7a56;
      }
      .sidebar-nav li.arrow.folder-expanded {
        color: #1f7a56;
      }
      .sidebar-nav li.arrow.folder-expanded > ul {
        border-radius: 0;
      }
    `;
    document.head.appendChild(style);

    function generateElementPath(element) {
      const path = [];
      let currentElement = element;

      while (currentElement && currentElement.tagName === 'LI') {
        const parent = currentElement.parentElement;
        const siblings = Array.from(parent.children).filter(child => child.tagName === 'LI');
        const index = siblings.indexOf(currentElement);
        path.unshift(index + 1);

        const parentLi = parent.parentElement;
        currentElement = parentLi && parentLi.tagName === 'LI' ? parentLi : null;
      }

      return path.join('.');
    }

    function isChapterLink(link) {
      const href = link.getAttribute('href') || '';
      return (
        link.classList.contains('section-link') ||
        Boolean(link.closest('ul.app-sub-sidebar')) ||
        href.includes('?id=')
      );
    }

    function generateChapterPath(element) {
      const path = [];
      let currentElement = element;

      while (currentElement && currentElement.tagName === 'LI') {
        const parent = currentElement.parentElement;
        const siblings = Array.from(parent.children).filter(child => {
          const link = Array.from(child.children).find(node => node.tagName === 'A');
          return link && isChapterLink(link);
        });
        const index = siblings.indexOf(currentElement);
        if (index >= 0) path.unshift(index + 1);

        if (parent.classList.contains('app-sub-sidebar')) break;

        const parentLi = parent.parentElement;
        currentElement = parentLi && parentLi.tagName === 'LI' ? parentLi : null;
      }

      return path.join('.');
    }

    function normalizeChapterId(value) {
      const rawValue = (value || '').trim();

      if (!rawValue) return '';

      try {
        return decodeURIComponent(rawValue);
      } catch (error) {
        return rawValue;
      }
    }

    function getChapterLinkId(link) {
      const href = link.getAttribute('href') || '';
      const marker = '?id=';
      const markerIndex = href.indexOf(marker);

      if (markerIndex < 0) return '';

      return normalizeChapterId(
        href
          .slice(markerIndex + marker.length)
          .split('&')[0]
      );
    }

    function createChapterNumberMap() {
      const chapterLinks = Array.from(document.querySelectorAll('.sidebar-nav a'))
        .filter(isChapterLink);
      const linkedIds = new Set(
        chapterLinks
          .map(getChapterLinkId)
          .filter(Boolean)
      );

      if (!linkedIds.size) return new Map();

      const headings = Array.from(
        document.querySelectorAll(
          '.markdown-section h1[id], .markdown-section h2[id], .markdown-section h3[id], .markdown-section h4[id], .markdown-section h5[id], .markdown-section h6[id]'
        )
      )
        .map(heading => ({
          id: normalizeChapterId(heading.id),
          level: Number(heading.tagName.slice(1)),
        }))
        .filter(heading => linkedIds.has(heading.id));

      if (!headings.length) return new Map();

      const minLevel = Math.min(...headings.map(heading => heading.level));
      const counters = [];
      const numbers = new Map();

      headings.forEach(heading => {
        const depth = Math.max(0, heading.level - minLevel);
        counters.length = depth + 1;

        for (let index = 0; index < depth; index += 1) {
          if (!counters[index]) counters[index] = 1;
        }

        counters[depth] = (counters[depth] || 0) + 1;
        numbers.set(heading.id, counters.slice(0, depth + 1).join('.'));
      });

      return numbers;
    }

    function getChapterNumber(link, element, chapterNumberMap) {
      const chapterId = getChapterLinkId(link);

      return (
        (chapterId && chapterNumberMap.get(chapterId)) ||
        generateChapterPath(element)
      );
    }

    function normalizeText(text) {
      return (text || '').replace(/\s+/g, ' ').trim();
    }

    function createChapterNumber(value) {
      const number = document.createElement('span');
      number.className = 'sidebar-chapter-number';
      number.textContent = value;
      return number;
    }

    function removeLinkNumber(link) {
      const oldNumber = link.querySelector(':scope > .sidebar-chapter-number');
      const title = link.querySelector(':scope > .sidebar-chapter-title');
      if (oldNumber) oldNumber.remove();
      if (title) {
        link.textContent = normalizeText(title.textContent);
      }
    }

    function ensureChapterNumber(link, number) {
      let title = link.querySelector(':scope > .sidebar-chapter-title');
      const oldNumber = link.querySelector(':scope > .sidebar-chapter-number');
      if (oldNumber) oldNumber.remove();

      if (!title) {
        const text = normalizeText(link.textContent);
        link.textContent = '';
        title = document.createElement('span');
        title.className = 'sidebar-chapter-title';
        title.textContent = text;
        link.appendChild(title);
      }

      link.insertBefore(createChapterNumber(number), title);
    }

    function ensureFolderLabel(li) {
      let label = Array.from(li.children).find(child => child.classList.contains('sidebar-folder-label'));
      let title = label && label.querySelector('.sidebar-folder-title');
      const directText = normalizeText(
        Array.from(li.childNodes)
          .filter(node => node.nodeType === Node.TEXT_NODE)
          .map(node => node.textContent)
          .join(' ')
      );

      if (!label) {
        label = document.createElement('span');
        label.className = 'sidebar-folder-label';
        li.insertBefore(label, li.firstChild);
      }

      if (!title) {
        title = document.createElement('span');
        title.className = 'sidebar-folder-title';
      }

      const titleText = directText || normalizeText(title.textContent);
      Array.from(li.childNodes)
        .filter(node => node.nodeType === Node.TEXT_NODE)
        .forEach(node => node.remove());

      label.textContent = '';
      title.textContent = titleText;
      label.appendChild(title);
    }

    const chapterNumberMap = createChapterNumberMap();

    document.querySelectorAll('.sidebar-nav li').forEach(li => {
      const elementId = `sidebar-path-${generateElementPath(li)}`;
      li.dataset.sidebarId = elementId;

      const directLink = Array.from(li.children).find(
        child => child.tagName === 'A'
      );

      if (directLink) {
        if (isChapterLink(directLink)) {
          ensureChapterNumber(directLink, getChapterNumber(directLink, li, chapterNumberMap));
        } else {
          removeLinkNumber(directLink);
        }
      }

      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink) return;

      ensureFolderLabel(li);

      if (!li.classList.contains('has-arrow')) {
        li.classList.add('arrow', 'has-arrow');
        li.setAttribute('role', 'button');
        li.setAttribute('tabindex', '0');

        li.addEventListener('click', function (e) {
          if (e.target.closest('a')) return;
          if (e.target === this || e.target.closest('.sidebar-folder-label')) {
            this.classList.toggle('folder-expanded');
            if (this.classList.contains('folder-expanded')) {
              docsifySidebarExpandedIds.add(this.dataset.sidebarId);
            } else {
              docsifySidebarExpandedIds.delete(this.dataset.sidebarId);
            }
            e.stopPropagation();
            e.preventDefault();
          }
        });

        li.addEventListener('keydown', function (e) {
          if (e.key !== 'Enter' && e.key !== ' ') return;
          this.classList.toggle('folder-expanded');
          if (this.classList.contains('folder-expanded')) {
            docsifySidebarExpandedIds.add(this.dataset.sidebarId);
          } else {
            docsifySidebarExpandedIds.delete(this.dataset.sidebarId);
          }
          e.stopPropagation();
          e.preventDefault();
        });
      }

      li.classList.toggle('folder-expanded', docsifySidebarExpandedIds.has(elementId));
    });
  });
});
