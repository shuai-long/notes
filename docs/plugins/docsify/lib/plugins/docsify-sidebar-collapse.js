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
      .sidebar {
        background: linear-gradient(180deg, rgba(250, 252, 255, 0.98), rgba(246, 248, 251, 0.98));
      }
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
        background: rgba(66, 185, 131, 0.10);
        color: #1f7a56;
        transform: translateX(1px);
      }
      .sidebar-nav li a::before {
        content: none !important;
        display: none !important;
      }
      .sidebar-nav li.active > a,
      .sidebar-nav li.open > a {
        background: rgba(66, 185, 131, 0.14);
        color: var(--theme-color, #42b983);
        font-weight: 600;
      }
      .sidebar-nav .sidebar-chapter-number {
        align-items: center;
        background: rgba(148, 163, 184, 0.14);
        border-radius: 999px;
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
      .sidebar-nav li.open > a .sidebar-chapter-number,
      .sidebar-nav li.arrow.folder-expanded > .sidebar-folder-label .sidebar-chapter-number {
        background: rgba(66, 185, 131, 0.16);
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
        background: rgba(148, 163, 184, 0.10);
      }
      .sidebar-nav li.arrow.folder-expanded {
        background: rgba(66, 185, 131, 0.06);
      }
      .sidebar-nav li.arrow.folder-expanded > ul {
        background: rgba(255, 255, 255, 0.48);
        border-radius: 0 0 6px 6px;
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

    function normalizeText(text) {
      return (text || '').replace(/\s+/g, ' ').trim();
    }

    function createChapterNumber(value) {
      const number = document.createElement('span');
      number.className = 'sidebar-chapter-number';
      number.textContent = value;
      return number;
    }

    function ensureLinkNumber(link, number) {
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

    function ensureFolderLabel(li, number) {
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
      label.appendChild(createChapterNumber(number));
      title.textContent = titleText;
      label.appendChild(title);
    }

    document.querySelectorAll('.sidebar-nav li').forEach(li => {
      const elementId = `sidebar-path-${generateElementPath(li)}`;
      li.dataset.sidebarId = elementId;

      const directLink = Array.from(li.children).find(
        child => child.tagName === 'A'
      );

      if (directLink) {
        ensureLinkNumber(directLink, generateElementPath(li));
      }

      const hasDirectLink = Array.from(li.children).some(
        child => child.tagName === 'A'
      );

      if (hasDirectLink) return;

      ensureFolderLabel(li, generateElementPath(li));

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
