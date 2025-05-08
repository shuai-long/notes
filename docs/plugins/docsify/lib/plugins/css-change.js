window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  // 初始化保存按钮
  function initButton() {
    const btn = document.createElement('button');
    btn.innerHTML = '✎ 编辑样式';
    Object.assign(btn.style, {
      position: 'fixed',
      bottom: '20px',
      right: '20px',
      zIndex: 9999,
      padding: '8px 16px',
      background: '#42b983',
      color: 'white',
      border: 'none',
      borderRadius: '4px',
      cursor: 'pointer'
    });
    btn.addEventListener('click', () => modal.classList.add('active'));
    document.body.appendChild(btn);
    return btn;
  }

  // 创建样式隔离的模态框
  function createModal() {
    const modal = document.createElement('div');
    const shadow = modal.attachShadow({ mode: 'open' });

    // Shadow DOM内部样式
    const style = document.createElement('style');
    style.textContent = `
      :host {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.5);
        z-index: 10000;
      }

      :host(.active) {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .container {
        position: relative;  /* 新增定位上下文 */
        background: white;
        padding: 30px 20px 20px; /* 调整顶部内边距 */
        border-radius: 8px;
        width: 60%;
        max-width: 600px;
      }
      .close-btn {
        position: absolute;
        top: 10px;
        right: 10px;
        background: none;
        border: none;
        font-size: 1.2em;
        cursor: pointer;
        color: #666;
      }
      .close-btn:hover {
        color: #333;
      }
      textarea {
        width: 100%;
        height: 300px;
        margin: 10px 0;
        font-family: monospace;
      }
      .button-group {
        text-align: right;
      }
      button {
        padding: 6px 12px;
        margin-left: 8px;
      }
    `;

    // 模态框内容
    const container = document.createElement('div');
    container.className = 'container';
    container.innerHTML = `
      <button class="close-btn">×</button>
      <h3>自定义CSS</h3>
      <textarea placeholder="输入CSS样式..."></textarea>
      <div class="button-group">
        <button class="reset">重置</button>
        <button class="save">保存</button>
      </div>
    `;

    shadow.appendChild(style);
    shadow.appendChild(container);
    document.body.appendChild(modal);
    return modal;
  }

  // 应用保存的样式
  function applySavedStyles() {
    const savedCss = localStorage.getItem('docsify-custom-css');
    if (savedCss) {
      // 移除旧样式
      const oldStyle = document.getElementById('docsify-custom-css');
      if (oldStyle) oldStyle.remove();

      // 添加新样式
      const style = document.createElement('style');
      style.id = 'docsify-custom-css';
      style.textContent = savedCss;
      document.head.appendChild(style);
    }
  }

  // 初始化操作
  let modal, textarea;
  hook.init(function () {
    // 创建界面元素
    initButton();
    modal = createModal();
    textarea = modal.shadowRoot.querySelector('textarea');

    // 加载已保存的CSS
    textarea.value = localStorage.getItem('docsify-custom-css') || '';

    // 更新后的Tab键处理逻辑
    textarea.addEventListener('keydown', function(e) {
      if (e.key === 'Tab' || e.keyCode === 9) {
        e.preventDefault();
        const start = this.selectionStart;
        const end = this.selectionEnd;
        const value = this.value;

        if (e.shiftKey) {
          // Shift+Tab 删除缩进
          const linesBefore = value.substring(0, start).split('\n');
          const linesAfter = value.substring(end).split('\n');
          const selection = value.substring(start, end);
          
          // 删除每行开头的两个空格
          const modified = selection.replace(/^ /gm, '');
          this.value = [
            ...linesBefore.slice(0, -1),
            linesBefore[linesBefore.length - 1] + modified + linesAfter[0],
            ...linesAfter.slice(1)
          ].join('\n');
          
          // 调整光标位置
          const diff = selection.length - modified.length;
          this.selectionStart = Math.max(start - diff, 0);
          this.selectionEnd = end - diff;
        } else {
          // 普通Tab 插入两个空格
          const linesBefore = value.substring(0, start).split('\n');
          const linesAfter = value.substring(end).split('\n');
          const selection = value.substring(start, end);
          
          // 添加两个空格缩进
          const modified = selection.replace(/^/gm, '  '); // 改为两个空格
          this.value = [
            ...linesBefore.slice(0, -1),
            linesBefore[linesBefore.length - 1] + modified + linesAfter[0],
            ...linesAfter.slice(1)
          ].join('\n');
          
          // 调整光标位置
          const addedSpace = modified.split('\n').length * 2;
          this.selectionStart = start + (start === end ? 2 : 0);
          this.selectionEnd = end + modified.length - selection.length;
        }
      }
    });


    // 关闭按钮事件
    modal.shadowRoot.querySelector('.close-btn').addEventListener('click', () => {
      modal.classList.remove('active');
    });

    // 新增重置按钮事件
    modal.shadowRoot.querySelector('.reset').addEventListener('click', () => {
      if (confirm('确定要重置所有自定义样式吗？')) {
        // 清除本地存储
        localStorage.removeItem('docsify-custom-css');
        // 移除页面样式
        const oldStyle = document.getElementById('docsify-custom-css');
        if (oldStyle) oldStyle.remove();
        // 清空编辑器内容
        textarea.value = '';
        // 关闭模态框
        modal.classList.remove('active');
      }
    });

    modal.shadowRoot.querySelector('.save').addEventListener('click', () => {
      try {
        // 验证CSS有效性
        const css = textarea.value;
        const style = document.createElement('style');
        style.textContent = css;
        document.head.appendChild(style);
        style.remove();

        // 保存到localStorage
        localStorage.setItem('docsify-custom-css', css);
        modal.classList.remove('active');
        applySavedStyles();
      } catch (e) {
        alert('CSS语法错误,请检查后重试');
      }
    });
  });

  // 每次路由切换后应用样式
  hook.mounted(function () {
    applySavedStyles();
  });

  // 初始加载应用样式
  hook.ready(function () {
    applySavedStyles();
  });
});