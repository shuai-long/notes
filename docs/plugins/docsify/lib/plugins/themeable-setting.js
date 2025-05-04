window.$docsify = window.$docsify || {};
window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook, vm) {
  // 配置需要管理的CSS变量（根据你的主题实际变量修改）
  const themeVariables = [
    '--theme-color',
    '--link-color',
    '--sidebar-width',
    '--base-background',
    '--base-text-color',
    '--code-background'
  ];

  // 创建样式
  const style = document.createElement('style');
  style.textContent = `
    #themeConfigBtn {
      position: fixed;
      bottom: 20px;
      right: 20px;
      z-index: 9999;
      padding: 10px 15px;
      background: var(--theme-color, #42b983);
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
    }
    #themeConfigModal {
      display: none;
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      background: white;
      padding: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      z-index: 10000;
      max-width: 500px;
      width: 90%;
    }
    #variableTable {
      width: 100%;
      margin: 15px 0;
    }
    #variableTable th, #variableTable td {
      padding: 8px;
      text-align: left;
    }
  `;
  document.head.appendChild(style);

  // 创建按钮和模态框
  const btn = document.createElement('button');
  btn.id = 'themeConfigBtn';
  btn.textContent = '主题配置';
  document.body.appendChild(btn);

  const modal = document.createElement('div');
  modal.id = 'themeConfigModal';
  modal.innerHTML = `
    <h3>主题配置</h3>
    <table id="variableTable">
      <thead>
        <tr>
          <th>变量名</th>
          <th>当前值</th>
          <th>新值</th>
        </tr>
      </thead>
      <tbody></tbody>
    </table>
    <button id="saveConfig">保存配置</button>
    <button id="closeModal">关闭</button>
  `;
  document.body.appendChild(modal);

  // 加载保存的配置
  function loadSavedConfig() {
    const saved = localStorage.getItem('docsifyThemeConfig');
    if (saved) {
      const config = JSON.parse(saved);
      Object.entries(config).forEach(([varName, value]) => {
        document.documentElement.style.setProperty(varName, value);
      });
    }
  }

  // 初始化加载保存的配置
  loadSavedConfig();

  // 显示模态框
  btn.addEventListener('click', () => {
    const tbody = modal.querySelector('tbody');
    tbody.innerHTML = '';

    themeVariables.forEach(varName => {
      const currentValue = getComputedStyle(document.documentElement)
        .getPropertyValue(varName).trim();

      const row = document.createElement('tr');
      row.innerHTML = `
        <td>${varName}</td>
        <td>${currentValue}</td>
        <td><input type="text" value="${currentValue}"></td>
      `;
      tbody.appendChild(row);
    });

    modal.style.display = 'block';
  });

  // 关闭模态框
  modal.querySelector('#closeModal').addEventListener('click', () => {
    modal.style.display = 'none';
  });

  // 修改后的保存配置逻辑
  modal.querySelector('#saveConfig').addEventListener('click', () => {
    const config = {};

    modal.querySelectorAll('tr').forEach(row => {
      const varName = row.cells[0].textContent;
      const input = row.querySelector('input');

      // 获取当前计算值作为旧值
      const currentValue = getComputedStyle(document.documentElement)
        .getPropertyValue(varName)
        .trim();

      // 带校验的新值获取逻辑
      const newValue = input.value.trim() || currentValue; // 空值时使用旧值
      input.value = newValue; // 自动修正输入框显示

      config[varName] = newValue;

      // 添加强制样式更新
      document.documentElement.style.setProperty(varName, newValue);
      // 同步更新原始<style>标签中的变量定义（关键修复）
      updateRootVariables(config);
    });

    localStorage.setItem('docsifyThemeConfig', JSON.stringify(config));
    modal.style.display = 'none';
    alert('配置已保存！');

    // 添加强制重绘（针对某些浏览器缓存问题）
    document.body.style.zoom = document.body.style.zoom === '1' ? '0.99' : '1';
  });

  // 新增函数：更新<style>标签中的变量定义
  function updateRootVariables(config) {
    let styleContent = ':root {';
    Object.entries(config).forEach(([varName, value]) => {
      styleContent += `${varName}: ${value} !important;`;
    });
    styleContent += '}';

    const existingStyle = document.getElementById('dynamicThemeVars');
    if (existingStyle) {
      existingStyle.textContent = styleContent;
    } else {
      const newStyle = document.createElement('style');
      newStyle.id = 'dynamicThemeVars';
      newStyle.textContent = styleContent;
      document.head.appendChild(newStyle);
    }
  }

  // 修改后的加载配置逻辑（确保在样式加载后执行）
  hook.mounted(function () {
    const saved = localStorage.getItem('docsifyThemeConfig');
    if (saved) {
      const config = JSON.parse(saved);
      updateRootVariables(config);
    }
  });

});