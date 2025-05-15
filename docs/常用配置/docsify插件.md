# docsify

- 使用教程
  - [Docsify-zh](https://docsify.js.org/#/zh-cn/quickstart)
  - [Docsify-en](https://docsify.js.org/#/)
  - [Docsify-使用指南](https://ysgstudyhards.github.io/Docsify-Guide/#/ProjectDocs/Docsify使用指南)

- 生成侧边栏程序指令

  ```shell
  cd /Users/zhangshuailong/Desktop/notes/notes/docs
  python3 sidebarn.py .  
  ```

<!-- tabs:start -->

#### **dashbord**

要创建仪表板，只需将以下代码添加到您的 markdown 文件中

```markdown
<!-- tabs:start -->

<!-- dashboard: metadata/posts numTabContent=5 -->

<!-- tabs:end -->
```

#### **tag-list**

创建侧边栏标签列表，只需将以下代码添加到侧边栏文件(例如：`_sidebar.md`)

```markdown
<!-- tag-list -->
```

#### **Flexible Alerts**

[GitHub地址](https://github.com/fzankl/docsify-plugin-flexible-alerts)

- Note

  ```markdown
  > [!NOTE]
  > An alert of type 'note' using global style 'callout'.
  ```

- Tip

  ```markdown
  > [!TIP]
  > An alert of type 'tip' using global style 'callout'.
  ```

- Warning

  ```markdown
  > [!WARNING]
  > An alert of type 'warning' using global style 'callout'.
  ```

- Attention

  ```markdown
  > [!ATTENTION]
  > An alert of type 'attention' using global style 'callout'.
  ```

<!-- tabs:end -->

