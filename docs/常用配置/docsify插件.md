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


### dashbord

要创建仪表板，只需将以下代码添加到您的 markdown 文件中

```markdown
<!-- tabs:start -->

<!-- dashboard: metadata/posts numTabContent=5 -->

<!-- tabs:end -->
```

### tag-list

创建侧边栏标签列表，只需将以下代码添加到侧边栏文件(例如：`_sidebar.md`)

```markdown
<!-- tag-list -->
```

### Flexible Alerts

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

### 音频播放

在 markdown 文件中使用 `audio` 代码块即可渲染音频播放器。第一行填写音频地址，后续可选配置使用 `key=value`。

````markdown
```audio
./media/demo.mp3
title=示例音频
```
````

也可以直接访问音频文件路由，例如：

```markdown
http://localhost:3000/#/media/demo.mp3
```

支持的音频格式：`mp3`、`wav`、`ogg`、`m4a`、`aac`、`flac`、`opus`。

### 视频播放

在 markdown 文件中使用 `video` 代码块即可渲染视频播放器。第一行填写视频地址，可使用 `title` 设置标题，使用 `poster` 设置封面图。

````markdown
```video
./media/demo.mp4
title=示例视频
poster=./media/demo-cover.jpg
```
````

也可以直接访问视频文件路由，例如：

```markdown
http://localhost:3000/#/media/demo.mp4
```

支持的视频格式：`mp4`、`webm`、`ogv`、`mov`、`m4v`。

### Mermaid 流程图

在 markdown 文件中使用 `mermaid` 代码块即可渲染流程图。

````markdown
```mermaid
flowchart TD
  A[开始] --> B{是否通过校验}
  B -- 是 --> C[执行处理]
  B -- 否 --> D[返回错误]
  C --> E[结束]
```
````

常用方向：

```markdown
flowchart TD  从上到下
flowchart LR  从左到右
```

### 图片排列

用于在页面中一行展示多张图片。推荐把控制参数写在第一张图片的 `title` 中，这样在 Typora 中正常预览时不会额外显示控制标记。

```markdown
![图片 A](./images/a.png "image-grid cols=3 min=260 gap=12")
![图片 B](./images/b.png)
![图片 C](./images/c.png)
```

参数说明：

- `cols`：最多显示几列，例如 `cols=3` 表示最多一行三张。
- `min`：每张图片建议的最小宽度，例如 `min=260` 表示宽度不足时自动换行。
- `gap`：图片之间的间距，例如 `gap=12`。
- `height`：限制图片最大高度，例如 `height=220`。

例如：屏幕宽度够时一行三张，宽度只够两张时会自动变成一行两张，再不够时变成单列。

```markdown
![图片 A](./images/a.png "image-grid cols=4 min=240 gap=10 height=220")
![图片 B](./images/b.png)
![图片 C](./images/c.png)
![图片 D](./images/d.png)
```

注意：需要排列到同一个图片组中的图片要连续书写，中间不要空行或插入正文。

如果图片路径中包含空格，可以使用尖括号：

```markdown
![截图 A](<./images/SAP 截图 A.png> "image-grid cols=2 min=280")
![截图 B](<./images/SAP 截图 B.png>)
```

也可以使用注释块写法，适合需要明确标记图片组范围的场景：

```markdown
<!-- image-grid:start cols=3 min=260 gap=12 -->
![图片 A](./images/a.png)
![图片 B](./images/b.png)
![图片 C](./images/c.png)
<!-- image-grid:end -->
```
