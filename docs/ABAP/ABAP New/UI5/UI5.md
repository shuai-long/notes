[TOC]

# UI5 应用开发笔记 #

[**学习资料链接**](https://jerry.blog.csdn.net/article/details/120618353?spm=1001.2014.3001.5502)

1. **开发环境搭建**

   工具:`Visual Studio Code`  和 `Node.js`

   安装两个插件: `SAP Fiori tools - Extension Packs`  和 `UI5 Tools`

2. 引入 SAP UI5 库文件

   ```html
   <!DOCTYPE html>
   <html>
   <head>
       <meta charset="utf-8">
       <title>SAPUI5 Walkthrough</title>
       <script
           id="sap-ui-bootstrap"
           src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"
           data-sap-ui-async="true"
           data-sap-ui-resourceroots='{
               "sap.ui5.walkthrough": "./"
           }'
           data-sap-ui-oninit="module:sap/ui5/walkthrough/index">
       </script>
   </head>
   <body>
   <div>Hello World</div>
   </body>
   </html>
   ```

   ```js
   // 创建 index.js 文件, 并复制以下代码实现弹窗
   sap.ui.define([
   ], function () {
       "use strict";
       alert("UI5 库文件已经加载就绪");
   });
   ```
   
   * `https://sapui5.hana.ondemand.com/resources/sap-ui-core.js` 为库文件的地址, 可直接放在浏览器打开
   
   * `data-sap-ui-async="true"` 同时执行多个异步加载所有模块的网络请求, 而不会阻塞 UI
   
   * 加载完 `sap-ui-core.js` 后, 执行另外 JS 中相关的操作的过程, 技术上就称为 SAP UI5的 Bootstrap 过程,可以通过两对设置来实现使用属性 data-sap-ui-oninit, 去定义当 SAP UI5 完成引导过程之后，需要加载的 module 的完整路径为：`sap/ui5/walkthrough/index`.
   
     * 其中末尾的 index, 对应的就是我们之前创建的 index.js. 而 sap/ui5/walkthrough, 实际上对应了一个命名空间：`sap.ui5.walkthrough`.
   
     * 使用 `data-sap-ui-resourceroots` 定义一个命名空间 (namespace), 并告诉 SAP UI5 框架，命名空间 `sap.ui5.walkthrough` 下面的资源文件，一律从 `index.html` 文件所在的当前目录，也就是 `./` 下面去检索资源文件并且加载。
   
     * 这里我们定义的命名空间是 `sap.ui5.walkthrough`, 需要和上面 `data-sap-ui-oninit` 定义的 module 的前缀 `sap/ui5/walkthrough` 一致。也就是说，上图第 11 行的命名空间，我们可以任意指定成 A.B.C, 但是随之而来的第 13 行代码，`data-sap-ui-oninit="module:A/B/C/index"` 必须与之匹配。
   
     * `data-sap-ui-oninit="module:sap/ui5/walkthrough/index"` 
     * `data-sap-ui-resourceroots='{"sap.ui5.walkthrough": "./"}'`

1. 