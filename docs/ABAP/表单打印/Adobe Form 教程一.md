[TOC]

# Adobe Form #

使用事务代码: ==SPF== 。 [参考链接](https://www.cnblogs.com/hhelibeb/p/15493324.html)

## 简单示例 ##

1. 创建 Interface ( Interface 对于 adobe form 是必填项 )

   * 在 form interface 中，你可以指定和应用程序交互的数据（表、结构、工作区）。
   * 使用全局定义，定义你的字段、变量等。
   * 系统字段包含预定义的数据，比如 date。

   ![步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413135526464.png)

   ![步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413181911613.png)

2. 填入描述并保存

   ![步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413182049404.png)

   ![步骤4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413182215378.png)

3. 检查 interface 属性，浏览面板的左侧和右侧，点击自动生成的 parameter name，

   ![步骤5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413182940570.png)

4. 添加自己的 parameter name。选择 form interface（在左侧）下的 import 选项，点击创建按钮（在右侧）。增加一个 importing parameter ==IV_TEXT==. ==保存并激活==

   ![步骤6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413183926327.png)

5. 回到事务码: ==SFP== , 创建 Form

   ![步骤7](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413184200407.png)

6. 填写描述, 选择界面( 刚刚创建的 Interface 名称 ), 选择请求号并保存

   ![步骤8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413184315349.png)

7. 展开接口的导入参数, 并拖放需要的变量到 创建的 Form 中。

   ![image-20230413184702518](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20230413184702518.png)

   > **表格构建器( Form builder )中 上下文( form context ) 的意义是什么:**
   >
   > 在 context（也称为 form context）中，指定将哪些数据从 interface 复制到 form。还可以将此数据作为节点包含在层次结构中。在此层次结构中，还可以通过指定处理节点的条件来决定 form 逻辑。
   >
   > 表格构建器 中的 context 函数是将 interface 绑定到布局的链接。可以从现有 interface 构造 form context。
   >
   > 如果上面的解释让你觉得很模糊，那就先忘掉它。只需将 context 记作全局区域的数据声明，可能就像 TOP Include 程序一样。
   >
   > 简而言之，如果你希望参数，即内表、工作区或变量等从程序传递到 interface，然后传递到 form，那么你需要在表单的 context 中定义该内表、工作区或变量 。
   >
   > form context 中定义的任何内容都可以在要显示或操作的 form 中使用。 如果在 form Interface 中定义了一个变量但没有在 context 中创建它，那么该 Interface 变量将不会在 form 中可用。
   >
   > **提示：**
   >
   > 从 interface 拖放到 context，然后更改 context 元素的名称 / 描述。 这会节省时间，并且不需要显式维护属性。
   >
   > 如果你不喜欢快捷方式（拖放）或希望上下文参数名称与表单界面中的名称不同，则需要自己在上下文元素的属性中指定数据字段。 例如，如果你想创建一个上下文 P_TEXT 但想用 IV_TEXT 绑定它，那么数据字段应该是 IV_TEXT，如下所示。

   
