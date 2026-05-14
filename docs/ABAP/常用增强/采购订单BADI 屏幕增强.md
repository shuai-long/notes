# ME21N/ME22N/ME23N

### 屏幕增强实现

在表 `CSKB` 中 include 结构 `CI_CSKB`，并加入自定义字段，如下图。

![CSKB 中 INCLUDE 结构 CI_CSKB](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p287-01.png)

创建自定义表 `ZMKPF` 以及结构 `ZMKPF_BADI`，如下图。

![自定义表 ZMKPF](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p287-02.png "image-grid cols=2 min=360 gap=12")
![结构 ZMKPF_BADI](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p287-03.png)

创建自定义表 `ZMSEG` 以及结构 `ZMSEG_BADI`，如下图。

![自定义表 ZMSEG](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p288-01.png "image-grid cols=2 min=360 gap=12")
![结构 ZMSEG_BADI](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p288-02.png)

创建函数组 `ZMIGO_FG`，如下图。

![函数组 ZMIGO_FG](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p289-01.png)

函数组的全局变量如下图。

![函数组全局变量](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p289-02.png)

函数 `ZMIGO_HEADER_GET_DATA`：抬头数据从 SCREEN -> BADI。

![ZMIGO_HEADER_GET_DATA 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p290-01.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_HEADER_GET_DATA 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p290-02.png)

函数 `ZMIGO_HEADER_SET_DATA`：抬头数据从 BADI -> SCREEN。

![ZMIGO_HEADER_SET_DATA 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p290-03.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_HEADER_SET_DATA 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p291-01.png)

函数 `ZMIGO_ITEM_GET_DATA`：行项目数据从 SCREEN -> BADI。

![ZMIGO_ITEM_GET_DATA 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p291-02.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_ITEM_GET_DATA 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p291-03.png)

函数 `ZMIGO_ITEM_SET_DATA`：行项目数据从 BADI -> SCREEN。

![ZMIGO_ITEM_SET_DATA 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p292-01.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_ITEM_SET_DATA 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p292-02.png)

函数 `ZMIGO_SAVE_DATA`：增强字段数据保存到自定义表。

![ZMIGO_SAVE_DATA 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p293-01.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_SAVE_DATA 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p293-02.png)

函数 `ZMIGO_SET_GOACTION`：取得 `MIGO` 操作码及事务码。

![ZMIGO_SET_GOACTION 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p294-01.png "image-grid cols=2 min=360 gap=12")
![ZMIGO_SET_GOACTION 代码](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p294-02.png)

屏幕 `9001`：`MIGO` 抬头自定义屏幕增强。

![屏幕 9001 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p295-01.png "image-grid cols=2 min=360 gap=12")
![屏幕 9001 字段布局](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p295-02.png)

![屏幕 9001 逻辑流](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p295-03.png "image-grid cols=2 min=360 gap=12")
![屏幕 9001 PBO 逻辑](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p296-01.png)

屏幕 `9002`：`MIGO` 行项目自定义屏幕增强。

![屏幕 9002 属性](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p296-02.png "image-grid cols=2 min=360 gap=12")
![屏幕 9002 字段布局](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p296-03.png)

![屏幕 9002 逻辑流](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p297-01.png "image-grid cols=2 min=360 gap=12")
![屏幕 9002 PBO 逻辑](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p297-02.png)

BADI `MB_MIGO_BADI`：外部详细子屏幕的 `MIGO` 中的 BAdI。

在事务码 `SE19` 中创建 BADI 实例，本例的 BADI 实例为 `ZMB_MIGO_BADI`，然后在事务码 `SE19` 打开实例 `ZMB_MIGO_BADI`，如下图。

![SE19 打开 BADI 实例 ZMB_MIGO_BADI](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p298-01.png)

双击上图红框处，进入实例 `ZMB_MIGO_BADI` 对应的类；进入后打开 `Attribute` 页签，为该类增加三个全局变量。

![BADI 实现类 Attribute 页签](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p298-02.png)

方法 `INIT`：

![方法 INIT](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p299-01.png)

方法 `LINE_DELETE`：

![方法 LINE_DELETE](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p299-05.png)

方法 `RESET`：

![方法 RESET](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p299-06.png)

方法 `MODE_SET`：

![方法 MODE_SET](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p300-01.png)

### 屏幕增强位置

抬头和行项目增强页签显示效果如下图。

![MIGO 抬头和行项目增强页签](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p300-05.png "image-grid cols=2 min=360 gap=12")
![MIGO 行项目增强页签](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p301-01.png)
