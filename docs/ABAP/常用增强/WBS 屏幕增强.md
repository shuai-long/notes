# WBS

### 屏幕增强实现

在表 `PRPS` 中 include 结构 `CI_PRPS`，并加入自定义字段，如下图。

![截图 p137](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p138-02.png)

客户增强 `CNEX0007`：PS 客户指定字段 WBS 要素。

将该增强通过事务码 `CMOD` 注册到项目中。

![截图 p137](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p138-03.png)

双击出口进入，操作位置如下图。

![截图 p137](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p139-01.png)

点击红框位置继续进入，操作位置如下图。

![截图 p138](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p139-02.png)

在下图中双击对应程序。

![截图 p138](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p139-03.png)

INCLUDE 程序 `ZXCN1TOP`。

![截图 p138](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p139-04.png)

继续双击屏幕 `0700`，如下图。

![截图 p139](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p140-01.png)

将自定义结构 `CNCI_PRPS` 中的字段放到屏幕里。

![截图 p139](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p140-02.png)

根据输入状态对自定义屏幕中的字段进行输入控制。

![截图 p139](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p140-03.png)

出口函数 `EXIT_SAPLCJWB_004`。

![截图 p140](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p140-04.png)

出口函数 `EXIT_SAPLCJWB_005`。

![截图 p140](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p141-01.png)

### 屏幕增强位置

增强位置可以通过事务码 `CJ20N` 查看。

![截图 p140](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p141-02.png "image-grid cols=2 min=360 gap=12")
![截图 p141](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p141-03.png)
