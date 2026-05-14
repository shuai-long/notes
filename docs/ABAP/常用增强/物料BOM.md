# CS01/CS02/CS03

## 物料BOM 抬头屏幕增强

### 屏幕增强实现

在表 `STKO` 中 include 结构 `CI_STKO`，并加入自定义字段，如下图。

![截图 p154](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p155-01.png)

客户增强 `PCSD0003`：BOMs:头部的客户字段。

将该增强通过事务码 `CMOD` 注册到项目中。

![截图 p154](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p155-02.png)

双击出口进入，操作位置如下图。

![截图 p154](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p155-03.png)

点击红框位置继续进入，操作位置如下图。

![截图 p154](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p155-04.png)

在下图中双击对应程序。

![截图 p154](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p156-01.png)

INCLUDE 程序 `ZXCSATOP`。

![截图 p155](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p156-02.png)

继续双击屏幕 `1100`，如下图。

![截图 p155](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p156-03.png)

将自定义结构 `CSCI_STKO` 中的字段放到屏幕里。

![截图 p155](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p156-04.png)

根据输入状态对自定义屏幕中的字段进行输入控制。

![截图 p155](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p157-01.png)

出口函数 `EXIT_SAPLCSDI_004`。

![截图 p156](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p157-02.png)

出口函数 `EXIT_SAPLCSDI_005`。

![截图 p156](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p157-03.png)

### 屏幕增强位置

## 物料BOM 行项目屏幕增强

### 屏幕增强实现

在表 `STPO` 中 include 结构 `CI_STPO`，并加入自定义字段，如下图。

![截图 p157](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p158-01.png)

客户增强 `PCSD0002`：BOMs:条目中的客户字段。

将该增强通过事务码 `CMOD` 注册到项目中。

![截图 p157](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p158-02.png)

双击出口进入，操作位置如下图。

![截图 p157](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p158-03.png)

点击红框位置继续进入，操作位置如下图。

![截图 p158](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p158-04.png)

在下图中双击对应程序。

![截图 p158](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p159-01.png)

INCLUDE 程序 `ZXCSATOP`。

![截图 p158](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p159-02.png)

继续双击屏幕 `1000`，如下图。

![截图 p158](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p159-03.png)

将自定义结构 `CSCI_STPO` 中的字段放到屏幕里。

![截图 p158](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p159-04.png)

根据输入状态对自定义屏幕中的字段进行输入控制。

![截图 p159](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p160-01.png)

出口函数 `EXIT_SAPLCSDI_002`。

![截图 p159](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p160-02.png)

出口函数 `EXIT_SAPLCSDI_003`。

![截图 p159](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/abap-screen-enhancement-v4-p161-01.png)

### 屏幕增强位置
