<!-- tabs:start -->

<!-- tab:ALV按钮文本切换 -->

1. 程序中定义一个全局变量，并初始化全局变量，类型是`smp_dyntxt`。

   ```abap
   data: gs_btn_change type smp_dyntxt.
   
   gs_btn_change = value #(
   	icon_id   = icon_display
   	text      = 'Display'
   	icon_text = 'Display'
   ).
   ```

1. 在GUI状态中定义动态文本的按钮![创建按钮](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250507200645006.png)

   输入字段名称（此处可以是表字段，也可以是程序变量）![输入字段名称](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250507201049354.png)

<!-- tabs:end -->
