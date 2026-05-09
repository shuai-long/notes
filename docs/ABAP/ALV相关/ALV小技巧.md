## 去除无意义0

- 在报表中定义字段 `unit type unit`

- 在需要去除零的列写

  ```abap
  <fs_fcat>-qfieldname = 'UNIT'.
  <fs_fcat>-quantity = 'ST'.
  <fs_fcat>-no_zero = abap_true.
  ```

## ALV字段添加格式

1. 添加inclde程序，`include <cl_alv_control>.`

2. 添加下划线

   ```abap
   <fs_fcat>-style = alv_style_font_underlined.
   ```

