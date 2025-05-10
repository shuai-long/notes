## ALV复选框

- 获取对象定义

  <!-- tabs:start -->

  <!-- tab:OO ALV -->

  ```abap
  data lo_alv_grid type ref to cl_gui_alv_grid.
  ```

  <!-- tab:Function ALV -->

  ```abap
  data lo_alv_grid type ref to cl_gui_alv_grid.
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lo_alv_grid.
  ```

  <!-- tabs:end -->

- 实现

  <!-- tabs:start -->

  #### **ALV自带复选框**

  - 获取选择行

    ```abap
    data lt_rows type lvc_t_row.
    lo_alv_grid->get_selected_rows(
      importing
        et_index_rows = lt_rows ).
    ```

  #### **内表定义字段**

  - 检查数据更改

    ```abap
    lo_alv_grid->check_changed_data( ).
    ```

  - 获取过滤掉的行

    <!-- tabs:start -->

    #### **方法一**

    ```abap
    lo_alv_grid->get_filtered_entries(
      importing
        et_filtered_entries = data(lt_filtered_entries)
    ).
    ```

    #### **方法二**

    ```abap
    data: lt_filtered_entries type slis_t_filtered_entries. 
    call function 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
      importing
        et_filtered_entries = lt_filtered_entries
      exceptions
        no_infos            = 1
        program_error       = 2
        others              = 3.
    ```

    <!-- tabs:end -->

  - 全选与取消全选

    <!-- tabs:start -->

    #### **全选**

    ```abap
    "--------------------> 全选
    sort lt_rows.
    loop at gt_alv assigning field-symbol(<fs_alv>).
      read table lt_filtered_entries transporting no fields with key table_line = sy-tabix binary search.
      if sy-subrc ne 0.
        <fs_alv>-checkbox = 'X'.
       else.
        <fs_alv>-checkbox = ' '.
      endif.
    endloop.
    ```

    #### **取消全选**

    ```abap
    "--------------------> 取消全选
    sort lt_rows.
    loop at gt_alv assigning field-symbol(<fs_alv>).
      read table lt_filtered_entries transporting no fields with key table_line = sy-tabix binary search.
      if sy-subrc ne 0.
        <fs_alv>-checkbox = ' '.
       else.
        <fs_alv>-checkbox = 'X'.
      endif.
    endloop.
    ```

    <!-- tabs:end -->

  - 刷新alv数据

    ```abap
    data ls_stable type lvc_s_stbl value 'XX'.
    lo_alv_grid->refresh_table_display( is_stable = ls_stable ).
    ```

  <!-- tabs:end -->

## ALV按钮

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

## ALV颜色设置

![alv常用颜色](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220901165722040.png)

<!-- tabs:start -->

<!-- tab:行颜色 -->

1. 在构成ALV数据的内表中，添加一个CHAR4类型的字段

   ```abap
   types: begin of ty_alv,
            row_color type char4, 
          end of ty_alv.
   data: gt_alv type table of ty_alv.
   ```

2. 在layout中指定颜色字段: 

   `slis_layout_alv`结构字段为`info_fieldname`

   `lvc_s_layo`结构字段为`info_fname`

   ```abap
   gs_layo-info_fname = 'ROW_COLOR'.
   ```

3. 在内表中为颜色赋值即可

   ```abap
   loop at gt_alv assigning field-symbol(<fs_alv>).
   	<fs_alv>-row_color = 'C610'
   endloop.
   ```

<!-- tab:列颜色 -->

在构成ALV字段的FIELDCAT内表字段`emphasize`赋值即可.

```abap
ls_fcat-emphasize = 'C610'.
```

<!-- tab:单元格颜色 -->

1. 在构成ALV数据的内表中，添加一个`lvc_t_scol`类型的字段，结构如下：

   - `fname`: 内表字段名称

   - `color`: 颜色代码，包含以下字段
     - `col`: 颜色
     - `int`: 强化 
     - `inv`: 相反，值范围：1/0 设置前景或者背景

   - `nokeycol`: 覆盖码颜色

   ```abap
   types: begin of ty_alv,
            cell_color type lvc_t_scol, 
          end of ty_alv.
   data: gt_alv type table of ty_alv.
   ```

2. 在layout中指定颜色字段: 

   `slis_layout_alv`结构字段为`coltab_fieldname`

   `lvc_s_layo`结构字段为`ctab_fname`

   ```abap
   gs_layo-ctab_fname = 'CELL_COLOR'.
   ```

3. 在内表中设置字段颜色。

   ```abap
   loop at gt_alv assigning field-symbol(<fs_alv>).
   	<fs_alv>-cell_color = value #( ( fname = 'BUKRS' color = value #( col = '6' int = '1' inv = '0' ) ) ).
   endloop.
   ```

<!-- tabs:end -->
