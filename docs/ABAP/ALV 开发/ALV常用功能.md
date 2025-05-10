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
