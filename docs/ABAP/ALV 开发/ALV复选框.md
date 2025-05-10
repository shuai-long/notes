<!-- tabs:start -->

ALV 复选框

- 定义 grid 对象

  ```abap
  data lo_alv_grid type ref to cl_gui_alv_grid.
  ```

- function alv 需要获取grid对象

  ```abap
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lo_alv_grid.
  ```

<!-- tab:ALV自带复选框 -->

- 获取选择行

  ```abap
  data lt_rows type lvc_t_row.
  lo_alv_grid->get_selected_rows(
    importing
      et_index_rows = lt_rows ).
  ```

<!-- tab:内表定义字段 -->

- 检查数据更改

  ```abap
  lo_alv_grid->check_changed_data( ).
  ```

- 获取过滤掉的行

  ```abap
  lo_alv_grid->get_filtered_entries(
    importing
      et_filtered_entries = data(lt_entries)
  ).
  ```

  ```abap
  call function 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
    importing
      et_filtered_entries = lt_rows
    exceptions
      no_infos            = 1
      program_error       = 2
      others              = 3.
  ```

- 全选与取消全选

  ```abap
  "--------------------> 全选
  sort lt_rows.
  loop at gt_alv assigning field-symbol(<fs_alv>).
    read table lt_rows transporting no fields with key table_line = sy-tabix binary search.
    if sy-subrc ne 0.
      <fs_alv>-checkbox = 'X'.
     else.
     	<fs_alv>-checkbox = ' '.
    endif.
  endloop.
  ```

  ```abap
  "--------------------> 取消全选
  sort lt_rows.
  loop at gt_alv assigning field-symbol(<fs_alv>).
    read table lt_rows transporting no fields with key table_line = sy-tabix binary search.
    if sy-subrc ne 0.
      <fs_alv>-checkbox = ' '.
     else.
     	<fs_alv>-checkbox = 'X'.
    endif.
  endloop.
  ```

- 刷新alv数据

  ```abap
  data ls_stable type lvc_s_stbl value 'XX'.
  lo_alv_grid->refresh_table_display( is_stable = ls_stable ).
  ```

<!-- tabs:end -->
