## 常规ALV

<!-- tabs:start -->

<!-- tab:FUNCTION ALV -->

- 使用变量定义

  ```abap
  data: gt_fcat       type lvc_t_fcat,
        gs_layout     type lvc_s_layo,
        gt_sort       type lvc_t_sort,
        gs_disvariant like disvariant,
        gt_events     type slis_t_event,
        gt_exclude    type slis_t_extab.
  ```

- 调用方法：`REUSE_ALV_GRID_DISPLAY_LVC`

  ```abap
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program       = sy-repid
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fcat
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
  *   i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
  *   i_html_height_top        = 14
  *   it_excluding             = gt_exclude
  *   it_events                = gt_events
  *   i_default                = abap_true
  *   i_save                   = 'A'
  *   is_variant               = gs_disvariant
    tables
      t_outtab                 = gt_alv
    exceptions
      others                   = 0.
  ```

- callback form 定义

  <!-- tabs:start -->

  <!-- tab:PF-STATUS -->

  ```abap
  form set_pf_status using pt_exclude type kkblo_t_extab.
  
    data:lt_exclude type kkblo_t_extab with header line.
     
     "--------------------> 需要排除的按钮
     "lt_exclude[] = value #( ( fcode = 'GZ' ) ).
   
    set pf-status 'STANDARD' excluding lt_exclude[].
  
  endform.
  ```

  <!-- tab:USER_COMMAND -->

  ```abap
  form alv_user_command using r_ucomm like sy-ucomm
                              rs_selfield type slis_selfield.
  
    data: lo_grid             type ref to cl_gui_alv_grid,
          lt_filtered_entries type lvc_t_fidx,
          ls_stbl             type lvc_s_stbl value 'XX'.
  
    call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      importing
        e_grid = lo_grid.
  
    "--------------------> 检查更改
    lo_grid->check_changed_data( ).
    
    "--------------------> 获取过滤行
    lo_grid->get_filtered_entries(
      importing
        et_filtered_entries = lt_filtered_entries
    ).
  
    "--------------------> 刷新展示 
    lo_grid->refresh_table_display( is_stable = ls_stbl ).
  
  endform.
  ```
  
  <!-- tab:HTML_TOP_OF_PAGE -->
  
  ```abap
  form html_top_of_page using document type ref to cl_dd_document.
    data: lv_position type i.
  
    search document->html_table for document->cursor.
    if sy-subrc eq 0.
      lv_position = sy-tabix.
  
      call method document->html_insert
        exporting
          contents = '<div><span style="text-align:center"><h2>多维科目余额表</h2></span></div>'
        changing
          position = lv_position.
  
    endif.
  endform.
  ```
  
  <!-- tabs:end -->

<!-- tab:OO ALV -->

- 变量定义

  ```abap
  data: go_alv_grid type ref to cl_gui_alv_grid,
        gt_fcat     type lvc_t_fcat,
        gt_sort     type lvc_t_sort,
        gt_filter   type lvc_t_filt,
        gt_f4       type lvc_t_f4,
        gs_stbl     type lvc_s_stbl value 'XX',
        gs_layo     type lvc_s_layo,
        gs_variant  type disvariant.
  ```

- 初始化对象

  <!-- tabs:start -->

  <!-- tab:cl_gui_custom_container -->

  - 定义

    ```abap
    data: go_alv_container type ref to cl_gui_custom_container.
    ```

  - 初始化

    ```abap
    go_alv_container = new #( 'ALV_DATA' ).
    go_alv_grid = new #( go_alv_container ).
    ```

  <!-- tab:cl_gui_docking_container -->

  - 定义

    ```abap
    data: go_alv_container type ref to cl_gui_docking_container.
    ```

  - 初始化

    ```abap
    go_alv_container = new #(
      repid = sy-repid
      dynnr = gc_screen_list-screen_0100
      side  = cl_gui_docking_container=>dock_at_bottom
      ratio = 85
    ).
    go_alv_grid = new #( go_alv_container ).
    ```

  <!-- tab:cl_gui_splitter_container -->

  - 定义

    ```abap
    data: go_alv_container       type ref to cl_gui_splitter_container,
    			go_alv_container_left  type ref to cl_gui_docking_container,
    			go_alv_container_right type ref to cl_gui_docking_container.
    ```

  - 初始化

    ```abap
    go_alv_container = new cl_gui_splitter_container( parent = cl_gui_container=>default_screen rows = 1 columns = 2 ).
    go_alv_container->set_column_width( id = 1 width = 20 ).
    go_alv_container_left  = go_alv_container->get_container( row = 1 column = 1 ).
    go_alv_container_right = go_alv_container->get_container( row = 1 column = 2 ).
    go_alv_grid = new #( go_alv_container_right ).
    ```

  <!-- tab:默认容器 -->

  - 初始化

    ```abap
    go_alv_grid = new #( cl_gui_container=>default_screen ).
    ```

  <!-- tabs:end -->

- 展示

  ```abap
  go_alv_grid->set_table_for_first_display(
    exporting
      is_variant                    = gs_variant
      i_save                        = 'A'
      is_layout                     = gs_layo
    changing
      it_outtab                     = <fs_outtab>
      it_fieldcatalog               = gt_fcat
      it_sort                       = gt_sort
      it_filter                     = gt_filter
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4 ).
  ```

- 刷新

  ```abap
  go_alv_grid->refresh_table_display( is_stable = gs_stbl ).
  ```

<!-- tabs:end -->

## 层级ALV

- 定义输出抬头表和行项目表

  ```abap
  types: begin of ty_alv_header,
           vbeln type vbak-vbeln,
         end of ty_alv_header.
  
  types: begin of ty_alv_item,
           vbeln type vbak-vbeln,
         end of ty_alv_item.
  
  data: gt_alv_header type table of ty_alv_header,
        gt_alv_item   type table of ty_alv_item.
  ```

- 定义函数调用参数并赋值

  ```abap
  data: gv_tabname_header type slis_tabname,
        gv_tabname_item   type slis_tabname,
        gt_fieldcat       type slis_t_fieldcat_alv, 
        gs_keyinfo        type slis_keyinfo_alv.
  ```

  - 设置主表及明细表名称

    ```abap
    gv_tabname_header = 'GT_ALV_HEADER'. "主表
    gv_tabname_item = 'GT_ALV_ITEM'.     "明细表
    ```

  - 设置主表与行项目表关联字段，最多可以设置五个

    ```abap
    gs_keyinfo = value #( header01 = 'VBELN'
                          item01   = 'VBELN' ).
    ```

- 设置fieldcat并调用函数输出

  ```abap
  call function 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    exporting
      i_callback_program = sy-repid
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
      i_tabname_header   = gv_tabname_header
      i_tabname_item     = gv_tabname_item
      is_keyinfo         = gs_keyinfo
      i_bypassing_buffer = 'X'
      i_buffer_active    = ' '
    tables
      t_outtab_header    = gt_alv_header
      t_outtab_item      = gt_alv_item
    exceptions
      program_error      = 1
      others             = 2.
  ```

## Tree ALV
