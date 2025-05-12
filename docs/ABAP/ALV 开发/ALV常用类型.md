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

<!-- tabs:start -->

<!-- tab:cl_gui_custom_container -->

```abap

```

<!-- tab:cl_gui_docking_container -->

<!-- tab:cl_gui_splitter_container -->

<!-- tab:默认容器 -->

<!-- tabs:end -->

<!-- tabs:end -->

