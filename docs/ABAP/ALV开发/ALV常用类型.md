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

[参考链接](https://www.cnblogs.com/tangToms/p/11870579.html)

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

<!-- tabs:start -->
<!-- tab:list tree -->

- 定义变量

  ```abap
  types: begin of ty_tree.
           include type treev_node.
  types:   no_display type char1,
           items      type hashed table of mtreeitm with unique key node_key,
         end of ty_tree.
         
  data: go_dock_container type ref to cl_gui_docking_container,
        go_list_tree      type ref to cl_gui_list_tree,
        gt_tree           type hashed table of ty_tree with unique key node_key,
        gv_node_key       type tv_nodekey value '10',
        gv_item_name      type tv_itmname.
  ```

- 定义添加节点方法并实现

  ```abap
  methods add_nodes_to_tree
    importing
      iv_nodekey type any optional.
  ```

  ```abap
  method add_nodes_to_tree.
  
    data: lt_nodes type treev_ntab,
          lt_items type table of mtreeitm.
  
    loop at gt_tree into data(ls_tree) where relatkey = iv_nodekey and no_display is initial.
      append initial line to lt_nodes assigning field-symbol(<fs_node>).
      move-corresponding ls_tree to <fs_node>.
      loop at ls_tree-items into data(ls_item).
        append initial line to lt_items assigning field-symbol(<fs_item>).
        move-corresponding ls_item to <fs_item>.
      endloop.
    endloop.
  
    go_list_tree->add_nodes_and_items(
      exporting
        node_table                     = lt_nodes
        item_table                     = lt_items
        item_table_structure_name      = 'MTREEITM'
      exceptions
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6 ).
  
    if iv_nodekey is initial.
      go_list_tree->expand_root_nodes(
        exporting
          level_count         = 1
          expand_subtree      = 'X'
        exceptions
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          others              = 4 ).
    endif.
  
  endmethod.
  ```

- 定义事件并实现

  <!-- tabs:start -->

  <!-- tab:展开节点 -->

  ```abap
  methods handle_expand_no_children
    for event expand_no_children of cl_gui_list_tree
    importing
      node_key.
  ```

  ```abap
  method handle_expand_no_children.
  
    gv_node_key = node_key.
    add_nodes_to_tree( gv_node_key ).
  
  endmethod.
  ```

  <!-- tab:node双击事件 -->

  ```abap
  methods
    handle_node_double_click
      for event node_double_click of cl_gui_list_tree
      importing
        node_key.
  ```

  ```abap
  method handle_node_double_click.
  
    gv_node_key = node_key.
  
  endmethod.
  ```

  <!-- tab:item双击事件 -->

  ```abap
  methods
    handle_item_double_click
      for event item_double_click  of cl_gui_list_tree
      importing
        node_key
        item_name.
  ```

  ```abap
    method handle_item_double_click.
  
      gv_node_key = node_key.
  
    endmethod.
  ```

  <!-- tabs:end -->

- 实例化对象并注册事件

  ```abap
  if go_dock_container is initial.
    go_dock_container = new #(
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 200
      side      = cl_gui_docking_container=>dock_at_left ).
  
    go_list_tree = new cl_gui_list_tree(
      parent              = go_dock_container
      node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
      item_selection      = 'X'
      with_headers        = ' ' ).
  
    data: lt_events type cntl_simple_events.
    lt_events = value #( appl_event = 'X' ( eventid = cl_gui_list_tree=>eventid_node_double_click )
                                          ( eventid = cl_gui_list_tree=>eventid_item_double_click )
                                          ( eventid = cl_gui_list_tree=>eventid_expand_no_children )
                                          ( eventid = cl_gui_list_tree=>eventid_link_click )
                                          ( eventid = cl_gui_list_tree=>eventid_button_click )
                                          ( eventid = cl_gui_list_tree=>eventid_checkbox_change )
                                          ( eventid = cl_gui_list_tree=>eventid_node_context_menu_req )
                                          ( eventid = cl_gui_list_tree=>eventid_item_context_menu_req ) ).
  	
  	go_list_tree->set_registered_events( lt_events ).
    set handler handle_node_double_click for go_list_tree.
    set handler handle_item_double_click for go_list_tree.
    set handler handle_expand_no_children for go_list_tree.
  
    add_nodes_to_tree( ).
    
  else.
    cl_gui_cfw=>flush( ).
  endif.
  ```

<!-- tabs:end -->

## 弹出式ALV

<!-- tabs:start -->

<!-- tab:cl_salv_table -->

- 获取ALV对象

  ```abap
  cl_salv_table=>factory(
    importing
      r_salv_table = data(lo_alv)
    changing
      t_table      = lt_email_log "需要展示的内表
  ).
  ```

- 设置展示位置

  ```abap
  lo_alv->set_screen_popup(
    start_column = 30
    end_column   = 100
    start_line   = 5
    end_line     = 20 ).
  ```

- 设置其他属性

  <!-- tabs:start -->

  <!-- tab:设置fieldcat -->

  ```abap
  data(lo_cols) = lo_alv->get_columns( ).
  " 设置自动列宽
  lo_cols->set_optimize( 'X' ).
  " 设置某个字段不可见
  data(lo_col)  = lo_cols->get_column( 'ZZDJD' ).
  lo_col->set_visible( ).
  lo_col->set_technical( 'X' ).
  ```

  <!-- tabs:end -->

- 展示ALV

  ```abap
  lo_alv->display( ).
  ```

<!-- tab:弹框选择 -->

```abap
data: lt_objid    like table of hrp1000 with header line,
      ls_selfield type slis_selfield,
      lv_exit     type char1.

call function 'REUSE_ALV_POPUP_TO_SELECT'
  exporting
    i_title              = '选择导出单位'
    i_zebra              = 'X'
    i_checkbox_fieldname = 'CHECKBOX'
    i_tabname            = space
    i_structure_name     = 'HRP1000'
  importing
    es_selfield          = ls_selfield
    e_exit               = lv_exit
  tables
    t_outtab             = lt_objid[]
  exceptions
    program_error        = 1
    others               = 2.

"--------------------> 用户取消选择
if lv_exit eq 'X'.

endif.
```

<!-- tabs:end -->

## 复合ALV

ALV可合并单元格展示，新建类`zcl_gui_alv_grid_merge`继承`cl_gui_alv_grid`。[参考链接](https://tricktresor.de/blog/zellen-verbinden/)

<!-- tabs:start -->

<!-- tab:新增方法定义 -->

```abap
methods z_set_merge_horiz
  importing
    row           type i
  changing
    tab_col_merge type lvc_t_co01 .
    
methods z_set_merge_vert
  importing
    row           type i
  changing
    tab_col_merge type lvc_t_co01 .
    
methods z_display .

methods z_set_cell_style
  importing
    row    type i optional
    col    type i optional
    style  type lvc_style
    style2 type lvc_style optional .
    
methods z_set_fixed_col_row
  importing
    col type i
    row type i .
    
methods z_init_cell_styles .

```

<!-- tab:新增方法实现 -->

<!-- tabs:start -->

<!-- tab:z_set_merge_horiz -->

```abap
method z_set_merge_horiz.

* ROW - Zeile deren Spalten zusammengef�hrt werden sollen
* tab_col_merge - Spalten, die zusammengef�hrt werden sollen
  field-symbols <fs_cols> type lvc_s_co01.
  field-symbols <fs_data> type lvc_s_data.
  data outputlen type i.

  sort tab_col_merge.
* Die Spalten, die zusammengef�hrt werden sollen
  loop at tab_col_merge assigning <fs_cols>.
* ein paar Pr�fungen
    if <fs_cols>-col_id    le 0.                continue. endif.
    if <fs_cols>-outputlen le <fs_cols>-col_id. continue. endif.
    outputlen = <fs_cols>-outputlen - <fs_cols>-col_id.
    loop at mt_data assigning <fs_data>
         where row_pos = row  and
               ( col_pos between <fs_cols>-col_id and
                                 <fs_cols>-outputlen ).
* Setze wie weit soll gemerged werden Von Spalte in L�nge
* und zwar wird bei der 1 Spalte angefangen
      if <fs_data>-col_pos = <fs_cols>-col_id.
        <fs_data>-mergehoriz = outputlen.
* bei allen anderen, die zusammangeh�ren
* muss der Wert raus, da er aus der 1. Spalte kommt
* und das mergekennzeichen muss auch weg !
      else.
        clear <fs_data>-mergehoriz.
        clear <fs_data>-value.
      endif.
    endloop.

  endloop.

endmethod.
```

<!-- tab:z_set_merge_vert -->

```abap
method z_set_merge_vert.

* ROW - Zeile deren Spalten zusammengef�hrt werden sollen
* tab_col_merge - Spalten, die zusammengef�hrt werden sollen
  field-symbols <fs_cols> type lvc_s_co01.
  field-symbols <fs_data> type lvc_s_data.
  data outputlen type i.

  sort tab_col_merge.
* Die Spalten, die zusammengef�hrt werden sollen
  loop at tab_col_merge assigning <fs_cols>.
* ein paar Pr�fungen
    if <fs_cols>-col_id    le 0.                continue. endif.
    if <fs_cols>-outputlen le <fs_cols>-col_id. continue. endif.
    outputlen = <fs_cols>-outputlen - <fs_cols>-col_id.
    loop at mt_data assigning <fs_data>
         where row_pos = row  and
               ( col_pos between <fs_cols>-col_id and
                                 <fs_cols>-outputlen ).
* Setze wie weit soll gemerged werden Von Spalte in L�nge
* und zwar wird bei der 1 Spalte angefangen
      if <fs_data>-col_pos = <fs_cols>-col_id.
        <fs_data>-mergevert = outputlen.
* bei allen anderen, die zusammangeh�ren
* muss der Wert raus, da er aus der 1. Spalte kommt
* und das mergekennzeichen muss auch weg !
      else.
        clear <fs_data>-mergevert.
        clear <fs_data>-value.
      endif.
    endloop.

  endloop.

endmethod.
```

<!-- tab:z_display -->

```abap
method z_display.

  data lv_stable type lvc_s_stbl.
  data lv_soft   type c.

**** Prepare refresh
*  lv_stable-row = 'X'.
*  lv_stable-col = 'X'.
*  lv_soft       = 'X'.
*
**** Refresh table because Z_SET_CELL_STYLE adds style-values
**** Refresh initializes mt_data
*  CALL METHOD refresh_table_display
*    EXPORTING
*      is_stable      = lv_stable
*      i_soft_refresh = lv_soft
*    EXCEPTIONS
*      OTHERS         = 1.

* Jetzt noch  �bertragen der ge�nderten Daten
  call method me->set_data_table
    changing
      data_table = mt_data[].

  call method set_auto_redraw
    exporting
      enable = 1.

endmethod.
```

<!-- tab:z_set_cell_style -->

```abap
method z_set_cell_style.

  field-symbols <fs_data> type lvc_s_data.
  if row is initial.
    if col is initial.
* Beides leer -> nichts zu tun.
      exit.
    else.
* Nur Spalte setze komplette Spalte
      loop at mt_data assigning <fs_data>
            where col_pos = col.
        <fs_data>-style  = <fs_data>-style + style.
        <fs_data>-style2 = <fs_data>-style2 + style2.
      endloop.
    endif.
  else.
    if col is initial.
* Nur Zeile eingegeben -> komplette Zeile setzen
      loop at mt_data assigning <fs_data>
            where row_pos = row.
        <fs_data>-style  = <fs_data>-style + style.
        <fs_data>-style2 = <fs_data>-style2 + style2.
      endloop.
    else.
      read table mt_data assigning <fs_data>
          with key row_pos = row
                   col_pos = col.
      if sy-subrc eq 0.
        <fs_data>-style  = <fs_data>-style + style.
        <fs_data>-style2 = <fs_data>-style2 + style2.
      else.
        exit.
      endif.
    endif.
  endif.

endmethod.
```

<!-- tab:z_set_fixed_col_row -->

```abap
method z_set_fixed_col_row.

  me->set_fixed_cols( col ).
  me->set_fixed_rows( row ).

endmethod.
```

<!-- tab:z_init_cell_styles -->

```abap
method z_init_cell_styles.

  field-symbols <fs_data> type lvc_s_data.
* Nur Spalte setze komplette Spalte
  loop at mt_data assigning <fs_data>.
    <fs_data>-style = 0.
  endloop.

endmethod.
```

<!-- tabs:end -->

<!-- tab:DEMO -->

```abap
*&---------------------------------------------------------------------*
*& (c) Edwin Leippi Software-Entwicklung                               *
*& Email : info@leippi.de                                              *
*& Datum : 01.03.2008                                                  *
*&                                                                     *
*& Der Autor �bernimmt keine Haftung f�r Sch�den,                      *
*& die durch den Einsatz dieses Programmes entstehen k�nnen            *
*&---------------------------------------------------------------------*
*& http://www.tricktresor.de
*&---------------------------------------------------------------------*
report  zz_alv_merge_cells.

* *** Allgemeines  ****************************************************
include <cl_alv_control>.
include <icon>.
data retc    type sy-subrc.                  .
data ok_code type sy-ucomm.
data it_grp  type lvc_t_sgrp.
data it_fil  type lvc_t_filt.
data wa_fil  type lvc_s_filt.

* **** ALV_GRID    ****************************************************
types: begin of t_check_styles,
         field01(20),
         field02(20),
         field03(20),
         field04(20),
         field05(20),
         field06(20),
         field07(20),
         field08(20),
         field09(20),
         field10(20),
         field11(20),
         field12(20).
types end of t_check_styles.

data it_styles type  t_check_styles occurs 0.
data wa_styles type t_check_styles.
field-symbols <fs_styles>        type t_check_styles.
data :          lt_fieldcatalog  type lvc_t_fcat.
data :          ls_fieldcatalog  type lvc_t_fcat.
data :          wa_cat           type lvc_s_fcat.
data : fieldname(40).
data : fieldnr(2) type n.

field-symbols  <fs_cat> type lvc_s_fcat.

data  lt_iinfo type lvc_t_info.
data  wa_iinfo type lvc_s_info.
data  lt_idata type lvc_t_data.
data  wa_idata type lvc_s_data.

data it_col_merge        type lvc_t_co01.
data wa_col_merge        type lvc_s_co01.
data: g_container        type scrfname value 'CU_CON'.
data: g_custom_container type ref to cl_gui_custom_container.
data  g_alv_grid         type ref to zcl_gui_alv_grid_merge.
class cl_gui_cfw definition load.

data: x_save,                     "for Parameter I_SAVE
      gs_variant type disvariant. "for parameter IS_VARIANT
data gs_layout type lvc_s_layo.   " Layout
data wa_style  type lvc_s_styl.

* **** ALV_GRID    ****************************************************
start-of-selection.
  call screen 0200.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0200 output.

** Status und Titel setzen
  set pf-status '0200'.
  set titlebar '001'.

** Objekte instanzieren und zuordnen: Grid
  if g_custom_container is initial.

    create object g_custom_container
      exporting
        container_name = g_container.

    create object g_alv_grid
      exporting
        i_parent = g_custom_container.

    gs_layout-stylefname = 'CELL'.
    gs_layout-no_headers = 'X'.
    gs_layout-cwidth_opt = ' '.
    gs_layout-no_toolbar = 'X'.

** Feldkatalog erzeugen
    refresh lt_fieldcatalog.

    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
        i_internal_tabname     = 'IT_STYLES'
      changing
        ct_fieldcat            = lt_fieldcatalog
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    refresh lt_fieldcatalog.
    refresh lt_iinfo.

    do 12 times.
      clear wa_cat.
      fieldnr = sy-index.
      wa_cat-col_pos = sy-index.
      concatenate 'FIELD' fieldnr into fieldname.
      wa_cat-fieldname = fieldname.
      wa_cat-tabname   = '1'.
      wa_cat-datatype  = 'CHAR'.
      wa_cat-inttype   = 'C'.
      wa_cat-intlen    = 20.
      if sy-index > 1.
        wa_cat-outputlen    = 6.
      else.
        wa_cat-outputlen    = 20.
      endif.
      wa_cat-reptext   = fieldname.
      wa_cat-scrtext_l = fieldname.
      wa_cat-scrtext_m = fieldname.
      wa_cat-scrtext_s = fieldname.
      wa_cat-scrtext_l = fieldname.
      append wa_cat to lt_fieldcatalog.
    enddo.

* 1 Zeile
    clear wa_styles.
    wa_styles-field01 = 'TRICKTRESOR'.
    wa_styles-field03 = 'F'.
    wa_styles-field04 = 'P'.
    wa_styles-field09 = 'M'.
    wa_styles-field10 = 'K'.
    append wa_styles to it_styles.
* 2 Zeile
    clear wa_styles.
    wa_styles-field03 = 'HQ'.
    wa_styles-field04 = 'HC'.
    wa_styles-field08 = 'HW'.
    wa_styles-field09 = 'HC'.
    wa_styles-field10 = 'HC'.
    wa_styles-field12 = 'HW'.
    append wa_styles to it_styles.
* 3-Zeile
    clear wa_styles.
    wa_styles-field01 = 'Bezeichnung'.
    wa_styles-field02 = 'Radius'.
    wa_styles-field03 = 'WPX 12'.
    wa_styles-field04 = 'WAP 25'.
    wa_styles-field05 = 'WAP 35'.
    wa_styles-field06 = 'WTP 35'.
    wa_styles-field07 = 'WXP 45'.
    wa_styles-field08 = 'WPM'.
    wa_styles-field09 = 'WXM 35'.
    wa_styles-field10 = 'WAK 15'.
    wa_styles-field11 = 'WAK 25'.
    wa_styles-field12 = 'WKM'.
    append wa_styles to it_styles.

* 4..Zeile
    clear wa_styles.
    wa_styles-field01 = 'SPMW 060304 T - A 27'.
    wa_styles-field02 = '0.54'.
    wa_styles-field03 = icon_led_green.
    wa_styles-field04 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field08 = icon_led_yellow.
    append wa_styles to it_styles.

    clear wa_styles.
    wa_styles-field01 = 'SPMW 060304 - A 57'.
    wa_styles-field02 = '0.43'.
    wa_styles-field03 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field08 = icon_led_yellow.
    wa_styles-field10 = icon_led_yellow.
    wa_styles-field11 = icon_led_red.
    wa_styles-field12 = icon_led_yellow.
    append wa_styles to it_styles.

    clear wa_styles.
    wa_styles-field01 = 'SPMW 060304 - D 51'.
    wa_styles-field02 = '0.76'.
    wa_styles-field04 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field06 = icon_led_red.
    wa_styles-field07 = icon_led_red.
    append wa_styles to it_styles.

    clear wa_styles.
    wa_styles-field01 = 'SPMW 060304 - F 55'.
    wa_styles-field02 = '0.44'.
    wa_styles-field03 = icon_led_red.
    wa_styles-field05 = icon_led_green.
    wa_styles-field06 = icon_led_yellow.
    wa_styles-field07 = icon_led_red.
    wa_styles-field09 = icon_led_yellow.
    wa_styles-field10 = icon_led_green.
    wa_styles-field11 = icon_led_yellow.
    wa_styles-field12 = icon_led_yellow.
    append wa_styles to it_styles.

*
    call method g_alv_grid->set_table_for_first_display
      exporting
        is_variant      = gs_variant
        i_save          = x_save
        is_layout       = gs_layout
      changing
        it_fieldcatalog = lt_fieldcatalog
        it_outtab       = it_styles.

    refresh it_col_merge.

*** DEMO vertikal verbinden
    wa_col_merge-col_id    = 1.
    wa_col_merge-outputlen = 2.
    append wa_col_merge to it_col_merge.

    call method g_alv_grid->z_set_merge_vert
      exporting
        row           = 1
      changing
        tab_col_merge = it_col_merge.
    wa_style-style     = alv_style_font_bold
                       + alv_style_align_center_center
                       + alv_style_color_key.

    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 1
        col   = 1
        style = wa_style-style.

*** VERTIKAL verbinden
    clear it_col_merge.

    wa_col_merge-col_id    = 4.
    wa_col_merge-outputlen = 8.
    append wa_col_merge to it_col_merge.

    wa_col_merge-col_id    = 10.
    wa_col_merge-outputlen = 12.
    append wa_col_merge to it_col_merge.

    call method g_alv_grid->z_set_merge_horiz
      exporting
        row           = 1
      changing
        tab_col_merge = it_col_merge.

    wa_style-style     = alv_style_font_bold.

    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 1
        col   = 3
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 1
        col   = 4
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 1
        col   = 9
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 1
        col   = 10
        style = wa_style-style.

    refresh it_col_merge.

    wa_col_merge-col_id    = 4.
    wa_col_merge-outputlen = 7.
    append wa_col_merge to it_col_merge.

    wa_col_merge-col_id    = 10.
    wa_col_merge-outputlen = 2.
    append wa_col_merge to it_col_merge.


    call method g_alv_grid->z_set_merge_horiz
      exporting
        row           = 2
      changing
        tab_col_merge = it_col_merge.

    wa_style-style     = alv_style_color_group +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 3
        style = wa_style-style.

    wa_style-style     = alv_style_color_heading +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 4
        style = wa_style-style.


    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 5
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 6
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 7
        style = wa_style-style.
    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 8
        style = wa_style-style.

    wa_style-style     = alv_style_color_total +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 9
        style = wa_style-style.


    wa_style-style     = alv_style_color_negative +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 10
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 11
        style = wa_style-style.


    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 12
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 13
        style = wa_style-style.

    wa_style-style     = alv_style_color_positive +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 14
        style = wa_style-style.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 15
        style = wa_style-style.

    wa_style-style     = alv_style_color_int_background +
                         alv_style_align_center_center.

    call method g_alv_grid->z_set_cell_style
      exporting
        col   = 16
        style = wa_style-style.

    wa_style-style     = alv_style_color_positive +
                         alv_style_align_center_center +
                         alv_style_font_italic.


    call method g_alv_grid->z_set_cell_style
      exporting
        row   = 4
        col   = 2
        style = wa_style-style.

    g_alv_grid->z_set_fixed_col_row(
      exporting
        col = 3
        row = 3 ).

    g_alv_grid->z_display( ).

  endif.
endmodule.                    "status_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module user_command_0200 input.
*   to react on oi_custom_events:
  cl_gui_cfw=>dispatch( ).

  case ok_code.
    when 'BACK'.
      set screen 0. leave screen.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
```

<!-- tabs:end -->
