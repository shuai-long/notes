[TOC]

# ALV 模板 #

## FUNCTION ALV ##

```ABAP
TABLES sscrfields.

TYPES: BEGIN OF ty_alv,
         checkbox TYPE char1,
       END OF ty_alv.
DATA:gt_alv TYPE TABLE OF ty_alv.

**选择屏幕按钮
*SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM frm_init_data.

**字段添加搜索帮助
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bstnk-high.
*  PERFORM frm_f4_help TABLES gt_help_data  USING 'BSTNK' 'P_BSTNK-HIGH'.

**文件搜索帮助
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM frm_f4_help_file CHANGING p_file.

**字段的检查
*AT SELECTION-SCREEN ON s_monat.
*  PERFORM frm_check_data.

AT SELECTION-SCREEN.
  PERFORM frm_excue_button.

START-OF-SELECTION.
  PERFORM frm_get_data.
  PERFORM frm_deal_data.

END-OF-SELECTION.
  PERFORM frm_display_data.

*&---------------------------------------------------------------------*
*& Form frm_init_data
*&---------------------------------------------------------------------*
*& text 初始化选择屏幕
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init_data .
  sscrfields-functxt_01 = VALUE smp_dyntxt(
  icon_id = icon_operation
  icon_text = '维护配置表'
  quickinfo = '维护配置表'
  ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_excue_button
*&---------------------------------------------------------------------*
*& text  响应选择屏幕的按钮功能
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_excue_button .

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
*      SET CURSOR FIELD 'P_PERNR'.   "设置鼠标焦点到字段

    WHEN 'ONLI'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_f4_help
*&---------------------------------------------------------------------*
*& text 给字段添加搜索帮助
*&---------------------------------------------------------------------*
*&      --> GT_HELP_DATA
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_f4_help  TABLES   gt_help_data
                  USING    VALUE(pv_retfield)
                           VALUE(pv_dynprofield).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = pv_retfield     "内表对应的字段
      dynpprog        = sy-repid        " 定义系统变量自动获取程序名称
      dynpnr          = sy-dynnr        "定义系统变量自动获取程序编号
      dynprofield     = pv_dynprofield  " 屏幕接收值得字段
      value_org       = 'S'
    TABLES
      value_tab       = gt_help_data     "传入搜索帮助值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_f4_help_file
*&---------------------------------------------------------------------*
*& text 文件搜索帮助
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_f4_help_file CHANGING o_fname TYPE rlgrap-filename.
  DATA: l_filetab TYPE filetable,
        l_waftab  LIKE LINE OF l_filetab,
        l_rc      TYPE i.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = '打开文件'
      initial_directory       = 'C:/'
    CHANGING
      file_table              = l_filetab
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.
    READ TABLE l_filetab INTO l_waftab INDEX 1.
    o_fname = l_waftab-filename.
    CLEAR: l_filetab,
    l_waftab.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_check_data
*&---------------------------------------------------------------------*
*& text 权限检查
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_data .
*  AUTHORITY-CHECK OBJECT 'K_PCAR_REP'
*  ID 'PRCTR'     FIELD s_prctr-low
*  ID 'ACTVT'     FIELD '03'.
*
*  IF sy-subrc <> 0.
*    CONCATENATE '您没有'  s_prctr-low  '利润中心的权限' INTO DATA(lv_message).
*    MESSAGE lv_message TYPE 'E' DISPLAY LIKE 'S'.
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*& text 取数
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_deal_data
*&---------------------------------------------------------------------*
*& text 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_deal_data .

ENDFORM.


*&---------------------------------------------------------------------*
*& Form frm_display_data
*&---------------------------------------------------------------------*
*& text 数据展示
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_data .
  DATA:lt_fcat       TYPE lvc_t_fcat,
       ls_layout     TYPE lvc_s_layo,
       ls_disvariant LIKE disvariant,
       lt_events     TYPE slis_t_event,
       lt_exclude    TYPE slis_t_extab.

  PERFORM frm_set_layout   CHANGING ls_layout."样式设置
  PERFORM frm_set_fieldcat CHANGING lt_fcat.  "设置FIELDCAT  仅显示
  PERFORM frm_set_disvariant CHANGING ls_disvariant.
  PERFORM frm_alv_display  USING    lt_events
                           CHANGING lt_fcat
                            ls_layout
                            lt_exclude
                            ls_disvariant
                            gt_alv."ALV显示
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_set_layout
*&---------------------------------------------------------------------*
*& text 设置ALV样式
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_layout CHANGING cs_layout TYPE lvc_s_layo.

*<---设置列优化及行选择--->
  cs_layout-cwidth_opt = 'A'.
  cs_layout-sel_mode   = 'A'."A（行与列的选择，无法选择单元格）。"B（单选，不可以多选行，不可以多选单元格）。"C（多选行、列，不可以多选单元格）。"D（多选行、列，还可以多选单元格）。
  cs_layout-zebra      = abap_true.

ENDFORM. " FRM_SET_LAYOUT

*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*& text 设置fieldcat目录
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat CHANGING ct_fcat TYPE lvc_t_fcat.

  DATA: ls_fcat TYPE lvc_s_fcat.

  DEFINE m_fcat.
    CLEAR ls_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-scrtext_m = &2.
    APPEND ls_fcat TO ct_fcat.
  END-OF-DEFINITION.

  CLEAR: ct_fcat.
  m_fcat:'SAKNR'  '科目编号'.
ENDFORM. " FRM_SET_FIELDCAT

*&---------------------------------------------------------------------*
*& Form frm_set_disvariant
*&---------------------------------------------------------------------*
*& text alv布局与选择屏幕挂钩
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_disvariant CHANGING cs_disvariant LIKE disvariant.
*  cs_disvariant-report = 'ZFIB_032'."程序名
*  cs_disvariant-handle = p_ywcj. "选择屏幕变量
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*& text ALV展示
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_alv_display USING    it_events  TYPE slis_t_event
CHANGING ct_fcat    TYPE lvc_t_fcat
  cs_layout  TYPE lvc_s_layo
  ct_exclude TYPE slis_t_extab
  cs_disvariant LIKE disvariant
  ct_tab     TYPE STANDARD TABLE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = cs_layout
      it_fieldcat_lvc          = ct_fcat
      i_callback_pf_status_set = 'SET_PF_STATUS'          "GUI状态设置
      i_callback_user_command  = 'ALV_USER_COMMAND'       "用户状态PAI
*     i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'       "ALV标题内容
*     i_html_height_top        = 14                       "ALV标题高度
*     it_excluding             = ct_exclude
*     it_events                = it_events
*     i_default                = abap_true
*     i_save                   = 'A'
*     is_variant               = cs_disvariant
    TABLES
      t_outtab                 = ct_tab
    EXCEPTIONS
      OTHERS                   = 0.

ENDFORM. " FRM_ALV_DISPLAY

*&---------------------------------------------------------------------*
*& Form set_pf_status
*&---------------------------------------------------------------------*
*& text GUI状态栏设置
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_pf_status USING pt_exclude TYPE kkblo_t_extab.

  DATA:lt_exclude TYPE kkblo_t_extab WITH HEADER LINE.

*  lt_exclude-fcode = 'GZ'. APPEND lt_exclude. "需要排除的按钮
  SET PF-STATUS 'STANDARD' EXCLUDING lt_exclude[].

ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_user_command
*&---------------------------------------------------------------------*
*& text  ALV按钮事件
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  DATA:ref_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_rows TYPE slis_t_filtered_entries.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref_grid.

  CALL METHOD ref_grid->check_changed_data. " 更改


  "得到过滤掉的行
  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
    IMPORTING
      et_filtered_entries = lt_rows
    EXCEPTIONS
      no_infos            = 1
      program_error       = 2
      OTHERS              = 3.
  SORT lt_rows.


  CASE r_ucomm.
    WHEN 'ALL'.
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE checkbox EQ ''.
        READ TABLE lt_rows TRANSPORTING NO FIELDS WITH KEY table_line = sy-tabix BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_alv>-checkbox = 'X'.
        ENDIF.
      ENDLOOP.
    WHEN 'SAL'.
      LOOP AT gt_alv ASSIGNING <fs_alv> WHERE checkbox EQ 'X'.
        READ TABLE lt_rows TRANSPORTING NO FIELDS WITH KEY table_line = sy-tabix BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_alv>-checkbox = ''.
        ENDIF.
      ENDLOOP.

    WHEN '&IC1'.
      READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        CASE rs_selfield-fieldname."字段名
          WHEN 'BELNR'.
        ENDCASE.
      ENDIF.
  ENDCASE.

  CALL METHOD ref_grid->refresh_table_display. " 刷新

ENDFORM.

*&---------------------------------------------------------------------*
*& Form html_top_of_page
*&---------------------------------------------------------------------*
*& text alv抬头标题
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.
  DATA: position TYPE i.

  SEARCH document->html_table FOR document->cursor.
  IF sy-subrc EQ 0.
    position = sy-tabix.

    CALL METHOD document->html_insert
      EXPORTING
        contents = '<div><span style="text-align:center"><h2>多维科目余额表</h2></span></div>'
      CHANGING
        position = position.

  ENDIF.

ENDFORM. "HTML_TOP_OF_PAGE
```

## OO ALV

OO ALV主要通过 CL_GUI_ALV_GRID 这个类来控制 ALV 的显示，以下代码粘贴即用。[参考链接](https://www.cnblogs.com/tangToms/p/11870579.html)

```abap
class lcl_report definition deferred.

data: go_report type ref to lcl_report.

class lcl_report definition.
  public section.
    types: begin of ty_alv,

           end of ty_alv.

    constants: gc_report_title type string value ' '.

    methods: constructor.

    methods:
      call_screen,
      set_data_to_alv,
      create_alv,
      set_fcat,
      set_layout,
      set_handler,
      set_variant.

    methods:
      handle_toolbar for event toolbar of cl_gui_alv_grid
        importing
          e_object
          e_interactive,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing
          e_ucomm,

      handle_double_click for event double_click of cl_gui_alv_grid
        importing
          sender
          e_row
          e_column
          es_row_no.

  private section.
    data: go_alv_grid    type ref to cl_gui_alv_grid,
          gt_alv_data    type table of ty_alv,
          gt_alv_fcat    type lvc_t_fcat,
          gs_alv_layout  type lvc_s_layo,
          gs_alv_variant type disvariant,
          gs_alv_stbl    type lvc_s_stbl.

endclass.


class lcl_report implementation.
  method constructor.

  endmethod.

  method call_screen.
    call screen 9000.
  endmethod.

  method set_data_to_alv.

  endmethod.

  method create_alv.
    if go_alv_grid is initial.

      go_alv_grid = new cl_gui_alv_grid(
        i_parent = cl_gui_container=>default_screen
      ).

      set_fcat( ).
      set_layout( ).
      set_handler( ).
      set_variant( ).

      go_alv_grid->set_table_for_first_display(
        exporting
          is_layout       = gs_alv_layout
          i_save          = 'A'
          is_variant      = gs_alv_variant
        changing
          it_outtab       = gt_alv_data
          it_fieldcatalog = gt_alv_fcat
      ).

    else.
      go_alv_grid->refresh_table_display( is_stable = gs_alv_stbl ).
    endif.
  endmethod.


  method set_fcat.

    define append_fcat.
      append initial line to gt_alv_fcat assigning <fs_fcat>.
      <fs_fcat>-col_pos = lines( gt_alv_fcat ).
      <fs_fcat>-fieldname = &1.
      <fs_fcat>-coltext   = &2.
      <fs_fcat>-reptext   = &2.
      <fs_fcat>-scrtext_l = &2.
      <fs_fcat>-scrtext_m = &2.
      <fs_fcat>-scrtext_s = &2.
      <fs_fcat>-key       = &3.
      <fs_fcat>-inttype   = &4.
      <fs_fcat>-intlen    = &5.
    end-of-definition.

    field-symbols: <fs_fcat> type lvc_s_fcat.
    " 字段 、 描述 、 key、 内部类型、 内部长度


  endmethod.


  method set_layout.

    gs_alv_layout-cwidth_opt = 'A'.
    gs_alv_layout-zebra      = 'X'.

  endmethod.

  method set_handler.

    define set_handler.
      set handler &1 for go_alv_grid.
    end-of-definition.

    set_handler: handle_toolbar,
                 handle_user_command,
                 handle_double_click.

    go_alv_grid->set_toolbar_interactive( ).

    go_alv_grid->register_edit_event(
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

  endmethod.

  method set_variant.
    gs_alv_variant-report = sy-repid.
*    gs_alv_variant-handle = ''
  endmethod.

  method handle_toolbar.
    data: ls_toolbar type stb_button.
    define append_toolbar.
      ls_toolbar-function  = &1.
      ls_toolbar-icon      = &2.
      ls_toolbar-quickinfo = &3.
      ls_toolbar-text      = &4.
      ls_toolbar-disabled  = &5.
      append ls_toolbar to e_object->mt_toolbar.
    end-of-definition.
  endmethod.

  method handle_user_command.
    case e_ucomm.
      when ''.
    endcase.
    go_alv_grid->refresh_table_display( is_stable = gs_alv_stbl ).
  endmethod.

  method handle_double_click.

  endmethod.

endclass.

initialization.

start-of-selection.
  go_report = new lcl_report( ).
  go_report->set_data_to_alv( ).

end-of-selection.
  go_report->call_screen( ).

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.

  set titlebar 'STATUS' with lcl_report=>gc_report_title.
  go_report->create_alv( ).

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.

  case sy-ucomm.
    when 'BACK'.
      set screen 0.
  endcase.

endmodule.
```

ALV 的需要的容器对应类可分为三种:

### CL_GUI_CUSTOM_CONTAINER

```abap
"默认容器, ALV自动占满整个容器；
TABLES:spfli.

CLASS cl_event_handle DEFINITION."事件处理类定义
  PUBLIC SECTION.
    "初始化ALV工具栏对象事件，如增加按钮并设定属性
    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
              e_object
              e_interactive.
    "该事件用于在下ALV工具栏的下拉菜单按钮中增加选项
    METHODS handle_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
            IMPORTING
              e_object
              e_ucomm.
    "ALV工具栏按钮的点击事件
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
              e_ucomm.

    "ALV表格双击事件
    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING
              e_row     "作废
              e_column  "通过e_column-fieldname 获取点击字段名,点击列
              es_row_no. "通过es_row_no-row_id 返回当前行号


    "字段的字段目录HOTSPOT设置为"X",热点单击事件
    METHODS handle_hotspot_click FOR EVENT HOTSPOT_CLICK of cl_gui_alv_grid
            IMPORTING
              E_ROW_ID "作废
              E_COLUMN_ID "通过e_column_id-fieldname 获取点击字段名,点击列
              ES_ROW_NO. "通过es_row_no-row_id 返回当前行号
ENDCLASS.

DATA: gs_toolbar TYPE stb_button.
DATA:t_sflight LIKE TABLE OF sflight WITH HEADER LINE.

CLASS cl_event_handle IMPLEMENTATION."事件处理类实现部分
  METHOD handle_toolbar.
    gs_toolbar-function = 'B_SUM'."按钮的FunctionCode
    gs_toolbar-icon = icon_display."按钮图标,通过Tcode:ICON可以查看
    gs_toolbar-text = '总行数'."按钮标签
    gs_toolbar-butn_type = '0'."定义按钮类型，0为标准按钮，具体取值可参考这里
    APPEND gs_toolbar TO e_object->mt_toolbar."添加按钮到工具栏中

    gs_toolbar-function = 'B_LIST'."按钮的FunctionCode
    gs_toolbar-quickinfo = '自定义下拉菜单按钮'."按钮的冒泡提示
    gs_toolbar-icon = icon_biw_report_view."按钮图标
    gs_toolbar-text = '下拉菜单按钮'."按钮标签
    gs_toolbar-butn_type = '1'."定义按钮类型，1为下拉菜单按钮
    APPEND gs_toolbar TO e_object->mt_toolbar."添加下拉菜单按钮到工具栏中
  ENDMETHOD.

  METHOD handle_menu_button.
    IF e_ucomm = 'B_LIST'."给下拉菜单按钮增加选项，可以多次调用该方法以增加多行
      CALL METHOD e_object->add_function
        EXPORTING
          icon  = icon_display
          fcode = 'B_SUM'"字菜单按钮的FunCode
          text  = '显示ALV总行数'.
    ENDIF.
  ENDMETHOD.
  METHOD handle_user_command.
    DATA: sum TYPE i .
    IF e_ucomm = 'B_SUM'.
      DESCRIBE TABLE t_sflight[] LINES sum.
      MESSAGE i001(00) WITH '当前ALV表格中的数据总行数为：' sum.
    ENDIF.
  ENDMETHOD.

  "双击事件
  METHOD  handle_double_click.
    DATA:temp_message TYPE String.
    READ TABLE t_sflight INTO t_sflight INDEX es_row_no-row_id.
    temp_message = '点击列:' && e_column-fieldname && ',行号：' && es_row_no-row_id.
    MESSAGE i001(00) WITH temp_message.
  ENDMETHOD.

  "字段的字段目录HOTSPOT设置为"X",热点单击事件
  METHOD handle_hotspot_click.
    DATA:temp_message TYPE String.
    temp_message = '点击列:' && e_column_id-fieldname && ',行号:' && es_row_no-row_id.
    MESSAGE i001(00) WITH temp_message.
  ENDMETHOD.
ENDCLASS.

"定义cl_gui_alv_grid 类引用
DATA:alv_grid TYPE REF TO cl_gui_alv_grid.
"定义cl_gui_custom_container 容器
DATA:alv_container TYPE REF TO cl_gui_custom_container.
"alv显示参数
"字段目录HOTSPOT,热点单击
DATA:fieldcat TYPE lvc_t_fcat.
DATA:s_fieldcat TYPE lvc_s_fcat.
DATA:layout TYPE lvc_s_layo.
"事件
DATA: event_handle TYPE REF TO cl_event_handle."定义类对象的引用

SELECTION-SCREEN BEGIN OF SCREEN 101 as SUBSCREEN.
    SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE title.
      SELECT-OPTIONS:
        s_carrid FOR spfli-carrid,
        s_connid FOR spfli-connid.
    SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN END OF SCREEN 101.

*初始化
INITIALIZATION.
  title = '查询条件'.

START-OF-SELECTION.
  CALL SCREEN 100.

MODULE STATUS_0100 OUTPUT.
   SET PF-STATUS 'STA_0100'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE USER_COMMAND_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      "查询数据
       SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE t_sflight
        WHERE carrid IN s_carrid AND connid IN s_connid.

      IF   alv_container  IS  INITIAL.
        CREATE  OBJECT    alv_container"创建ALV容器对象
                 EXPORTING container_name =  'CONTAINER1'.
        CREATE  OBJECT  alv_grid"创建ALV控件
                EXPORTING i_parent  =  alv_container.

        CALL METHOD alv_grid->set_table_for_first_display
          EXPORTING
            i_structure_name = 'sflight'
            is_layout        = layout
            i_save           = 'X'"可以保存变式
          CHANGING
            it_outtab        = t_sflight[]
            it_fieldcatalog  = fieldcat[]."如果fieldcat内表为空，则相当于没有配置，采用默认方式显示
        CREATE OBJECT :event_handle.
        "为ALV按钮注册监听事件
        SET HANDLER :event_handle->handle_toolbar FOR alv_grid,
                     event_handle->handle_menu_button FOR alv_grid,
                     event_handle->handle_user_command FOR alv_grid,
                     event_handle->handle_double_click FOR alv_grid,
                     event_handle->handle_hotspot_click FOR alv_grid.
        "调用此方法才能激活工具栏上增加的自定义按钮
        CALL METHOD alv_grid->set_toolbar_interactive.
      ELSE.
        CALL METHOD alv_grid->refresh_table_display.
      ENDIF.
    WHEN 'BACK' or 'EXIT' or 'QUIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
```



### CL_GUI_DOCKING_CONTAINER

```ABAP
"DOCKING容器, ALV宽度可以直接调整；
TABLES:sflight.

"alv显示设置
DATA:fieldcat TYPE lvc_t_fcat.
DATA:layout TYPE lvc_s_layo.

"cl_gui_alv_grid类引用
DATA:alv_grid TYPE REF TO cl_gui_alv_grid.
DATA:alv_container TYPE REF TO cl_gui_docking_container.

DATA:t_sflight LIKE TABLE OF sflight WITH HEADER LINE.

START-OF-SELECTION.
  CALL SCREEN 100.

MODULE USER_COMMAND_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' or 'QUIT' or 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STA_0100'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OUT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       查询数据，生成docking container对象,显示alv
*----------------------------------------------------------------------*
MODULE OUT_SCREEN OUTPUT.
  "查询数据
  SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE t_sflight WHERE carrid = 'AA'.

  "创建container对象
  CREATE OBJECT alv_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 300."alv宽度
  "创建alv对象
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent = alv_container.
  "调用显示方法
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      i_structure_name = 'sflight'
      is_layout        = layout
      i_save           = 'X'"可以保存变式
    CHANGING
      it_outtab        = t_sflight[]
      it_fieldcatalog  = fieldcat[]."如果fieldcat内表为空，则相当于没有配置，采用默认方式显示
ENDMODULE.                 " OUT_SCREEN  OUTPUT
```



### CL_GUI_SPLITTER_CONTIANER

```ABAP
"SPLITTER容器，可以将屏幕划分区域显示多个ALV；
TABLES:sflight.

"alv显示设置
DATA:fieldcat TYPE lvc_t_fcat.
DATA:layout TYPE lvc_s_layo.
"cl_gui_alv_grid类引用
DATA:alv_grid TYPE REF TO cl_gui_alv_grid.
DATA:alv_grid1 TYPE REF TO cl_gui_alv_grid.
DATA:alv_container TYPE REF TO cl_gui_docking_container.
"spillter container 对象引用
DATA:alv_splitter_container TYPE REF TO cl_gui_splitter_container.
"屏幕容器
DATA:ref_container TYPE REF TO cl_gui_container.

DATA:t_sflight LIKE TABLE OF sflight WITH HEADER LINE.

START-OF-SELECTION.
  CALL SCREEN 100.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' or 'QUIT' or 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STA_0100'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OUT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       查询数据，生成docking container对象,显示alv
*----------------------------------------------------------------------*
MODULE OUT_SCREEN OUTPUT.
  "查询数据
  SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE t_sflight WHERE carrid = 'AA'.

  "创建container对象
  CREATE OBJECT alv_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 1200."alv宽度
  "创建spillter container对象
  CREATE OBJECT alv_splitter_container
    EXPORTING
      parent = alv_container
      rows = 1
      columns = 2. "将父容器分为1行两列，两个容器

  "调用cl_splitter_container对象方法，获取容器
  CALL METHOD alv_splitter_container->get_container
    EXPORTING
      row = 1
      column = 1
    RECEIVING
      container = ref_container.

  "创建左边容器alv对象
  "创建alv对象
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent = ref_container.

  "调用显示方法
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      i_structure_name = 'sflight'
      is_layout        = layout
      i_save           = 'X'"可以保存变式
    CHANGING
      it_outtab        = t_sflight[]
      it_fieldcatalog  = fieldcat[]."如果fieldcat内表为空，则相当于没有配置，采用默认方式显示

   "调用cl_splitter_container对象方法，获取容器
  CALL METHOD alv_splitter_container->get_container
    EXPORTING
      row = 1
      column = 2
    RECEIVING
      container = ref_container.
  "创建右边容器alv对象
  "创建alv对象
  CREATE OBJECT alv_grid1
    EXPORTING
      i_parent = ref_container.

  "调用显示方法
  CALL METHOD alv_grid1->set_table_for_first_display
    EXPORTING
      i_structure_name = 'sflight'
      is_layout        = layout
      i_save           = 'X'"可以保存变式
    CHANGING
      it_outtab        = t_sflight[]
      it_fieldcatalog  = fieldcat[]."如果fieldcat内表为空，则相当于没有配置，采用默认方式显示

ENDMODULE.                 " OUT_SCREEN  OUTPUT
```

## 层级 ALV

```abap
TABLES: ekko,ekpo.

SELECT-OPTIONS s_ebeln FOR ekko-ebeln.

*1、在程序里包含SLIS
TYPE-POOLS:slis.

*2、声明主表和明细表
DATA: BEGIN OF headertab OCCURS 0,
        ebeln LIKE ekko-ebeln,
        bstyp LIKE ekko-bstyp,
        bsart LIKE ekko-bsart,
        statu LIKE ekko-statu,
      END OF headertab.

DATA: BEGIN OF itemtab OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        matnr LIKE ekpo-matnr,
        werks LIKE ekpo-werks,
        menge LIKE ekpo-menge,
        netpr LIKE ekpo-netpr,
        peinh LIKE ekpo-peinh,
        netwr LIKE ekpo-netwr,
      END OF itemtab.


DATA: i_fieldcat TYPE slis_t_fieldcat_alv."alv输出的主表标题

DATA: v_repid LIKE sy-repid."当前程序名

*3、申明主表和明细表的名称变量
DATA: g_tabname_header TYPE slis_tabname,
      g_tabname_item   TYPE slis_tabname.

*4、声明主表和明细表连接关键字变量
DATA: gs_keyinfo TYPE slis_keyinfo_alv.

INITIALIZATION.
  v_repid = sy-repid.

START-OF-SELECTION.

  PERFORM get_data.

END-OF-SELECTION.


*5、获取头表输出表头字段名称

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_repid "当前程序名
      i_internal_tabname     = 'HEADERTAB' "主表名
      i_inclname             = v_repid "当前程序名
      i_bypassing_buffer     = 'X'
      i_buffer_active        = ''
    CHANGING
      ct_fieldcat            = i_fieldcat "ALV输出的主表标题
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1
    sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*6、获取从表输出表头字段名称
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'ITEMTAB'
      i_inclname             = v_repid
      i_bypassing_buffer     = 'X'
      i_buffer_active        = ''
    CHANGING
      ct_fieldcat            = i_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1
    sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*7、设置主从表的关联字段，最多可以设置五个字段关联。
  gs_keyinfo-header01 = 'EBELN'.
  gs_keyinfo-item01 = 'EBELN'.

*8、设置主表和明细表变量对应的内表名称
  g_tabname_header = 'HEADERTAB'."主表
  g_tabname_item = 'ITEMTAB'."明细表

*9、调用层次ALV输出函数
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program = v_repid
      it_fieldcat        = i_fieldcat
      i_save             = 'A'
      i_tabname_header   = g_tabname_header
      i_tabname_item     = g_tabname_item
      is_keyinfo         = gs_keyinfo
      i_bypassing_buffer = 'X'
      i_buffer_active    = ' '
    TABLES
      t_outtab_header    = headertab
      t_outtab_item      = itemtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM get_data.

  SELECT ebeln bstyp bsart statu
  INTO TABLE headertab
  FROM ekko
  WHERE ebeln IN s_ebeln.

  IF NOT headertab[] IS INITIAL.
    SELECT ebeln ebelp matnr werks menge netpr peinh netwr
    INTO TABLE itemtab
    FROM ekpo
    FOR ALL ENTRIES IN headertab
    WHERE ebeln = headertab-ebeln.
  ENDIF.
ENDFORM. "get_data
```



