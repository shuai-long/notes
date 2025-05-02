[toc]

# 下拉框设置 #

```abap

REPORT ztestxue044 NO STANDARD PAGE HEADING
                LINE-SIZE 170.
TABLES mara.

TYPES:BEGIN OF typ_data,        "输出内表的结构类型
        matnr TYPE makt-matnr,
        matkl TYPE mara-matkl,
        maktx TYPE makt-maktx,
        spras TYPE makt-spras,
      END OF typ_data.


TYPES:BEGIN OF typ_alv,
        matnr      TYPE makt-matnr,
        matkl      TYPE mara-matkl,
        maktx      TYPE makt-maktx,
        spras      TYPE makt-spras,
        dd_handle  TYPE int4,
        dd_handle1 TYPE int4,    "这个字段用于结果内表关联下拉列表
      END OF typ_alv.

*    全局变量
DATA i_result   TYPE TABLE OF typ_data WITH HEADER LINE.

* ALV 工作区、变量与内表
TYPE-POOLS: slis.

DATA:i_fieldcat  TYPE lvc_t_fcat,     "输出的内表字段
     wa_fieldcat TYPE lvc_s_fcat,
     i_events    TYPE slis_t_event,   "事件存储内表
     wa_events   TYPE slis_alv_event,
     i_ddval     TYPE lvc_t_drop,     "存储下拉列表的数据
     wa_ddval    TYPE lvc_s_drop,
     i_alv       TYPE TABLE OF typ_alv, "ALV
     wa_alv      TYPE typ_alv.
DATA:wa_sort    TYPE slis_t_sortinfo_alv,
     wa_variant TYPE disvariant,
     v_repid    TYPE sy-repid VALUE sy-repid.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
*@  SELECTION-SCREEN
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SELECT-OPTIONS s_matnr FOR mara-matnr.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
*@  Executing program's events
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
INITIALIZATION.

START-OF-SELECTION.
  PERFORM f_query_data.
  PERFORM f_create_dropdown_list.

END-OF-SELECTION.
  PERFORM f_create_fieldcat.
  PERFORM f_create_event_exits.
  PERFORM f_display_alv.

*@---------------------------------------------------------------------*
*@      Form  f_query_data
*@---------------------------------------------------------------------*
*       从数据库表中查询数据
*----------------------------------------------------------------------*
FORM f_query_data .
  SELECT mara~matnr
         mara~matkl
         makt~maktx
         makt~spras
      INTO TABLE i_result UP TO 3 ROWS
      FROM mara INNER JOIN makt ON mara~matnr = makt~matnr
      WHERE mara~matnr IN s_matnr AND
            makt~spras EQ '1'.

  LOOP AT i_result.
    MOVE-CORRESPONDING i_result TO wa_alv.
    APPEND wa_alv TO i_alv.
  ENDLOOP.
ENDFORM.                    " f_query_data
*@---------------------------------------------------------------------*
*@      Form  f_create_fieldcat
*@---------------------------------------------------------------------*
*       初始化输出内表的格式与字段
*----------------------------------------------------------------------*
FORM f_create_fieldcat .
  DEFINE m_fieldcat.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-ref_field = &2.
    wa_fieldcat-ref_table = &3.
    IF &1 = 'MATKL'.
      wa_fieldcat-inttype   = 'C'.
      wa_fieldcat-intlen    = '9'.
      wa_fieldcat-edit       = 'X'.
      wa_fieldcat-outputlen = '12'.
      wa_fieldcat-dd_outlen = '12'.
      wa_fieldcat-drdn_field = 'DD_HANDLE'.   "设置下拉菜单
    ENDIF.

     IF &1 = 'MAKTX'.
      wa_fieldcat-inttype   = 'C'.
      wa_fieldcat-intlen    = '9'.
      wa_fieldcat-edit       = 'X'.
      wa_fieldcat-outputlen = '12'.
      wa_fieldcat-dd_outlen = '12'.
      wa_fieldcat-drdn_field = 'DD_HANDLE1'.   "设置下拉菜单
    ENDIF.

    APPEND wa_fieldcat TO i_fieldcat.
  END-OF-DEFINITION.

  m_fieldcat 'MATNR' 'MATNR' 'MAKT'.
  m_fieldcat 'MATKL' 'MATKL' 'MARA'.
  m_fieldcat 'MAKTX' 'MAKTX' 'MAKT'.
ENDFORM.                    " f_create_fieldcat
*@---------------------------------------------------------------------*
*@      Form  f_create_dropdown_list
*@---------------------------------------------------------------------*
*       创建下拉菜单内表
*----------------------------------------------------------------------*
FORM f_create_dropdown_list .
  DATA: l_matkl TYPE mara-matkl,
        l_count TYPE i.

  LOOP AT i_alv INTO wa_alv.
    ADD 1 TO l_count.
    CLEAR wa_ddval.
    wa_ddval-handle = l_count.
    wa_ddval-value  = '         '.
    APPEND wa_ddval TO i_ddval.

    SELECT matkl
        INTO l_matkl
        FROM mara
        WHERE matnr = wa_alv-matnr.

      CLEAR wa_ddval.
      wa_ddval-handle = l_count.
      wa_ddval-value  = l_matkl.
      APPEND wa_ddval TO i_ddval.
    ENDSELECT.

*设置对应
    wa_alv-dd_handle = l_count.

    MODIFY i_alv FROM wa_alv.
  ENDLOOP.



  LOOP AT i_alv INTO wa_alv.
    ADD 1 TO l_count.
    CLEAR wa_ddval.
    wa_ddval-handle = l_count.
    wa_ddval-value  = '描述' && sy-tabix.
    APPEND wa_ddval TO i_ddval.

    SELECT maktx
        INTO @DATA(l_maktx)
        FROM makt
        WHERE matnr = @wa_alv-matnr.

      CLEAR wa_ddval.
      wa_ddval-handle = l_count.
      wa_ddval-value  = l_maktx.
      APPEND wa_ddval TO i_ddval.
    ENDSELECT.


*设置对应
    wa_alv-dd_handle1 = l_count.
    MODIFY i_alv FROM wa_alv.
  ENDLOOP.


ENDFORM.                    " f_create_dropdown_list
*@---------------------------------------------------------------------*
*@      Form  f_create_event_exits
*@---------------------------------------------------------------------*
*       添加下拉菜单事件
*----------------------------------------------------------------------*
FORM f_create_event_exits .
  wa_events-name = 'CALLER_EXIT'.        "固定值
  wa_events-form = 'F_CALLER_EXIT'.
  APPEND wa_events TO i_events.
ENDFORM.                    " f_create_event_exits
*@---------------------------------------------------------------------*
*@      Form  f_display_alv
*@---------------------------------------------------------------------*
*       调用 FUNCTION 输出ALV报表
*----------------------------------------------------------------------*
FORM f_display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = v_repid
      it_fieldcat_lvc    = i_fieldcat
      it_events          = i_events[]
    TABLES
      t_outtab           = i_alv.
ENDFORM.                    " f_display_alv
*@---------------------------------------------------------------------*
*@  FORMf_caller_exit
*@---------------------------------------------------------------------*
*   设置下拉列表，使Grid和内表能链接上
*----------------------------------------------------------------------*
FORM f_caller_exit USING ls_data TYPE slis_data_caller_exit.
  DATA: l_ref_alv TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
  CALL METHOD l_ref_alv->set_drop_down_table
    EXPORTING
      it_drop_down = i_ddval.
ENDFORM.                    "CALLER_EXIT
```

