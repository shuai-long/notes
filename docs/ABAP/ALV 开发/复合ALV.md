[TOC]

# 复合ALV

## 创建类

SE24 创建类 `ZCL_GUI_ALV_GRID_MERGE`并继承`CL_GUI_ALV_GRID`

<img src="https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/%E5%A4%8D%E5%90%88ALV-%E5%88%9B%E5%BB%BA%E7%B1%BB.png" alt="创建自定义类实现CL_GUIALV_GRID" style="zoom:50%;" />

创建方法,分别用来合并列,合并行,设置单元格格式等:

<img src="https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/%E5%A4%8D%E5%90%88ALV-%E5%88%9B%E5%BB%BA%E6%96%B9%E6%B3%95.png" alt="创建类的方法" style="zoom:50%;" />

方法定义及方法实现

```ABAP
CLASS zcl_gui_alv_grid_merge DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_grid
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS z_set_merge_horiz
      IMPORTING
        row           TYPE i
      CHANGING
        tab_col_merge TYPE lvc_t_co01 .

    METHODS z_set_merge_vert
      IMPORTING
        row           TYPE i
      CHANGING
        tab_col_merge TYPE lvc_t_co01 .

    METHODS z_display .

    METHODS z_set_cell_style
      IMPORTING
        row    TYPE i OPTIONAL
        col    TYPE i OPTIONAL
        style  TYPE lvc_style
        style2 TYPE lvc_style OPTIONAL .

    METHODS z_set_fixed_col_row
      IMPORTING
        col TYPE i
        row TYPE i .

    METHODS z_init_cell_styles .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GUI_ALV_GRID_MERGE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_DISPLAY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD z_display.

    DATA lv_stable TYPE lvc_s_stbl.
    DATA lv_soft   TYPE c.

    CALL METHOD me->set_data_table
      CHANGING
        data_table = mt_data[].

    CALL METHOD set_auto_redraw
      EXPORTING
        enable = 1.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_INIT_CELL_STYLES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD z_init_cell_styles.

    FIELD-SYMBOLS <fs_data> TYPE lvc_s_data.

    LOOP AT mt_data ASSIGNING <fs_data>.
      <fs_data>-style = 0.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_SET_CELL_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            TYPE        I(optional)
* | [--->] COL                            TYPE        I(optional)
* | [--->] STYLE                          TYPE        LVC_STYLE
* | [--->] STYLE2                         TYPE        LVC_STYLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD z_set_cell_style.

    FIELD-SYMBOLS <fs_data> TYPE lvc_s_data.
    IF row IS INITIAL.
      IF col IS INITIAL.
        EXIT.
      ELSE.
        LOOP AT mt_data ASSIGNING <fs_data>
        WHERE col_pos = col.
          <fs_data>-style  = <fs_data>-style + style.
          <fs_data>-style2 = <fs_data>-style2 + style2.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF col IS INITIAL.
        LOOP AT mt_data ASSIGNING <fs_data>
        WHERE row_pos = row.
          <fs_data>-style  = <fs_data>-style + style.
          <fs_data>-style2 = <fs_data>-style2 + style2.
        ENDLOOP.
      ELSE.
        READ TABLE mt_data ASSIGNING <fs_data>
        WITH KEY row_pos = row
        col_pos = col.
        IF sy-subrc EQ 0.
          <fs_data>-style  = <fs_data>-style + style.
          <fs_data>-style2 = <fs_data>-style2 + style2.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_SET_FIXED_COL_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] COL                            TYPE        I
* | [--->] ROW                            TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD z_set_fixed_col_row.

    me->set_fixed_cols( col ).
    me->set_fixed_rows( row ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_SET_MERGE_HORIZ
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            TYPE        I
* | [<-->] TAB_COL_MERGE                  TYPE        LVC_T_CO01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD z_set_merge_horiz.

    FIELD-SYMBOLS <fs_cols> TYPE lvc_s_co01.
    FIELD-SYMBOLS <fs_data> TYPE lvc_s_data.
    DATA outputlen TYPE i.

    SORT tab_col_merge.
    LOOP AT tab_col_merge ASSIGNING <fs_cols>.
      IF <fs_cols>-col_id    LE 0.                CONTINUE. ENDIF.
      IF <fs_cols>-outputlen LE <fs_cols>-col_id. CONTINUE. ENDIF.
      outputlen = <fs_cols>-outputlen - <fs_cols>-col_id.
      LOOP AT mt_data ASSIGNING <fs_data>
      WHERE row_pos = row  AND
      ( col_pos BETWEEN <fs_cols>-col_id AND
      <fs_cols>-outputlen ).
        IF <fs_data>-col_pos = <fs_cols>-col_id.
          <fs_data>-mergehoriz = outputlen.
        ELSE.
          CLEAR <fs_data>-mergehoriz.
          CLEAR <fs_data>-value.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_MERGE->Z_SET_MERGE_VERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            TYPE        I
* | [<-->] TAB_COL_MERGE                  TYPE        LVC_T_CO01
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD z_set_merge_vert.

  FIELD-SYMBOLS <fs_cols> TYPE lvc_s_co01.
  FIELD-SYMBOLS <fs_data> TYPE lvc_s_data.
  DATA outputlen TYPE i.

  SORT tab_col_merge.
  LOOP AT tab_col_merge ASSIGNING <fs_cols>.
    IF <fs_cols>-col_id    LE 0.                CONTINUE. ENDIF.
    IF <fs_cols>-outputlen LE <fs_cols>-col_id. CONTINUE. ENDIF.
    outputlen = <fs_cols>-outputlen - <fs_cols>-col_id.
    LOOP AT mt_data ASSIGNING <fs_data>
    WHERE row_pos = row  AND
    ( col_pos BETWEEN <fs_cols>-col_id AND
    <fs_cols>-outputlen ).
      IF <fs_data>-col_pos = <fs_cols>-col_id.
        <fs_data>-mergevert = outputlen.
      ELSE.
        CLEAR <fs_data>-mergevert.
        CLEAR <fs_data>-value.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
```

## 创建测试ALV

```ABAP
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
REPORT  zz_alv_merge_cells.


* *** Allgemeines  ****************************************************
INCLUDE <cl_alv_control>.
INCLUDE <icon>.
DATA retc    TYPE sy-subrc.                  .
DATA ok_code TYPE sy-ucomm.
DATA it_grp  TYPE lvc_t_sgrp.
DATA it_fil  TYPE lvc_t_filt.
DATA wa_fil  TYPE lvc_s_filt.

* **** ALV_GRID    ****************************************************
TYPES: BEGIN OF t_check_styles,
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
TYPES END OF t_check_styles.

DATA it_styles TYPE  t_check_styles OCCURS 0.
DATA wa_styles TYPE t_check_styles.
FIELD-SYMBOLS <fs_styles>        TYPE t_check_styles.
DATA :          lt_fieldcatalog  TYPE lvc_t_fcat.
DATA :          ls_fieldcatalog  TYPE lvc_t_fcat.
DATA :          wa_cat           TYPE lvc_s_fcat.
DATA : fieldname(40).
DATA : fieldnr(2) TYPE n.

FIELD-SYMBOLS  <fs_cat> TYPE lvc_s_fcat.

DATA  lt_iinfo TYPE lvc_t_info.
DATA  wa_iinfo TYPE lvc_s_info.
DATA  lt_idata TYPE lvc_t_data.
DATA  wa_idata TYPE lvc_s_data.

DATA it_col_merge        TYPE lvc_t_co01.
DATA wa_col_merge        TYPE lvc_s_co01.
DATA: g_container        TYPE scrfname VALUE 'CU_CON'.
DATA: g_custom_container TYPE REF TO cl_gui_custom_container.
DATA  g_alv_grid         TYPE REF TO zcl_gui_alv_grid_merge.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: x_save,                     "for Parameter I_SAVE
      gs_variant TYPE disvariant. "for parameter IS_VARIANT
DATA gs_layout TYPE lvc_s_layo.   " Layout
DATA wa_style  TYPE lvc_s_styl.


* **** ALV_GRID    ****************************************************
START-OF-SELECTION.
  CALL SCREEN 0200.

END-OF-SELECTION.




*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

** Status und Titel setzen
  SET PF-STATUS '0200'.
  SET TITLEBAR '001'.

** Objekte instanzieren und zuordnen: Grid
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT g_alv_grid
      EXPORTING
        i_parent = g_custom_container.

    gs_layout-stylefname = 'CELL'.
    gs_layout-no_headers = 'X'.
    gs_layout-cwidth_opt = ' '.
    gs_layout-no_toolbar = 'X'.

** Feldkatalog erzeugen
    REFRESH lt_fieldcatalog.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_internal_tabname     = 'IT_STYLES'
      CHANGING
        ct_fieldcat            = lt_fieldcatalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    REFRESH lt_fieldcatalog.
    REFRESH lt_iinfo.

    DO 12 TIMES.
      CLEAR wa_cat.
      fieldnr = sy-index.
      wa_cat-col_pos = sy-index.
      CONCATENATE 'FIELD' fieldnr INTO fieldname.
      wa_cat-fieldname = fieldname.
      wa_cat-tabname   = '1'.
      wa_cat-datatype  = 'CHAR'.
      wa_cat-inttype   = 'C'.
      wa_cat-intlen    = 20.
      IF sy-index > 1.
        wa_cat-outputlen    = 6.
      ELSE.
        wa_cat-outputlen    = 20.
      ENDIF.
      wa_cat-reptext   = fieldname.
      wa_cat-scrtext_l = fieldname.
      wa_cat-scrtext_m = fieldname.
      wa_cat-scrtext_s = fieldname.
      wa_cat-scrtext_l = fieldname.
      APPEND wa_cat TO lt_fieldcatalog.
    ENDDO.

* 1 Zeile
    CLEAR wa_styles.
    wa_styles-field01 = 'TRICKTRESOR'.
    wa_styles-field03 = 'F'.
    wa_styles-field04 = 'P'.
    wa_styles-field09 = 'M'.
    wa_styles-field10 = 'K'.
    APPEND wa_styles TO it_styles.
* 2 Zeile
    CLEAR wa_styles.
    wa_styles-field03 = 'HQ'.
    wa_styles-field04 = 'HC'.
    wa_styles-field08 = 'HW'.
    wa_styles-field09 = 'HC'.
    wa_styles-field10 = 'HC'.
    wa_styles-field12 = 'HW'.
    APPEND wa_styles TO it_styles.
* 3-Zeile
    CLEAR wa_styles.
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
    APPEND wa_styles TO it_styles.

* 4..Zeile
    CLEAR wa_styles.
    wa_styles-field01 = 'SPMW 060304 T - A 27'.
    wa_styles-field02 = '0.54'.
    wa_styles-field03 = icon_led_green.
    wa_styles-field04 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field08 = icon_led_yellow.
    APPEND wa_styles TO it_styles.

    CLEAR wa_styles.
    wa_styles-field01 = 'SPMW 060304 - A 57'.
    wa_styles-field02 = '0.43'.
    wa_styles-field03 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field08 = icon_led_yellow.
    wa_styles-field10 = icon_led_yellow.
    wa_styles-field11 = icon_led_red.
    wa_styles-field12 = icon_led_yellow.
    APPEND wa_styles TO it_styles.

    CLEAR wa_styles.
    wa_styles-field01 = 'SPMW 060304 - D 51'.
    wa_styles-field02 = '0.76'.
    wa_styles-field04 = icon_led_yellow.
    wa_styles-field05 = icon_led_red.
    wa_styles-field06 = icon_led_red.
    wa_styles-field07 = icon_led_red.
    APPEND wa_styles TO it_styles.

    CLEAR wa_styles.
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
    APPEND wa_styles TO it_styles.

*
    CALL METHOD g_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant
        i_save          = x_save
        is_layout       = gs_layout
      CHANGING
        it_fieldcatalog = lt_fieldcatalog
        it_outtab       = it_styles.

    REFRESH it_col_merge.

*** DEMO vertikal verbinden
    wa_col_merge-col_id    = 1.
    wa_col_merge-outputlen = 2.
    APPEND wa_col_merge TO it_col_merge.

    CALL METHOD g_alv_grid->z_set_merge_vert
      EXPORTING
        row           = 1
      CHANGING
        tab_col_merge = it_col_merge.
    wa_style-style     = alv_style_font_bold
                       + alv_style_align_center_center
                       + alv_style_color_key.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 1
        col   = 1
        style = wa_style-style.

*** VERTIKAL verbinden
    CLEAR it_col_merge.

    wa_col_merge-col_id    = 4.
    wa_col_merge-outputlen = 8.
    APPEND wa_col_merge TO it_col_merge.

    wa_col_merge-col_id    = 10.
    wa_col_merge-outputlen = 12.
    APPEND wa_col_merge TO it_col_merge.

    CALL METHOD g_alv_grid->z_set_merge_horiz
      EXPORTING
        row           = 1
      CHANGING
        tab_col_merge = it_col_merge.

    wa_style-style     = alv_style_font_bold.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 1
        col   = 3
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 1
        col   = 4
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 1
        col   = 9
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 1
        col   = 10
        style = wa_style-style.

    REFRESH it_col_merge.

    wa_col_merge-col_id    = 4.
    wa_col_merge-outputlen = 7.
    APPEND wa_col_merge TO it_col_merge.

    wa_col_merge-col_id    = 10.
    wa_col_merge-outputlen = 2.
    APPEND wa_col_merge TO it_col_merge.


    CALL METHOD g_alv_grid->z_set_merge_horiz
      EXPORTING
        row           = 2
      CHANGING
        tab_col_merge = it_col_merge.

    wa_style-style     = alv_style_color_group +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 3
        style = wa_style-style.

    wa_style-style     = alv_style_color_heading +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 4
        style = wa_style-style.


    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 5
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 6
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 7
        style = wa_style-style.
    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 8
        style = wa_style-style.

    wa_style-style     = alv_style_color_total +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 9
        style = wa_style-style.


    wa_style-style     = alv_style_color_negative +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 10
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 11
        style = wa_style-style.


    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 12
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 13
        style = wa_style-style.

    wa_style-style     = alv_style_color_positive +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 14
        style = wa_style-style.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 15
        style = wa_style-style.

    wa_style-style     = alv_style_color_int_background +
                         alv_style_align_center_center.

    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        col   = 16
        style = wa_style-style.

    wa_style-style     = alv_style_color_positive +
                         alv_style_align_center_center +
                         alv_style_font_italic.


    CALL METHOD g_alv_grid->z_set_cell_style
      EXPORTING
        row   = 4
        col   = 2
        style = wa_style-style.

    g_alv_grid->z_set_fixed_col_row(
            EXPORTING col = 3
                      row = 3 ).

    g_alv_grid->z_display( ).

  ENDIF.
ENDMODULE.                    "status_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
*   to react on oi_custom_events:
  cl_gui_cfw=>dispatch( ).

  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
```



