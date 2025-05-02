```abap
CLASS cl_tools_table_control DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      user_ok_tc
        IMPORTING
          iv_tc_name    TYPE dynfnam
          iv_table_name TYPE any
          iv_mark_name  TYPE any
        CHANGING
          iv_ok         LIKE sy-ucomm,

      fcode_insert_row
        IMPORTING
          iv_tc_name    TYPE dynfnam
          iv_table_name TYPE any ,

      fcode_delete_row
        IMPORTING
          iv_tc_name    TYPE dynfnam
          iv_table_name TYPE any
          iv_mark_name  TYPE any ,

      compute_scrolling_in_tc
        IMPORTING
          iv_tc_name TYPE any
          iv_ok      TYPE any,

      fcode_tc_mark_lines
        IMPORTING
          iv_tc_name    TYPE any
          iv_table_name TYPE any
          iv_mark_name  TYPE any,

      fcode_tc_demark_lines
        IMPORTING
          iv_tc_name    TYPE any
          iv_table_name TYPE any
          iv_mark_name  TYPE any.

ENDCLASS.
```

类实现

```abap
CLASS cl_tools_table_control IMPLEMENTATION.
  METHOD user_ok_tc.

    DATA: l_ok     TYPE sy-ucomm,
          l_offset TYPE i.

    SEARCH iv_ok FOR iv_tc_name.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    l_offset = strlen( iv_tc_name ) + 1.
    l_ok = iv_ok+l_offset.

    CASE l_ok.
      WHEN 'INSR'.
        fcode_insert_row(  iv_tc_name = iv_tc_name  iv_table_name = iv_table_name ).
        CLEAR iv_ok.

      WHEN 'DELE'.
        fcode_delete_row( iv_tc_name = iv_tc_name  iv_table_name = iv_table_name iv_mark_name = iv_mark_name ).
        CLEAR iv_ok.

      WHEN 'P--' OR  'P-'  OR 'P+'  OR 'P++'.
        compute_scrolling_in_tc( iv_tc_name = iv_tc_name  iv_ok = l_ok ).
        CLEAR iv_ok.
*
      WHEN 'MARK'.
        fcode_tc_mark_lines( iv_tc_name = iv_tc_name iv_table_name = iv_table_name iv_mark_name = iv_mark_name ).
        CLEAR iv_ok.

      WHEN 'DMRK'.
        fcode_tc_demark_lines( iv_tc_name = iv_tc_name iv_table_name = iv_table_name iv_mark_name = iv_mark_name ).
        CLEAR iv_ok.

    ENDCASE.
  ENDMETHOD.

  METHOD fcode_insert_row.

    DATA: l_lines_name TYPE feld-name,
          l_selline    LIKE sy-stepl,
          l_lastline   TYPE i,
          l_line       TYPE i,
          l_table_name TYPE feld-name.

    FIELD-SYMBOLS: <tc>    TYPE cxtab_control,
                   <table> TYPE STANDARD TABLE,
                   <lines> TYPE i.

    ASSIGN (iv_tc_name) TO <tc>.

    CONCATENATE iv_table_name '[]' INTO l_table_name.
    ASSIGN (l_table_name) TO <table>.

    CONCATENATE 'G_' iv_tc_name '_LINES' INTO l_lines_name.
    ASSIGN (l_lines_name) TO <lines>.

    GET CURSOR LINE l_selline.
    IF sy-subrc <> 0.
      l_selline = <tc>-lines + 1.

      IF l_selline > <lines>.
        <tc>-top_line = l_selline - <lines> + 1 .
      ELSE.
        <tc>-top_line = 1.
      ENDIF.
    ELSE.
      l_selline = <tc>-top_line + l_selline - 1.
      l_lastline = <tc>-top_line + <lines> - 1.
    ENDIF.

    l_line = l_selline - <tc>-top_line + 1.

    INSERT INITIAL LINE INTO <table> INDEX l_selline.
    <tc>-lines = <tc>-lines + 1.

    SET CURSOR 1 l_line.

  ENDMETHOD.

  METHOD fcode_delete_row.

    DATA: l_table_name TYPE feld-name.

    FIELD-SYMBOLS: <tc>         TYPE cxtab_control,
                   <table>      TYPE STANDARD TABLE,
                   <wa>         TYPE any,
                   <mark_field> TYPE any.

    ASSIGN (iv_tc_name) TO <tc>.

    CONCATENATE iv_table_name '[]' INTO l_table_name.
    ASSIGN (l_table_name) TO <table>.

    DESCRIBE TABLE <table> LINES <tc>-lines.

    LOOP AT <table> ASSIGNING <wa>.

      ASSIGN COMPONENT iv_mark_name OF STRUCTURE <wa> TO <mark_field>.

      IF <mark_field> = 'X'.
        DELETE <table> INDEX syst-tabix.
        IF sy-subrc = 0.
          <tc>-lines = <tc>-lines - 1.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD compute_scrolling_in_tc.

    DATA: l_tc_new_top_line TYPE i,
          l_tc_name         TYPE feld-name,
          l_tc_lines_name   TYPE feld-name,
          l_tc_field_name   TYPE feld-name.

    FIELD-SYMBOLS: <tc>    TYPE cxtab_control,
                   <lines> TYPE i.

    ASSIGN (iv_tc_name) TO <tc>.
    CONCATENATE 'G_' iv_tc_name '_LINES' INTO l_tc_lines_name.
    ASSIGN (l_tc_lines_name) TO <lines>.

    IF <tc>-lines = 0.
      l_tc_new_top_line = 1.
    ELSE.
      CALL FUNCTION 'SCROLLING_IN_TABLE'
        EXPORTING
          entry_act      = <tc>-top_line
          entry_from     = 1
          entry_to       = <tc>-lines
          last_page_full = 'X'
          loops          = <lines>
          ok_code        = iv_ok
          overlapping    = 'X'
        IMPORTING
          entry_new      = l_tc_new_top_line
        EXCEPTIONS
*         NO_ENTRY_OR_PAGE_ACT  = 01
*         NO_ENTRY_TO    = 02
*         NO_OK_CODE_OR_PAGE_GO = 03
          OTHERS         = 0.
    ENDIF.

    GET CURSOR FIELD l_tc_field_name
               AREA  l_tc_name.

    IF syst-subrc = 0.
      IF l_tc_name = iv_tc_name.
        SET CURSOR FIELD l_tc_field_name LINE 1.
      ENDIF.
    ENDIF.

    <tc>-top_line = l_tc_new_top_line.

  ENDMETHOD.

  METHOD fcode_tc_mark_lines.

    DATA: l_table_name       TYPE feld-name.

    FIELD-SYMBOLS: <tc>         TYPE cxtab_control,
                   <table>      TYPE STANDARD TABLE,
                   <wa>         TYPE any,
                   <mark_field> TYPE any.

    ASSIGN (iv_tc_name) TO <tc>.

    CONCATENATE iv_table_name '[]' INTO l_table_name.
    ASSIGN (l_table_name) TO <table>.

    LOOP AT <table> ASSIGNING <wa>.
      ASSIGN COMPONENT iv_mark_name OF STRUCTURE <wa> TO <mark_field>.
      <mark_field> = 'X'.
    ENDLOOP.

  ENDMETHOD.

  METHOD fcode_tc_demark_lines.

    DATA l_table_name TYPE feld-name.

    FIELD-SYMBOLS: <tc>         TYPE cxtab_control,
                   <table>      TYPE STANDARD TABLE,
                   <wa>         TYPE any,
                   <mark_field> TYPE any.

    ASSIGN (iv_tc_name) TO <tc>.

    CONCATENATE iv_table_name '[]' INTO l_table_name.
    ASSIGN (l_table_name) TO <table>.

    LOOP AT <table> ASSIGNING <wa>.
      ASSIGN COMPONENT iv_mark_name OF STRUCTURE <wa> TO <mark_field>.
      <mark_field> = space.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
```

