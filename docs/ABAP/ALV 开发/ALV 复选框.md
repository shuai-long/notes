[TOC]

# 复选框设置 #

## ALV自带复选框 ##

```abap
  DATA: ref_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.  "选择的数据
  
"得到当前ALV的GRID
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref_grid.
 
"获取当前ALV更改的内容
CALL METHOD ref_grid->check_changed_data. " 更改

"获取当前ALV选中的行
CALL METHOD ref_grid->get_selected_rows
    IMPORTING
       et_index_rows = lt_rows[]. "选择的行

"判断选中的行是否为空
IF lt_rows[]  IS INITIAL.
  MESSAGE '请至少选中一条数据' TYPE 'E'.
ENDIF.

"刷新ALV显示
CALL METHOD ref_grid->refresh_table_display. " 刷新
```



## 内表定义字段 ##

```abap
 DATA:ref_grid TYPE REF TO cl_gui_alv_grid,
 	  lt_rows  TYPE slis_t_filtered_entries.
 
 "得到当前ALV的GRID
 CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref_grid.

 "获取当前ALV更改的内容
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
  
"全选
 LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE checkbox EQ ''.
 	READ TABLE lt_rows TRANSPORTING NO FIELDS WITH KEY table_line = sy-tabix BINARY SEARCH.
 		IF sy-subrc NE 0.
           <fs_alv>-checkbox = 'X'.
        ENDIF.
 ENDLOOP.
 
 "取消全选
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE checkbox EQ 'X'.
  	READ TABLE lt_rows TRANSPORTING NO FIELDS WITH KEY table_line = sy-tabix BINARY SEARCH.
  		IF sy-subrc NE 0.
          <fs_alv>-checkbox = ''.
         ENDIF.
  ENDLOOP.
  
  "刷新ALV显示
  CALL METHOD ref_grid->refresh_table_display. " 刷新
```

