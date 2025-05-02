[TOC]

# SMW0 下载函数重写

![函数参数-导入](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20200717163148834.png)

![函数参数-导出](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20200717163214411.png)

```ABAP
FUNCTION ZFUN_XLSDOWNLOAD .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(I_OBJID) TYPE  W3OBJID
*"     VALUE(I_NAME) TYPE  STRING DEFAULT '模板'
*"  EXPORTING
*"     VALUE(E_FULLPATH) TYPE  STRING
*"--------------------------------------------------------------------"

*  DATA: query_str TYPE w3query OCCURS 0 WITH HEADER LINE.
  DATA : lv_objdata  LIKE wwwdatatab.
  DATA : thtml TYPE w3html OCCURS 0 WITH HEADER LINE,
         tmime TYPE w3mime OCCURS 0 WITH HEADER LINE.
  DATA : user_action TYPE i.
  DATA : return_code    LIKE  w3param-ret_code,
         content_type   LIKE  w3param-cont_type,
         content_length LIKE  w3param-cont_len.
  DATA : lv_filename TYPE string,
         lv_file     TYPE rlgrap-filename,
         lv_path     TYPE string,
         lv_fullpath TYPE string.
  DATA : l_rc  TYPE sy-subrc,
         l_msg TYPE text100. "消息"


  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_file_name         = i_name
      default_extension         = 'xlsx'
*     default_extension         = 'xls'
    CHANGING
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
      user_action               = user_action
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.
  IF sy-subrc = 0.
    e_fullpath = lv_fullpath.
*   Implement suitable error handling here
  ELSE.

  ENDIF.
  IF user_action EQ cl_gui_frontend_services=>action_cancel.
    MESSAGE '用户取消操作！' TYPE 'S' DISPLAY LIKE 'W'. "用户取消操作
    EXIT.
  ENDIF.

  SELECT relid objid
      FROM wwwdata
      INTO  CORRESPONDING FIELDS OF lv_objdata
      UP TO 1 ROWS
      WHERE srtf2 = 0 AND relid = 'MI'
        AND objid = i_objid.
  ENDSELECT.

  IF lv_objdata IS INITIAL.
    CLEAR : l_msg.
    l_msg = '模板' && i_objid && '不存在!'.
    MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lv_fullpath IS NOT INITIAL.
    DATA : lr_destina TYPE rlgrap-filename.
    MOVE lv_fullpath TO lr_destina.
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = lv_objdata
        destination = lr_destina
      IMPORTING
        rc          = l_rc
*     CHANGING
*       TEMP        =
      .
    IF l_rc = 0.
      MESSAGE '下载成功!' TYPE 'S'.
    ELSE.
      MESSAGE '下载失败!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


ENDFUNCTION.
```

