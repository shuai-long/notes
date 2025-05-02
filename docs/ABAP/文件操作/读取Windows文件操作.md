[TOC]

# 读取文件 #

## 获取用户桌面 ##

```abap
FORM FRM_GET_USER_DESKTOP USING DESK_TOP_PATH.
  DATA: LV_PATH TYPE STRING.

  LV_PATH = DESK_TOP_PATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY = LV_PATH.

  CALL METHOD CL_GUI_CFW=>UPDATE_VIEW.

  DESK_TOP_PATH = LV_PATH.
ENDFORM.
```

## 获取 SAPGUI 工作文件夹

```abap
data: lv_dirpath type string.

cl_gui_frontend_services=>get_sapgui_workdir(
	changing
		sapworkdir            = lv_dirpath
  exceptions
    get_sapworkdir_failed = 1
    cntl_error            = 2
    error_no_gui          = 3
    not_supported_by_gui  = 4
    others                = 5
).
```

## 选择文件 ##

* 使用类 `CL_GUI_FRONTEND_SERVICES` 的方法(常用):

```abap
FORM FRM_F4_HELP_FILE CHANGING O_FNAME TYPE RLGRAP-FILENAME.
  DATA: L_FILETAB TYPE FILETABLE,
        L_WAFTAB  LIKE LINE OF L_FILETAB,
        L_RC      TYPE I.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = '打开文件'
      INITIAL_DIRECTORY       = 'C:/'
    CHANGING
      FILE_TABLE              = L_FILETAB
      RC                      = L_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ELSE.
    READ TABLE L_FILETAB INTO L_WAFTAB INDEX 1.
    O_FNAME = L_WAFTAB-FILENAME.
    CLEAR: L_FILETAB,
           L_WAFTAB.
  ENDIF.
ENDFORM.
```

* 其他方法:

  ```abap
  REPORT ZTEST_0003.
  
  PARAMETERS P_FILE TYPE RLGRAP-FILENAME.
  
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  *  PERFORM FRM_GET_EXCEL1.    "不能限制上传文件类型
  *  PERFORM FRM_GET_EXCEL2.    "可以限制上传文件类型，不能设置默认路径
  *  PERFORM FRM_GET_EXCEL3.    "可以限制上传文件类型，能设置默认路径，默认文件
  *  PERFORM FRM_GET_EXCEL4.    "可以限制上传文件类型，能设置默认路径，默认文件，但系统报已过时
  *  PERFORM FRM_GET_EXCEL5.    "可以限制上传文件类型，能设置默认路径，默认文件，能上传多个文件
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL1
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_GET_EXCEL1 .
    CALL FUNCTION 'F4_FILENAME'
  * EXPORTING
  *   PROGRAM_NAME        = SYST-CPROG
  *   DYNPRO_NUMBER       = SYST-DYNNR
  *   FIELD_NAME          = ' '
      IMPORTING
        FILE_NAME = P_FILE.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL2
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_GET_EXCEL2 .
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
  *    EXPORTING
  *     PROGRAM_NAME        = SYST-REPID
  *     DYNPRO_NUMBER       = SYST-DYNNR
  *     FIELD_NAME          = ' '
  *     STATIC    = ' '
  *      MASK      = 'All Files (*.*)|*.*|Excel Files (*.xlxs)|*.xls|Word files(*.doc)|*.doc'
  *     FILEOPERATION       = 'R'
  *     PATH      =
      CHANGING
        FILE_NAME = P_FILE
  *     LOCATION_FLAG       = 'P'
  *   EXCEPTIONS
  *     MASK_TOO_LONG       = 1
  *     OTHERS    = 2
      .
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL3
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_GET_EXCEL3 .
    CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
      EXPORTING
  *     DEF_FILENAME           = ' '
        DEF_PATH = 'D:\'
        MASK     = 'All Files (*.*)|*.*|Excel Files (*.xls)|*.xls|Word files(*.doc)|*.doc'
  *     MODE     = ' '
  *     TITLE    = ' '
      IMPORTING
        FILENAME = P_FILE
  *     PATH     =
  *     FILE     =
  * EXCEPTIONS
  *     SELECTION_CANCEL       = 1
  *     SELECTION_ERROR        = 2
  *     OTHERS   = 3
      .
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL4
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_GET_EXCEL4 .
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
  *     DEF_FILENAME           = ' '
        DEF_PATH = 'D:\'
        MASK     = ',All Files (*.*)|*.*|Excel Files (*.xls)|*.xls|Word files(*.doc)|*.doc' "必须加一个逗号
  *     MODE     = ' '
  *     TITLE    = ' '
      IMPORTING
        FILENAME = P_FILE
  *     RC       =
  * EXCEPTIONS
  *     INV_WINSYS             = 1
  *     NO_BATCH = 2
  *     SELECTION_CANCEL       = 3
  *     SELECTION_ERROR        = 4
  *     OTHERS   = 5
      .
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL5
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_GET_EXCEL5 .
    DATA LT_TAB TYPE FILETABLE.
    DATA LS_TAB LIKE LINE OF LT_TAB.
    DATA L_RC TYPE I.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
      EXPORTING
  *     WINDOW_TITLE   =
  *     DEFAULT_EXTENSION = ''
  *     DEFAULT_FILENAME  =
        FILE_FILTER    = 'All Files (*.*)|*.*|Excel Files (*.xls)|*.xls|Word files(*.doc)|*.doc'
  *     WITH_ENCODING  =
  *     INITIAL_DIRECTORY =
        MULTISELECTION = 'X'
      CHANGING
        FILE_TABLE     = LT_TAB
        RC             = L_RC
  *     USER_ACTION    =
  *     FILE_ENCODING  =
  *  EXCEPTIONS
  *     FILE_OPEN_DIALOG_FAILED = 1
  *     CNTL_ERROR     = 2
  *     ERROR_NO_GUI   = 3
  *     NOT_SUPPORTED_BY_GUI    = 4
  *     OTHERS         = 5
      .
    READ TABLE LT_TAB INTO LS_TAB INDEX 1.
    P_FILE = LS_TAB-FILENAME.
  ENDFORM.
  
  ```

## 选择文件夹 ##

```abap
FORM FRM_GET_DIRECTORY USING DIRECTORY.

  DATA: WINDOW_TITLE    TYPE STRING VALUE '文件夹选择',
        INITIAL_FOLDER  TYPE STRING VALUE 'C:\',
        SELECTED_FOLDER TYPE STRING.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE         = WINDOW_TITLE
      INITIAL_FOLDER       = INITIAL_FOLDER
    CHANGING
      SELECTED_FOLDER      = SELECTED_FOLDER
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ELSE.
    DIRECTORY = SELECTED_FOLDER.
  ENDIF.
ENDFORM.
```

## 遍历文件夹中文件 ##

```abap
FORM FRM_GET_DIRECTORY_FILES TABLES FILE_TABLE STRUCTURE FILE_INFO USING PATH FILE_COUNT.

  DATA: DIRECTORY        TYPE STRING,
        FILES_ONLY       TYPE ABAP_BOOL,
        DIRECTORIES_ONLY TYPE ABAP_BOOL,
        COUNT            TYPE I.

  DIRECTORY = PATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = DIRECTORY
      FILTER                      = '*.*'
      FILES_ONLY                  = FILES_ONLY
      DIRECTORIES_ONLY            = DIRECTORIES_ONLY
    CHANGING
      FILE_TABLE                  = FILE_TABLE
      COUNT                       = COUNT
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      NOT_SUPPORTED_BY_GUI        = 5
      OTHERS                      = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  FILE_COUNT = COUNT.

ENDFORM.
```

# 下载文件 #

## SMW0 下载 ##

```abap
FORM FRM_DOWNLOAD_EXCEL USING OBJID TYPE WWWDATATAB-OBJID.

  DATA: LV_FILENAME TYPE STRING,
        LV_PATH     TYPE STRING,
        LV_FULLPATH TYPE STRING.

  SELECT SINGLE * FROM WWWDATA INTO @DATA(LS_OBJ) WHERE OBJID = @OBJID.
  LV_FILENAME = |{ LS_OBJ-TEXT }.xlsx|.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_FILE_NAME         = LV_FILENAME
      FILE_FILTER               = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
      PROMPT_ON_OVERWRITE       = 'X'
    CHANGING
      FILENAME                  = LV_FILENAME
      PATH                      = LV_PATH
      FULLPATH                  = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR                = 1
      ERROR_NO_GUI              = 2
      NOT_SUPPORTED_BY_GUI      = 3
      INVALID_DEFAULT_FILE_NAME = 4
      OTHERS                    = 5.

  CHECK SY-SUBRC EQ 0 AND LV_FULLPATH IS NOT INITIAL.

  DATA: LV_DESTINATION LIKE RLGRAP-FILENAME,
  		LS_KEY。       TYPE WWWDATATAB,
        LV_RC          LIKE SY-SUBRC.

  LV_DESTINATION = LV_FULLPATH.
  move-corresponding ls_obj to ls_key.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = LS_KEY
      DESTINATION = LV_DESTINATION
    IMPORTING
      RC          = LV_RC.

ENDFORM.
```

# 上传文件

## 上传文件（不用关闭 Excel）

```abap
   data: lv_filepath type ibipparms-path,
          lv_msg      type string,
          lv_lifnr    type ekko-lifnr,
          lt_excel    type standard table of kcde_intern_struc.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = '模板上传中...'.

    lv_filepath = p_file.

    call function 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      exporting
        filename                = lv_filepath
        i_begin_col             = gc_col_begin
        i_begin_row             = gc_row_begin
        i_end_col               = gc_col_end
        i_end_row               = gc_row_end
      tables
        intern                  = lt_excel
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2.

    if sy-subrc <> 0.
      message '数据读取失败' type 'S' display like 'E'.
      leave list-processing.
    endif.

    loop at lt_excel into data(ls_excel).
      at new row.
        append initial line to gt_excel assigning field-symbol(<fs_itab>).
      endat.
      assign component ls_excel-col of structure <fs_itab> to field-symbol(<fs_value>).
      <fs_value> = ls_excel-value.
    endloop.

    move-corresponding gt_excel to gt_alv.
```

## 上传文件（需要关闭 Excel）

```abap
FORM frm_read_excel TABLES l_tab
  USING pv_file TYPE rlgrap-filename
        lv_i_begin_col TYPE i
        lv_i_begin_row TYPE i
        lv_i_end_col TYPE i
        lv_i_end_row TYPE i
  CHANGING l_wa TYPE any.
  REFRESH l_tab.

  DATA: lt_excel TYPE TABLE OF alsmex_tabline WITH HEADER LINE.
  FIELD-SYMBOLS <lfs_value> TYPE any.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pv_file
      i_begin_col             = lv_i_begin_col
      i_begin_row             = lv_i_begin_row
      i_end_col               = lv_i_end_col
      i_end_row               = lv_i_end_row
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE s001(00) WITH '读取 Excel 文件失败' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  LOOP AT lt_excel.
    ASSIGN COMPONENT lt_excel-col OF STRUCTURE l_wa TO <lfs_value>.
    IF sy-subrc = 0 .
      <lfs_value> = lt_excel-value.
    ENDIF.
    AT END OF row.
      APPEND l_wa TO l_tab.
      CLEAR l_wa.
    ENDAT.
  ENDLOOP.
ENDFORM."模板读取
```

# 文件操作 #

## 获取路径和文件名 ##

```abap
FORM FRM_GET_FILE_AND_PATH USING FULL_NAME FILE PATH.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME     = FULL_NAME
    IMPORTING
      STRIPPED_NAME = FILE
      FILE_PATH     = PATH
    EXCEPTIONS
      X_ERROR       = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

ENDFORM.

```

## 获取文件类型 ##

```abap
FORM FRM_GET_FILE_TYPE USING FULL_FILE FILE_NAME FILE_TYPE.
  DATA: LEN          TYPE I,
        POS          TYPE I,
        LV_FULL_FILE TYPE CHAR255.

  LV_FULL_FILE = FULL_FILE.
  LEN = STRLEN( LV_FULL_FILE ).
  POS = LEN.

  WHILE LV_FULL_FILE+POS(1) <> '.' AND POS > 0.
    POS = POS - 1.
  ENDWHILE.


  FILE_NAME    = LV_FULL_FILE+0(POS).
  POS          = POS + 1.
  FILE_TYPE    = LV_FULL_FILE+POS.
ENDFORM.
```

