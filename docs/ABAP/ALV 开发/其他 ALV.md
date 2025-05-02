[TOC]

# 弹出式ALV #

* **方式一**

  ```abap
    DATA: LO_ALV       TYPE REF TO CL_SALV_TABLE,
          LO_FUNLST    TYPE REF TO CL_SALV_FUNCTIONS_LIST,
          LO_SELETIONS TYPE REF TO CL_SALV_SELECTIONS,
          LT_ROWS      TYPE SALV_T_ROW,
          LO_CX_ROOT   TYPE REF TO CX_ROOT,
          LV_MSG       TYPE STRING.
  
    SELECT * INTO TABLE @DATA(LT_ZPST_XMLX_SJ_001) FROM ZPST_XMLX_SJ_001
      WHERE ZDJBH = @GS_ALV_0100-ZDJBH AND PSPID EQ @GS_ALV_0100-PSPID.
  
    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = LO_ALV
          CHANGING
            T_TABLE = LT_ZPST_XMLX_SJ_001[] ).
  
        LO_FUNLST = LO_ALV->GET_FUNCTIONS( ).
        LO_FUNLST->SET_ALL('X').
  	
  	  "设置 ALV 弹出位置
        IF LO_ALV IS BOUND.
          LO_ALV->SET_SCREEN_POPUP(
            START_COLUMN = 20
            END_COLUMN   = 180
            START_LINE   = 8
            END_LINE     = 18 ).
          LO_ALV->DISPLAY( ).
        ENDIF.
  
  	  "获取选中行
        LO_SELETIONS = LO_ALV->GET_SELECTIONS( ).
        LT_ROWS = LO_SELETIONS->GET_SELECTED_ROWS( ).
        IF LINES( LT_ROWS ) NE '1'.
          MESSAGE '请选则一行数据' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          READ TABLE LT_ZPST_XMLX_SJ_001 INTO GS_ALV_0100 INDEX LT_ROWS[ 1 ].
        ENDIF.
      CATCH CX_ROOT INTO LO_CX_ROOT.
        LV_MSG = LO_CX_ROOT->GET_TEXT( ).
        MESSAGE LV_MSG TYPE 'S' DISPLAY LIKE 'E'.
  
    ENDTRY.
  ```

* **方式二**

  ```abap
    DATA: LT_OBJID    LIKE TABLE OF ZHRS_051 WITH HEADER LINE,
          ES_SELFIELD TYPE  SLIS_SELFIELD,
          E_EXIT      TYPE CHAR1,
          LV_ERROR    TYPE CHAR1,
          LV_MESSAGE  TYPE CHAR255.
  
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        I_TITLE              = '选择导出单位'
        I_ZEBRA              = 'X'
        I_CHECKBOX_FIELDNAME = 'CHECKBOX'
        I_TABNAME            = SPACE    "这里默认是空就OK了
        I_STRUCTURE_NAME     = 'ZHRS_051'
      IMPORTING
        ES_SELFIELD          = ES_SELFIELD
        E_EXIT               = E_EXIT
      TABLES
        T_OUTTAB             = LT_OBJID[]  "输出内表.
      EXCEPTIONS
        PROGRAM_ERROR        = 1
        OTHERS               = 2.
  
    IF E_EXIT EQ 'X'.
      LV_ERROR = 'X'.
      LV_MESSAGE = '用户取消选择'.
    ENDIF.
  
  ```

# 弹框报错 #

- 弹框报错

  ```abap
  DATA: LT_MSG                TYPE RS_T_MSG,
        LF_ONE_MSG_AS_SYS_MSG TYPE FLAG.
  
  CL_EPIC_UI_SERVICES=>SHOW_MESSAGES_WITH_ALOG(
  IT_MESSAGES       = LT_MSG
  IV_ONE_MSG_DIRECT = LF_ONE_MSG_AS_SYS_MSG ).
  ```

  

