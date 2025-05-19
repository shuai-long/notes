[TOC]

# HR常用BAPI #

后续可以使用相关类 `CL_HRBAS *`,待研究

## 人事操作 ##

### 员工照片处理

- **获取员工照片二进制数据**

  ```ABAP
  "查询图片
  FORM FRM_GET_PERNR_PICTURE TABLES PERNR USING JSON.
  
    DATA: LT_PERNR TYPE TABLE OF PERNR_D.
  
    DATA: BEGIN OF LS_PIC,
            PERNR       TYPE PERNR_D,
            TYPE        TYPE CHAR1,
            MESSAGE     TYPE CHAR255,
            ACCESS_INFO TYPE SCMS_ACINF,
            PIC         TYPE STRING,
          END OF LS_PIC.
  
    DATA: BEGIN OF LS_DATA_OUT.
            INCLUDE STRUCTURE GS_DATA_OUT.
    DATA:   PICTURE LIKE TABLE OF LS_PIC.
    DATA:END OF LS_DATA_OUT.
  
    DATA: P_EXISTS       TYPE C,
          P_CONNECT_INFO TYPE TOAV0.
  
    DATA: STOR_CAT    TYPE SDOKSTCA-STOR_CAT,
          FROM_CACHE  TYPE C,
          CREA_TIME   TYPE T,
          CREA_DATE   TYPE D,
          CHNG_TIME   TYPE T,
          CHNG_DATE   TYPE D,
          STATUS      TYPE SCMS_DOCST,
          DOC_PROT    TYPE C,
          ACCESS_INFO TYPE TABLE OF SCMS_ACINF,
          CONTENT_TXT TYPE TABLE OF SDOKCNTASC,
          CONTENT_BIN TYPE TABLE OF SDOKCNTBIN.
  
    LT_PERNR = PERNR[].
  
    IF LT_PERNR[] IS INITIAL.
      LS_DATA_OUT-TYPE = 'E'.
      LS_DATA_OUT-MESSAGE = '人员编号不能为空'.
    ELSE.
  
      LS_DATA_OUT-TYPE = 'S'.
      LS_DATA_OUT-MESSAGE = '查询成功'.
  
      LOOP AT LT_PERNR INTO DATA(LS_PERNR).
  
        CLEAR: P_EXISTS, P_CONNECT_INFO,LS_PIC.
        REFRESH: ACCESS_INFO,CONTENT_TXT,CONTENT_BIN.
  
        LS_PIC-PERNR = LS_PERNR.
  
        CALL FUNCTION 'HR_IMAGE_EXISTS'
          EXPORTING
            P_PERNR               = LS_PERNR
          IMPORTING
            P_EXISTS              = P_EXISTS
            P_CONNECT_INFO        = P_CONNECT_INFO
          EXCEPTIONS
            ERROR_CONNECTIONTABLE = 1
            OTHERS                = 2.
  
        IF SY-SUBRC NE 0 AND P_EXISTS IS INITIAL.
          LS_PIC-TYPE = 'E'.
          LS_PIC-MESSAGE = '图片不存在'.
        ELSE.
  
          CALL FUNCTION 'SCMS_DOC_READ'
            EXPORTING
              STOR_CAT              = STOR_CAT
              CREP_ID               = P_CONNECT_INFO-ARCHIV_ID
              DOC_ID                = P_CONNECT_INFO-ARC_DOC_ID
              RAW_MODE              = 'X'
            IMPORTING
              FROM_CACHE            = FROM_CACHE
              CREA_TIME             = CREA_TIME
              CREA_DATE             = CREA_DATE
              CHNG_TIME             = CHNG_TIME
              CHNG_DATE             = CHNG_DATE
              STATUS                = STATUS
              DOC_PROT              = DOC_PROT
            TABLES
              ACCESS_INFO           = ACCESS_INFO
              CONTENT_TXT           = CONTENT_TXT
              CONTENT_BIN           = CONTENT_BIN
            EXCEPTIONS
              BAD_STORAGE_TYPE      = 1
              BAD_REQUEST           = 2
              UNAUTHORIZED          = 3
              COMP_NOT_FOUND        = 4
              NOT_FOUND             = 5
              FORBIDDEN             = 6
              CONFLICT              = 7
              INTERNAL_SERVER_ERROR = 8
              ERROR_HTTP            = 9
              ERROR_SIGNATURE       = 10
              ERROR_CONFIG          = 11
              ERROR_FORMAT          = 12
              ERROR_PARAMETER       = 13
              ERROR                 = 14
              OTHERS                = 15.
  
  
          IF SY-SUBRC NE 0.
            LS_PIC-TYPE = 'E'.
            LS_PIC-MESSAGE = '获取图片失败'.
          ELSE.
            LS_PIC-TYPE = 'S'.
            LS_PIC-MESSAGE = '获取图片成功'.
  
            READ TABLE ACCESS_INFO INTO LS_PIC-ACCESS_INFO INDEX 1.
  
            LOOP AT CONTENT_BIN INTO DATA(LS_CONTENT_BIN).
              LS_PIC-PIC = |{ LS_PIC-PIC }{ LS_CONTENT_BIN-LINE }|.
            ENDLOOP.
  
            DATA: L_BUFFER TYPE XSTRING.
  
            CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
              EXPORTING
                INPUT_LENGTH = LS_PIC-ACCESS_INFO-COMP_SIZE
              IMPORTING
                BUFFER       = L_BUFFER
              TABLES
                BINARY_TAB   = CONTENT_BIN[]
              EXCEPTIONS
                FAILED       = 1
                OTHERS       = 2.
          ENDIF.
        ENDIF.
        APPEND LS_PIC TO LS_DATA_OUT-PICTURE.
      ENDLOOP.
  
    ENDIF.
  
    JSON = /UI2/CL_JSON=>SERIALIZE( DATA = LS_DATA_OUT PRETTY_NAME = 'X' ).
  
  ENDFORM.
  
  ```

- **使用二进制数据上传图片**

  ```abap
  "上载图片
  FORM FRM_UPLOAD_PICTURE TABLES ITAB USING JSON.
  
    DATA: BEGIN OF LS_PICTURE,
            PERNR TYPE PERNR_D,
            PIC   TYPE STRING,
          END OF LS_PICTURE.
  
    DATA: BEGIN OF LS_DATA,
            PERNR   TYPE PERNR_D,
            TYPE    TYPE CHAR1,
            MESSAGE TYPE CHAR255,
          END OF LS_DATA.
  
    DATA: LT_PICTURE     LIKE TABLE OF LS_PICTURE,
          LV_STOR_CAT    TYPE SDOKSTCA-STOR_CAT,
          LV_CREP_ID     TYPE CHAR2 VALUE 'A2',
          LV_DOC_ID      TYPE CHAR40,
          LV_OBJECT_ID   TYPE SAPB-SAPOBJID,
          LV_XSTRING     TYPE XSTRING,
          LT_ACCESS_INFO TYPE TABLE OF SCMS_ACINF,
          LT_CONTENT_BIN TYPE TABLE OF SDOKCNTBIN.
  
    DATA: BEGIN OF LS_DATA_OUT.
            INCLUDE STRUCTURE GS_DATA_OUT.
    DATA:   DATA LIKE TABLE OF LS_DATA.
    DATA:END OF LS_DATA_OUT.
  
    LT_PICTURE  = ITAB[].
    LS_DATA_OUT-TYPE = 'S'.
    LS_DATA_OUT-MESSAGE = '接收成功'.
  
    LOOP AT LT_PICTURE INTO LS_PICTURE.
      REFRESH LT_CONTENT_BIN.
      CLEAR LS_DATA.
      LS_DATA-PERNR = LS_PICTURE-PERNR.
      LV_XSTRING = LS_PICTURE-PIC.
      PERFORM FRM_STRING_TO_FTEXT TABLES LT_CONTENT_BIN USING LV_XSTRING.
      LT_ACCESS_INFO = VALUE #( (  COMP_ID = 'data'
                                   MIMETYPE = 'image/jpeg'
                                   COMP_SIZE = STRLEN( LS_PICTURE-PIC ) / 2
                                   BINARY_FLG = 'X'
                                   FIRST_LINE = 1
                                   LAST_LINE = LINES( LT_CONTENT_BIN ) ) ).
  
      CALL FUNCTION 'SCMS_DOC_CREATE'
        EXPORTING
          STOR_CAT              = LV_STOR_CAT
          CREP_ID               = LV_CREP_ID
        IMPORTING
          DOC_ID_OUT            = LV_DOC_ID
        TABLES
          ACCESS_INFO           = LT_ACCESS_INFO
          CONTENT_BIN           = LT_CONTENT_BIN
        EXCEPTIONS
          BAD_STORAGE_TYPE      = 1
          BAD_REQUEST           = 2
          UNAUTHORIZED          = 3
          FORBIDDEN             = 4
          CONFLICT              = 5
          INTERNAL_SERVER_ERROR = 6
          ERROR_HTTP            = 7
          ERROR_SIGNATURE       = 8
          ERROR_CONFIG          = 9
          ERROR_HIERARCHY       = 10
          ERROR_FORMAT          = 11
          ERROR_PARAMETER       = 12
          ERROR                 = 13
          BLOCKED_BY_POLICY     = 14
          OTHERS                = 15.
  
      IF SY-SUBRC NE 0.
        LS_DATA-TYPE = 'E'.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          INTO LS_DATA-MESSAGE WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  
      ELSE.
        LV_OBJECT_ID = |{ LS_PICTURE-PERNR }0002|.
        CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
          EXPORTING
            ARCHIV_ID             = 'A2'
            ARC_DOC_ID            = LV_DOC_ID
            AR_OBJECT             = 'HRICOLFOTO'
            OBJECT_ID             = LV_OBJECT_ID
            SAP_OBJECT            = 'PREL'
            DOC_TYPE              = 'JPG'
          EXCEPTIONS
            ERROR_CONNECTIONTABLE = 1
            OTHERS                = 2.
  
        IF SY-SUBRC NE 0.
          LS_DATA-TYPE = 'E'.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            INTO LS_DATA-MESSAGE WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          LS_DATA-TYPE = 'S'.
          LS_DATA-MESSAGE = '更新成功'.
        ENDIF.
      ENDIF.
      APPEND LS_DATA TO LS_DATA_OUT-DATA.
  
    ENDLOOP.
  
    JSON = /UI2/CL_JSON=>SERIALIZE( DATA = LS_DATA_OUT PRETTY_NAME = 'X' ).
  ENDFORM.
  ```

- **使用本地文件夹进行照片上传**

  ```ABAP
  *&---------------------------------------------------------------------*
  *& Report ZHR_REP_001
  *&---------------------------------------------------------------------*
  *&
  *&---------------------------------------------------------------------*
  REPORT ZHRB_003.
  TYPES: BEGIN OF TYP_PHOTO,
           PERNR(8),
           FLAG     TYPE C,
         END OF TYP_PHOTO.
  *&---------------------------------------------------------------------*
  *&  类型定义
  *&
  *&---------------------------------------------------------------------*
  
  DATA IT_PHOTO TYPE TABLE OF TYP_PHOTO WITH HEADER LINE.
  DATA:
    G_SAPOBJID LIKE SAPB-SAPOBJID,
    G_SAPPFAD  LIKE SAPB-SAPPFAD,
    G_FLAG     TYPE C.
  
  *&---------------------------------------------------------------------*
  *&                   SELECTION-SCREEN
  *&
  *&---------------------------------------------------------------------*
  
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: P_PATH LIKE RLGRAP-FILENAME OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK B1.
  
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
    CALL FUNCTION 'TMP_GUI_BROWSE_FOR_FOLDER'
      IMPORTING
        SELECTED_FOLDER = P_PATH.     "FOLDER.
  
  
  *----------------------------------------------------------------------*
  *                  AT SELECTION-SCREEN
  *----------------------------------------------------------------------*
  
  START-OF-SELECTION.
  
  *-------------------导入数据-----------------------*
  
  
    PERFORM FRM_GETDAT_TXT.
  
  
  *----------------------------------------------------------*
  *判断INFNR是否存在，存在则修改数据，否则创建数据
  *
  *----------------------------------------------------------*
  
    PERFORM FRM_CHECK.
    PERFORM FRM_UPLOAD_PHOTO.
  
  *&---------------------------------------------------------------------*
  *&      Form  FRM_UPLOAD_PHOTO
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  
  FORM FRM_UPLOAD_PHOTO .
    DATA: LS_ZHRT_065 TYPE ZHRT_065,
          LT_ZHRT_065 TYPE TABLE OF ZHRT_065.
    DATA: L_MES TYPE STRING.
    WRITE:/ '人员编号  ' ,'               导入状态' .
    SKIP 2.
    LOOP AT IT_PHOTO.
      IF IT_PHOTO-FLAG EQ 'X'.
        CONCATENATE P_PATH  '\' IT_PHOTO-PERNR '.JPG'
        INTO G_SAPPFAD.
  
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = IT_PHOTO-PERNR
          IMPORTING
            OUTPUT = IT_PHOTO-PERNR.
  
        CONCATENATE IT_PHOTO-PERNR '0002' INTO G_SAPOBJID.
        CONDENSE G_SAPOBJID NO-GAPS.
        PERFORM FRM_DELETE_PHOTO.
        CALL FUNCTION 'ARCHIV_CREATE_FILE'
          EXPORTING
            AR_OBJECT               = 'HRICOLFOTO'
            OBJECT_ID               = G_SAPOBJID
            SAP_OBJECT              = 'PREL'
            DOC_TYPE                = 'JPG'
            PATH                    = G_SAPPFAD
          EXCEPTIONS
            ERROR_CONECTIONTABLE    = 1
            ERROR_PARAMETER         = 2
            ERROR_ARCHIV            = 3
            ERROR_UPLOAD            = 4
            ERROR_KERNEL            = 5
            NO_ENTRY_POSSIBLE       = 6
            ERROR_COMUNICATIONTABLE = 7
            OTHERS                  = 8
  *         OTHERS                  = 9
          .
        IF SY-SUBRC <> 0.
          CONCATENATE ':     导入失败.错误信息:' SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO L_MES.
          WRITE: / IT_PHOTO-PERNR COLOR 2 ,L_MES COLOR 7 .
          ULINE.
          CONTINUE.
        ENDIF.
  
        CLEAR LS_ZHRT_065.
        LS_ZHRT_065-PERNR    = IT_PHOTO-PERNR.
        LS_ZHRT_065-ZHR_TYPE = '2'.
        APPEND LS_ZHRT_065 TO LT_ZHRT_065.
  
        IF G_FLAG EQ 'X'.
          WRITE: / IT_PHOTO-PERNR  COLOR 2, ':     导入成功.原来的照片被替换' .
          ULINE.
        ELSE.
          WRITE: / IT_PHOTO-PERNR  COLOR 2, ':     导入成功.' .
          ULINE.
        ENDIF.
      ELSE.
        WRITE: / IT_PHOTO-PERNR  COLOR 2, ':     人员数据不存在,请先维护人员数据,再重新导入' COLOR 5 .
        ULINE.
      ENDIF.
    ENDLOOP.
  
    IF LT_ZHRT_065 IS NOT INITIAL.
      MODIFY ZHRT_065 FROM TABLE LT_ZHRT_065.
    ENDIF.
  
  ENDFORM. " FRM_BDC_CALL
  
  *&---------------------------------------------------------------------*
  *&      Form  FRM_GETDAT_TXT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  
  FORM FRM_GETDAT_TXT .
    DATA: L_NAME      TYPE STRING,
          L_TAB       TYPE STANDARD TABLE OF FILE_INFO,
          L_LINE      TYPE I,
          L_CHAR1(20),
          L_CHAR2(20).
  
    DATA: WA_TAB TYPE FILE_INFO .
    L_NAME = P_PATH.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
      EXPORTING
        DIRECTORY                   = L_NAME
        FILTER                      = '*.JPG'
  *     FILES_ONLY                  =
  *     DIRECTORIES_ONLY            =
      CHANGING
        FILE_TABLE                  = L_TAB
        COUNT                       = L_LINE
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
    ENDIF.
    LOOP AT L_TAB INTO WA_TAB.
      SPLIT WA_TAB-FILENAME AT '.' INTO L_CHAR1 L_CHAR2.
      IT_PHOTO-PERNR = L_CHAR1.
  
      APPEND IT_PHOTO.
    ENDLOOP.
  
  ENDFORM. " FRM_GETDAT_TXT
  
  *&---------------------------------------------------------------------*
  *&      Form  FRM_CHECK
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  
  FORM FRM_CHECK .
    DATA:L_0002 LIKE TABLE OF PA0002 WITH HEADER LINE.
    FIELD-SYMBOLS: <WA> LIKE IT_PHOTO.
    SELECT *
    INTO TABLE L_0002
    FROM PA0002.
    LOOP AT IT_PHOTO ASSIGNING <WA>.
      CLEAR L_0002.
      READ TABLE L_0002 WITH KEY PERNR = <WA>-PERNR.
      IF SY-SUBRC EQ 0.
        <WA>-FLAG = 'X'.
      ENDIF.
    ENDLOOP.
  ENDFORM. " FRM_CHECK
  
  *&---------------------------------------------------------------------*
  *&      Form  FRM_DELETE_PHOTO
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  
  FORM FRM_DELETE_PHOTO .
    DATA: AA TYPE I.
    CALL FUNCTION 'ARCHIV_DELETE_META'
      EXPORTING
  *     ARCHIV_ID                = ' '
  *     ARC_DOC_ID               = ' '
        AR_OBJECT                = 'HRICOLFOTO'
        DELETE_FLAG              = 2
        OBJECT_ID                = G_SAPOBJID
        SAP_OBJECT               = 'PREL'
  *     CLIENT                   =
  *     SINGLE_ENTRY             = ' '
  *     DOCUMENTCLASS            =
  *     NO_AUTH_CHECK            =
      IMPORTING
        ALL_CONNECTIONS_DELETED  = AA
      EXCEPTIONS
        ERROR_CONNECTIONTABLE    = 1
        ERROR_PARAMETER          = 2
        ERROR_ARCHIV             = 3
        ERROR_KERNEL             = 4
        ERROR_COMMUNICATIONTABLE = 5
        ERROR_AUTHORITY          = 6
        OTHERS                   = 7.
    IF SY-SUBRC EQ 0.
      G_FLAG = 'X'.
    ENDIF.
  ENDFORM. " FRM_DELETE_PHOTO
  ```

## 结构化授权读取 ##

```abap
"获取结构化的组织架构
DATA: LD_SELECTED_PLVAR	     TYPE OBJEC-PLVAR,
      LD_ACT_SEARCH_OTYPE	   TYPE OBJEC-OTYPE,
      IT_SELECTED_OBJECTS	   TYPE STANDARD TABLE OF HRSOBID,
      WA_SELECTED_OBJECTS	   LIKE LINE OF IT_SELECTED_OBJECTS,
      LD_SELECTED_OTYPE	     TYPE OBJEC-OTYPE,
      LD_ACT_SEARCH_WEGID	   TYPE GDSTR-WEGID,
      LD_SELECTED_OBJID	     TYPE OBJEC-REALO,
      LD_ACT_SEARCH_SVECT	   TYPE GDSTR-SVECT,
      LD_CHANGED_FLAG	       TYPE HRPP0C-TEST,
      LD_SET_MODE	           TYPE OBJEC-HISTO,
      LD_LAST_OK_CODE	       TYPE T77FC-FCODE,
      LD_ACT_ROOT_OT         TYPE OBJEC-OTYPE,
      LD_ACT_ROOT_ID         TYPE STRING,
      LD_ACT_PLVAR           TYPE OBJEC-PLVAR,
      LD_ACT_SEARCH_BEGDA	   TYPE OBJEC-BEGDA,
      LD_ACT_SEARCH_ENDDA	   TYPE OBJEC-ENDDA,
      LD_NO_SEARK	           TYPE OBJEC-HISTO,
      LD_ACT_LIST_TYPE       TYPE STRING,
      LD_ACT_INT_WEGID       TYPE HRRHAS-77AW_INT,
      LD_SELECTED_OBJ_APPEND TYPE HRPP0C-TEST,
      LD_CHANGE_SEARCH_TYPE	 TYPE HRPP0C-TEST,
      LD_RESTRICT_CALLBACK   TYPE TFDIR-FUNCNAME.

CALL FUNCTION 'RH_TYPE_STRUC_HELP'
  EXPORTING
    ACT_SEARCH_OTYPE         = LD_ACT_SEARCH_OTYPE
*   act_search_wegid         = ld_act_search_wegid
*   act_search_svect         = ld_act_search_svect
*   set_mode                 = ld_set_mode
*   act_root_ot              = ld_act_root_ot
*   act_root_id              = ld_act_root_id
*   act_plvar                = ld_act_plvar
*   act_search_begda         = ld_act_search_begda
*   act_search_endda         = ld_act_search_endda
*   no_seark                 = ld_no_seark
*   act_list_type            = ld_act_list_type
*   act_int_wegid            = ld_act_int_wegid
*   selected_obj_append      = ld_selected_obj_append
*   change_search_type       = ld_change_search_type
*   restrict_callback        = ld_restrict_callback
  IMPORTING
    SELECTED_PLVAR           = LD_SELECTED_PLVAR
    SELECTED_OTYPE           = LD_SELECTED_OTYPE
    SELECTED_OBJID           = LD_SELECTED_OBJID
    CHANGED_FLAG             = LD_CHANGED_FLAG
    LAST_OK_CODE             = LD_LAST_OK_CODE
* TABLES
*   selected_objects         = it_selected_objects
  EXCEPTIONS
    NO_ACTIVE_PLVAR          = 1
    NO_OBJECT_SELECTED       = 2
    NO_STRUC_SEARCH_POSSIBLE = 3.
```

## 读取0041时间数据

```ABAP
DATA: MESSAGE_HANDLER TYPE REF TO IF_HRPA_MESSAGE_HANDLER,
      LS_P0041        TYPE P0041.
      
MOVE-CORRESPONDING P0041 TO LS_P0041.

CALL FUNCTION 'HR_ECM_READ_IT0041_DATE_TYPE'
   EXPORTING
     DATAR           = LS_DATA_IN-DATAR   "时间类型
     P0041           = LS_P0041           "时间数据
     MESSAGE_HANDLER = MESSAGE_HANDLER 
   IMPORTING
     DATE            = LS_DATA_OUT-DATA-DARDT.
```





