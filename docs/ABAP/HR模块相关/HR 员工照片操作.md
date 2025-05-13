[TOC]

# 上传照片 #

* **本地照片上传**

  ```abap
  FORM FRM_UPLOAD_PERNR_PIC USING SAP_OBJID TYPE SAEOBJID PATH TYPE SAEPFAD.
    "SAP_OBJID: 人员编号加信息类型
    "PATH     : 本地文件路径
  
  
    CALL FUNCTION 'ARCHIV_CREATE_FILE'
      EXPORTING
        AR_OBJECT               = 'HRICOLFOTO'
        OBJECT_ID               = SAP_OBJID
        SAP_OBJECT              = 'PREL'
        DOC_TYPE                = 'JPG'
        PATH                    = PATH
      EXCEPTIONS
        ERROR_CONECTIONTABLE    = 1
        ERROR_PARAMETER         = 2
        ERROR_ARCHIV            = 3
        ERROR_UPLOAD            = 4
        ERROR_KERNEL            = 5
        NO_ENTRY_POSSIBLE       = 6
        ERROR_COMUNICATIONTABLE = 7
        OTHERS                  = 8.
  
  ENDFORM.
  ```

* **接口图片上传**

  1. **16 进制转 2 进制**

     ```abap
     DATA: LT_CONTENT_BIN TYPE TABLE OF SDOKCNTBIN,
           LV_XSTRING     TYPE XSTRING.
     
     PERFORM FRM_STRING_TO_FTEXT TABLES LT_CONTENT_BIN USING LV_XSTRING.
     "16进制转2进制
     FORM FRM_STRING_TO_FTEXT TABLES FTEXT_TAB USING TEXT .
       
       DATA: LENGTH TYPE I,
             BUFFER TYPE XSTRING.
     
       REFRESH FTEXT_TAB.
       CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
         EXPORTING
           BUFFER        = TEXT
         IMPORTING
           OUTPUT_LENGTH = LENGTH
         TABLES
           BINARY_TAB    = FTEXT_TAB.
     
     ENDFORM.
     ```

  2. **上传文件**

     ```abap
     DATA: LT_ACCESS_INFO TYPE TABLE OF SCMS_ACINF,
           LV_STOR_CAT    TYPE SDOKSTCA-STOR_CAT,
           LV_CREP_ID     TYPE CHAR2 VALUE 'A2',
           LV_DOC_ID      TYPE CHAR40.
     
     LT_ACCESS_INFO = VALUE #( (  COMP_ID = 'data'
                                  MIMETYPE = 'image/jpeg'
                                  COMP_SIZE = STRLEN( LV_XSTRING ) / 2
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
     ```

  3. **建立链接**

     ```abap
     CHECK SY-SUBRC EQ 0.
     
     DATA: LV_OBJECT_ID TYPE SAPB-SAPOBJID. "人员编号加信息类型
     LV_OBJECT_ID = |{ PERNR }0002|.
     
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
     ```

# 获取照片 #

1. **判断照片是否存在**

   ```abap
   DATA: P_EXISTS       TYPE C,
         P_CONNECT_INFO TYPE TOAV0.
         
   CALL FUNCTION 'HR_IMAGE_EXISTS'
     EXPORTING
       P_PERNR               = LS_PERNR
     IMPORTING
       P_EXISTS              = P_EXISTS
       P_CONNECT_INFO        = P_CONNECT_INFO
     EXCEPTIONS
       ERROR_CONNECTIONTABLE = 1
       OTHERS                = 2.      
   ```

2. **获取员工照片**

   ```abap
   IF SY-SUBRC NE 0 AND P_EXISTS IS INITIAL.
     LS_PIC-TYPE = 'E'.
     LS_PIC-MESSAGE = '图片不存在'.
   ELSE.
   
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
     ENDIF.
   ENDIF.
   ```

3. **二进制转 16 进制**

   ```abap
   CHECK CONTENT_BIN[] IS NOT INITIAL.
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
   ```


# 照片内容存储表

- 文件存储表： `SDOKCONT1`

- 可在 T-Code：`OAC0` 中查看相关配置：

  `OAC0` 进入后双击 Z1 查看

  ![照片文件存储表-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240522160244617.png)

  双击后即可看见内容表内容

  ![照片文件存储表-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240522160321473.png)

