[TOC]

# HR常用BAPI #

后续可以使用相关类 `CL_HRBAS *`,待研究

## 读取组织架构 ##

1. **常用评估路径**

   评估路径存储表`T778A`,评估路径文本存储表`T778T`.

   | 评估路径     | 描述                           |
   | ------------ | ------------------------------ |
   | **O-O_DOWN** | 读取组织单位下所有的组织单位   |
   | **ORGA-UP**  | 读取组织单位的组织架构（完整） |
   | **O-O-S**    | 读取组织单位下所有的职位       |
   | **O-O-P**    | 读取组织单位下所有的员工       |
   | **O-O**      | 读取组织单位的直属上级组织单位 |
   | **S-O**      | 职位所属部门                   |
   | **P_S_S_C**  | 人员相关的职位职务信息         |
   | **P-S-C-O**  | 人员相关的职位职务组织单位信息 |
   | **P-S-O-O**  | 人员的组织架构信息(完整)       |
   | **O-P**      | 查询直接挂在当前组织下的员工   |

```ABAP
DATA:ld_act_plvar        TYPE objec-plvar,
     ld_act_otype        TYPE objec-otype,
     it_result_tab       TYPE STANDARD TABLE OF swhactor,
     wa_result_tab       LIKE LINE OF it_result_tab,
     ld_act_objid        TYPE string,
     it_result_objec     TYPE STANDARD TABLE OF objec,
     wa_result_objec     LIKE LINE OF it_result_objec,
     ld_act_wegid        TYPE gdstr-wegid,
     it_result_struc     TYPE STANDARD TABLE OF struc,
     wa_result_struc     LIKE LINE OF it_result_struc,
     ld_act_int_flag     TYPE hrrhas-77aw_int,
     ld_act_plvar        TYPE objec-plvar,
     ld_act_begda        TYPE objec-begda,
     ld_act_endda        TYPE objec-endda,
     ld_act_tdepth       TYPE hrrhas-tdepth,
     ld_act_tflag        TYPE hrrhas-tflag,
     ld_act_vflag        TYPE hrrhas-vflag,
     ld_authority_check  TYPE hrrhas-authy,
     ld_text_buffer_fill TYPE hrpp0c-test,
     ld_buffer_mode      TYPE flag.

CALL FUNCTION 'RH_STRUC_GET'
  EXPORTING
    act_otype        = ld_act_otype         "对象类型 O/S/P
    act_objid        = ld_act_objid         "对象标识
    act_wegid        = ld_act_wegid         "评估路径
    act_int_flag     = ld_act_int_flag      "???未知
    act_plvar        = ld_act_plvar         "活动版本: 01
    act_begda        = ld_act_begda         "开始日期
    act_endda        = ld_act_endda         "结束日期
    act_tdepth       = ld_act_tdepth        "层级,默认为0(取全部层级,一般自身算一层,若只读下一层则填2)
    act_tflag        = ld_act_tflag         "提供文本,默认 'X'
    act_vflag        = ld_act_vflag         "提供关系信息,默认'X'
    authority_check  = ld_authority_check   "权限检查,默认'X'
    text_buffer_fill = ld_text_buffer_fill  "????未知
    buffer_mode      = ld_buffer_mode       "????未知
  IMPORTING
    act_plvar        = ld_act_plvar         "计划版本
  TABLES
    result_tab       = it_result_tab        "结果表:所有对象
    result_objec     = it_result_objec      "结果表:所有对象的基础信息(例如:文本信息)
    result_struc     = it_result_struc      "结果表:所有对象的层次架构关系
  EXCEPTIONS
    no_plvar_found   = 1
    no_entry_found   = 2
    OTHERS           = 3.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 2. "Exception
  "Add code for exception here
ENDIF.
```

## 权限检查 ##

### 结构化权限检查 ###

不仅包含标准的权限管理,还包含基于组织架构的权限控制 —— 即结构化权限,区别于常用的PFCG权限控制模式,结构化权限控制以组织结构对象为控制对象.并可以控制评估路径(根据评估路径,可获取不同的结构化数据).然后再分配给特定的人员,权限控制更为精准方便.

```abap
DATA:ld_fcode	     TYPE string,
     ld_plvar        TYPE string,
     ld_otype        TYPE string,
     ld_objid        TYPE string,
     ld_with_base_ac TYPE string.

CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
  EXPORTING
    fcode                    = ld_fcode "默认DISP: DISP(显示) CUTI(定界) INSE(创建) DELO(删除) 具体值可查看T77FC
    plvar                    = ld_plvar "计划版本: 默认 01
    otype                    = ld_otype "对象类型: O/S/P
    objid                    = ld_objid "对象标识: 
    with_base_ac             = ld_with_base_ac "默认 X 
  EXCEPTIONS
    no_stru_authority        = 1
    no_stru_authority_hyper  = 2
    no_stru_authority_at_all = 3
    no_base_authority        = 4
    OTHERS                   = 5.

IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
```

### PA权限检查 ###

```abap
DATA: ld_tclas TYPE pspar-tclas,
      it_i0001 TYPE STANDARD TABLE OF p0001,
      wa_i0001 LIKE LINE OF it_i0001,
      ld_pernr TYPE prelp-pernr,
      ld_infty TYPE prelp-infty,
      ld_subty TYPE prelp-subty,
      ld_begda TYPE prelp-begda,
      ld_endda TYPE prelp-endda,
      ld_level TYPE authc_d,
      ld_uname TYPE syuname.



CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY'
  EXPORTING
    tclas            = ld_tclas  "默认A
    pernr            = ld_pernr
    infty            = ld_infty
    subty            = ld_subty
    begda            = ld_begda
    endda            = ld_endda
    level            = ld_level  "默认R
    uname            = ld_uname 
  TABLES
    i0001            = it_i0001
  EXCEPTIONS
    no_authorization = 1
    internal_error   = 2
    OTHERS           = 3.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 2. "Exception
  "Add code for exception here
ENDIF.
```



### 基本权限检查 ###

```abap
DATA: ld_fcode TYPE string,
      ld_plvar TYPE string,
      ld_otype TYPE string,
      ld_infty TYPE string,
      ld_subty TYPE string,
      ld_istat TYPE string.

CALL FUNCTION 'RH_BASE_AUTHORITY_CHECK'
  EXPORTING
    fcode             = ld_fcode  "默认DISP: DISP(显示) CUTI(定界) INSE(创建) DELO(删除) 具体值可查看T77FC
    plvar             = ld_plvar  "计划版本: 默认 01
    otype             = ld_otype  "对象类型: O/S/P
    infty             = ld_infty  "信息类型:
    subty             = ld_subty  "子类型:
    istat             = ld_istat  "
  EXCEPTIONS
    no_base_authority = 1
    OTHERS            = 2.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ENDIF.
```

## 人事操作 ##

### 无人员编号操作 ###

```abap
PERFORM frm_write_infty.

FORM frm_write_infty.

  DATA: ld_return_tab	     TYPE hrpad_return_tab,
        ld_employeenumber	 TYPE pernr_d,
        ld_bapipakey_tab   TYPE hrpad_bapipakey_tab,
        ld_referencepernr	 TYPE pernr_d,
        ld_is_ok           TYPE boole_d,
        ld_hiringdate	     TYPE begda,
        ld_actiontype	     TYPE massn,
        ld_reasonforaction TYPE massg,
        ld_pnnnn_tab       TYPE prelp_tab,
        ld_pref_tab	       TYPE pref_tab,
        ld_nocommit	       TYPE flag.


  DATA: ls_p0000 TYPE p0000,
        ls_p0001 TYPE p0001,
        ls_p0002 TYPE p0001.

  PERFORM per_pnnnn_to_prelp TABLES ld_pnnnn_tab USING ls_p0000 '0000'.
  PERFORM per_pnnnn_to_prelp TABLES ld_pnnnn_tab USING ls_p0001 '0001'.
  PERFORM per_pnnnn_to_prelp TABLES ld_pnnnn_tab USING ls_p0002 '0002'.

  ld_hiringdate = ls_p0000-begda.
  ld_actiontype = ls_p0000-massn.
  ld_actiontype = ls_p0000-massg.

  CALL FUNCTION 'HR_PAD_HIRE_EMPLOYEE'
    EXPORTING
      employeenumber  = ld_employeenumber
      referencepernr  = ld_referencepernr
      hiringdate      = ld_hiringdate
      actiontype      = ld_actiontype
      reasonforaction = ld_reasonforaction
      pnnnn_tab       = ld_pnnnn_tab
      pref_tab        = ld_pref_tab
      nocommit        = ld_nocommit   "是否提交
    IMPORTING
      return_tab      = ld_return_tab
      bapipakey_tab   = ld_bapipakey_tab
      is_ok           = ld_is_ok.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFORM.

FORM per_pnnnn_to_prelp  TABLES  it_prelp STRUCTURE prelp
                          USING  is_pnnnn
                                 iv_infty.
  DATA: ls_prelp TYPE prelp.

  CALL METHOD cl_hr_pnnnn_type_cast=>pnnnn_to_prelp
    EXPORTING
      pnnnn = is_pnnnn
    IMPORTING
      prelp = ls_prelp.

  ls_prelp-infty = iv_infty.

  APPEND ls_prelp TO it_prelp.
ENDFORM.
```

### 有人员编号操作 ###

```abap
DATA: ld_return	TYPE bapireturn1,
      ld_number	TYPE bapip0001-pernr.

DATA: ld_infty            TYPE prelp-infty,
      ld_key              TYPE bapipakey,
      ld_subtype          TYPE p0001-subty,
      ld_objectid	        TYPE p0001-objps,
      ld_lockindicator    TYPE p0001-sprps,
      ld_validityend      TYPE p0001-endda,
      ld_validitybegin    TYPE p0001-begda,
      ld_recordnumber	    TYPE p0001-seqnr,
      ld_record	          TYPE string,
      ld_operation        TYPE pspar-actio,
      ld_tclas            TYPE pspar-tclas,
      ld_dialog_mode      TYPE c,
      ld_nocommit	        TYPE bapi_stand-no_commit,
      ld_view_identifier  TYPE p0003-viekn,
      ld_secondary_record	TYPE string.

CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
  EXPORTING
    number = ld_number
  IMPORTING
    return = ld_return.

IF sy-subrc EQ 0.
ENDIF.

CALL FUNCTION 'HR_INFOTYPE_OPERATION'
  EXPORTING
    infty            = ld_infty
    number           = ld_number
    subtype          = ld_subtype
    objectid         = ld_objectid
    lockindicator    = ld_lockindicator
    validityend      = ld_validityend
    validitybegin    = ld_validitybegin
    recordnumber     = ld_recordnumber
    record           = ld_record
    operation        = ld_operation
    tclas            = ld_tclas
    dialog_mode      = ld_dialog_mode
    nocommit         = ld_nocommit
    view_identifier  = ld_view_identifier
    secondary_record = ld_secondary_record
  IMPORTING
    return           = ld_return
    key              = ld_key.

IF sy-subrc EQ 0.
ENDIF.


CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait   = abap_true
  IMPORTING
    return = ld_return.


CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
  EXPORTING
    number = ld_number
  IMPORTING
    return = ld_return.
```

### 其他常用函数 ###

| 函数                                 | 描述                                                         |
| ------------------------------------ | ------------------------------------------------------------ |
| **`HR_READ_INFOTYPE`**               | 读取某个员工的某个信息类型数据                               |
| **`HR_PSBUFFER_INITIALIZE`**         | 清空缓存.在使用`hr_infotype_operation`循环批量更新信息类型时,需要用于清空缓存,否则有可能会出现意想不到的问题 |
| **`HR_INFOTYPE_OPERATION`**          | 信息类型数据更新,可<br />INS      –  插入数据<br />DEL     –  删除数据<br />MOD   –  更新执行<br />CHK    –  模拟执行<br />==注:更新或者删除时,请指定全关键字== |
| **`BAPI_EMPLOYEE_ENQUEUE`**          | 锁定员工,对员工操作前,都必须锁定                             |
| **`BAPI_EMPLOYEE_DEQUEUE`**          | 解除锁定                                                     |
| **`HR_CHECK_AUTHORITY_INFTY`**       | HR PA权限检查                                                |
| **`HR_READ_INFOTYPE_AUTHC_DISABLE`** | 跳过读权限,如果需要跳过权限,每次抵用`HR_READ_INFOTYPE`前都需要调用一次 |
| **`HR_ECM_READ_IT0041_DATE_TYPE`**   | 查询0041的日期                                               |

### 成本分配 ###

```abap
" 0014 和 0015 中成本分配存储的表是: ASSHR 和 ASSHR. 其数据库视图是 ASSOB_HR

" 查询时可以根据信息类型的主键先从 ASSHR 中获取 PDSNR, 再根据获取的 PDSNR 去表 ASSHR 查询
DATA: ld_number	       TYPE string,
      ld_nr_range_nr   TYPE inri-nrrangenr VALUE '01',
      ld_quantity	     TYPE inri-quantity VALUE '1',
      ld_object	       TYPE inri-object VALUE 'PD_SEQ_NR',
      ld_returncode	   TYPE inri-returncode,
      ld_subobject     TYPE string,
      ld_toyear	       TYPE inri-toyear,
      ld_ignore_buffer TYPE string VALUE 'X'.

"新增时获取下一个编号
CALL FUNCTION 'NUMBER_GET_NEXT'
  EXPORTING
    nr_range_nr             = ld_nr_range_nr
    object                  = ld_object
    quantity                = ld_quantity
    subobject               = ld_subobject
    toyear                  = ld_toyear
    ignore_buffer           = ld_ignore_buffer
  IMPORTING
    number                  = ld_number
    quantity                = ld_quantity
    returncode              = ld_returncode
  EXCEPTIONS
    interval_not_found      = 1
    number_range_not_intern = 2
    object_not_found        = 3
    quantity_is_0           = 4
    quantity_is_not_1       = 5
    interval_overflow       = 6
    buffer_overflow         = 7.
    
" 根据获取的编号进行更改、插入或删除的动作.
DATA:lv_ipdsnr LIKE pdsnr-pdsnr ,
     ls_ipref  LIKE pref,
     lv_iopera LIKE sy-msgty.

lv_ipdsnr = ld_number.

ls_ipref = VALUE #(
  pernr = '70023000'
  infty = '0015'
  subty = '3007'
  begda = '20220124'
  endda = '20220124'
  bukrs = '1100'
  kokrs = 'CCTC'
  posnr = '00014503' "原值是 00014459
  kostl = ''

).

lv_iopera = 'U'." U: 更改 I:插入 D:删除

CALL FUNCTION 'RP_PLANT_DATA_UPDATE_TABLES'
  EXPORTING
    ipdsnr                         = lv_ipdsnr
    ipref                          = ls_ipref
    iopera                         = lv_iopera
    update_coift                   = 'X'
  EXCEPTIONS
    insert_asshr_not_possible      = 1
    insert_coift_not_possible      = 2
    insert_assob_not_possible      = 3
    delete_asshr_not_possible      = 4
    delete_assob_not_possible      = 5
    delete_coift_not_possible      = 6
    update_assob_not_possible      = 7
    nearly_last_entry_out_of_range = 8
    last_entry_out_of_number_range = 9
    no_more_numbers_available      = 10
    interval_not_found             = 11
    number_range_not_found         = 12
    object_not_found               = 13
    quantity_is_0                  = 14
    unknown_error                  = 15
    delete_pdsnr_not_possible      = 16
    insert_pdsnr_not_possible      = 17
    insert_teven_more_not_possible = 18
    delete_teven_more_not_possible = 19
    OTHERS                         = 20.
IF sy-subrc <> 0.
ENDIF.

```

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

  

## 组织操作 ##



```abap
DATA: ld_act_ok_code           TYPE t77fc-fcode,
      ld_act_fcode             TYPE t77fc-fcode,
      it_act_hrtnnnn           TYPE STANDARD TABLE OF string,
      wa_act_hrtnnnn           LIKE LINE OF it_act_hrtnnnn,
      ld_act_pnnnn_out         TYPE string,
      ld_act_plvar             TYPE objec-plvar,
      ld_act_mess_info         TYPE hrrhad_msg,
      ld_act_otype             TYPE objec-otype,
      ld_act_objid             TYPE objec-objid,
      ld_act_infty             TYPE t778t-infty,
      ld_act_subty             TYPE t778u-subty,
      ld_act_istat             TYPE objec-istat,
      ld_act_begda             TYPE objec-begda,
      ld_act_endda             TYPE objec-endda,
      ld_act_new_langu         TYPE hrs1000-langu,
      ld_act_pnnnn             TYPE string,
      ld_act_infotypekey       TYPE hripkey,
      ld_act_dpatt             TYPE pm0d1-dpatt,
      ld_act_gdate             TYPE hrdialog-gdate,
      ld_act_new_histo         TYPE hrdialog-new_histo,
      ld_act_enqueue           TYPE pp0c-enq,
      ld_suppress_dialog       TYPE pppar-dsupr,
      ld_act_vtask             TYPE hrrhap-vtask,
      ld_act_commit_flg	       TYPE hrrhap-commit_flg,
      ld_act_maint             TYPE hrdialog-maint,
      ld_clear_buffer_plog_tab TYPE hrrhap-buffer_clr,
      ld_suppress_integration	 TYPE pppar-suppr_inte,
      ld_act_pppar_exep	       TYPE pppar_exep.

CALL FUNCTION 'RH_PNNNN_MAINTAIN'
  EXPORTING
    act_fcode             = ld_act_fcode
    act_plvar             = ld_act_plvar
    act_otype             = ld_act_otype
    act_objid             = ld_act_objid
    act_infty             = ld_act_infty
    act_subty             = ld_act_subty
    act_istat             = ld_act_istat
    act_begda             = ld_act_begda
    act_endda             = ld_act_endda
    act_new_langu         = ld_act_new_langu
    act_pnnnn             = ld_act_pnnnn
    act_infotypekey       = ld_act_infotypekey
    act_dpatt             = ld_act_dpatt
    act_gdate             = ld_act_gdate
    act_new_histo         = ld_act_new_histo
    act_enqueue           = ld_act_enqueue
    suppress_dialog       = ld_suppress_dialog
    act_vtask             = ld_act_vtask
    act_commit_flg        = ld_act_commit_flg
    act_maint             = ld_act_maint
    clear_buffer_plog_tab = ld_clear_buffer_plog_tab
    suppress_integration  = ld_suppress_integration
    act_pppar_exep        = ld_act_pppar_exep
  IMPORTING
    act_ok_code           = ld_act_ok_code
    act_pnnnn_out         = ld_act_pnnnn_out
    act_mess_info         = ld_act_mess_info
  TABLES
    act_hrtnnnn           = it_act_hrtnnnn
  EXCEPTIONS
    infty_not_valid       = 1
    no_plvar              = 2
    object_not_defined    = 3
    otype_not_valid       = 4
    no_authority          = 5
    action_rejected       = 6
    no_gdate              = 7
    fcode_not_supported   = 8.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 2. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 3. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 4. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 5. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 6. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 7. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 8. "Exception
  "Add code for exception here
ENDIF.
```

### 其他常用函数 ###

| 函数                               | 描述                                                         |
| ---------------------------------- | ------------------------------------------------------------ |
| **`RH_CLEAR_BUFFER`**              | 清空缓存                                                     |
| **`RH_DELETE_INFTY`**              | 删除信息类型数据                                             |
| **`RH_INSERT_INFTY`**              | 信息类型插入数据                                             |
| **`RH_UPDATE_INFTY`**              | 更新信息类型数据                                             |
| **`RH_UPDATE_DATABASE`**           | 提交数据库.如果上述`delete/insert/update`需要整体提交,可在调用时设置参数`VTASK='B'`.然后调佣该函数进行提交<br />S:  同步模式,其实这个描述不正确,应该是实时更新模式.具体来说,当完成更新后.系统会自动调用`COMMIT WORK AND WAIT`直接提交,不受参数`COMMIT_FLG`的制约<br />V:  异步更新,更新完成后,系统会判断`commit_flg`是否设置,如果设置会`COMMIT WORK`.然后继续执行,不会等待更新是否完成<br />B:  更新`BUFFER`,需要调用函数`RH_UPDATE_DATABASE`进行提交数据库操作,可用于每次更新多信息类型数据的需求<br />D:  `DIALOG`模式,受`COMMIT_FLG`制约,看代码,好像和==S==和==V==后台都差不多 |
| **`RH_READ_INFTY_NNNN`**           | 读取OM信息类型数据,该函数没有办法读取信息类型 1000 和 1001   |
| **` RH_READ_INFTY`**               | 读取OM所有信息类型数据                                       |
| **`RH_DELETE_OBJECT`**             | 删除组织对象（岗位，单位，部门等）                           |
| **`RH_OBJECT_CREATE`**             | 创建信息类型1000的数据                                       |
| **`RH_READ_INFTY_1000`**           | 读取信息类型1000的数据                                       |
| **`RH_INSERT_INFTY_1001_EXT`**     | 创建信息类型1001的数据                                       |
| **`RH_READ_INFTY_1001`**           | 读取信息类型1001的数据                                       |
| **`HR_READ_FOREIGN_OBJECT_TEXT'`** | 返回对象文本                                                 |
| **`RH_STRU_AUTHORITY_CHECK`**      | OM结构化权限检查                                             |
| **`RH_BASE_AUTHORITY_CHECK`**      | 基本权限检查                                                 |
| **`RH_STRUC_GET`**                 | 根据评估路径读取指定OM架构信息                               |
| **`HR_ENQUEUE_OBJECT`**            | 锁定待操作的OM对象                                           |
| **`HR_DEQUEUE_OBJECT`**            | 解锁                                                         |

## 考勤常用函数 ##

| 函数                              | 描述                       |
| --------------------------------- | -------------------------- |
| **`HR_TIME_RESULTS_IN_INTERVAL`** | 读取考勤评估记录(**常用**) |
| **`HR_READ_TIMEDATA_PSP`**        | 读取员工每日计划工作时间   |
| **`HR_HK_DIFF_BT_2_DATES`**       | 计算两个日期的差别         |
| **`MONTH_NAMES_GET`**             | 月份名称获取               |
| **`HOLIDAY_CALENDAR_GET`**        | 读取公共假日列表           |
| **`LAST_DAY_OF_MONTHS`**          | 计算指定月份的最后一天     |
| **`HR_PERSONAL_WORK_SCHEDULE`**   | 读取个人考勤计划           |
| **`HR_FORMS_TIM_GET_B2_RESULTS`** | 读取个人考勤记录           |

考勤记录表：`T552A` 。该表按月记录了考勤记录，且每日的考勤记录分别对应 `TPR` 和 `FTK` 的字段。类似于 0041 的日期记录。

以下是一个考勤的示例程序：

```abap
*&---------------------------------------------------------------------*
*& Report ZTEST_0027
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTEST_0027.

TYPE-POOLS: SLIS.

TABLES: PERNR, PCL1, PCL2, T549S.
INFOTYPES: 0000, 0001, 0002, 0007, 2001, 2002, 2003, 0182, 2006, 2010.

DEFINE SET_FCAT.
  LV_POS = LINES( GT_FIELDCAT ) + 1.
  CLEAR:  LS_FIELDCAT.
  LS_FIELDCAT-COL_POS       = LV_POS.
  LS_FIELDCAT-KEY           = &1.
  LS_FIELDCAT-FIELDNAME     = &2.
  LS_FIELDCAT-REPTEXT       = &3.
  LS_FIELDCAT-JUST          = &4.
  LS_FIELDCAT-OUTPUTLEN     = &5.
  LS_FIELDCAT-DO_SUM        = &6.
  LS_FIELDCAT-NO_OUT        = &7.
  LS_FIELDCAT-SCRTEXT_L     = &3.
  LS_FIELDCAT-SCRTEXT_M     = &3.
  LS_FIELDCAT-SCRTEXT_S     = &3.
  APPEND LS_FIELDCAT TO  GT_FIELDCAT.
END-OF-DEFINITION.

DEFINE SET_SORT.
  LV_POS = LINES( GT_SORT ) + 1.
  CLEAR:  LS_SORT.
  LS_SORT-SPOS       = LV_POS.
  LS_SORT-FIELDNAME  = &1.
  LS_SORT-UP         = &2.
  LS_SORT-SUBTOT     = &3.
  APPEND LS_SORT TO GT_SORT.
END-OF-DEFINITION.

TYPES: BEGIN OF TS_DATA,
         ANZHL01 TYPE ANZHL,            "Plan working hours
         ANZHL02 TYPE ANZHL,            "Actual working hours
         ANZHL03 TYPE ANZHL,            "Late
         ANZHL04 TYPE ANZHL,            "Early leave
         ANZHL05 TYPE ANZHL,            "Absenteeism
         ANZHL06 TYPE ANZHL,            "Annual Leave
         ANZHL07 TYPE ANZHL,            "Personal Leave
         ANZHL08 TYPE ANZHL,            "Sick Leave
         ANZHL09 TYPE ANZHL,            "Marriage Leave
         ANZHL10 TYPE ANZHL,            "Maternity Leave
         ANZHL11 TYPE ANZHL,            "Prenatal Leave
         ANZHL12 TYPE ANZHL,            "Nursing Leave
         ANZHL13 TYPE ANZHL,            "The Nurses Leave
         ANZHL14 TYPE ANZHL,            "Bereavement Leave
         ANZHL15 TYPE ANZHL,            "Injury Leave
         ANZHL16 TYPE ANZHL,            "False Temperature Leave
         ANZHL17 TYPE ANZHL,            "Supplementary Holidays in Spring Festival
         ANZHL18 TYPE ANZHL,            "Time of in lieu
         ANZHL19 TYPE ANZHL,            "Overtime 150%
         ANZHL20 TYPE ANZHL,            "Overtime 200%
         ANZHL21 TYPE ANZHL,            "Overtime 300%
         ANZHL22 TYPE ANZHL,            "Overtime time off
         ANZHL23 TYPE ANZHL,            "Overtime Total
         ANZHL24 TYPE ANZHL,            "Business trip
         ANZHL25 TYPE ANZHL,            "Training
         ANZHL26 TYPE ANZHL,            "Out of office
         ANZHL27 TYPE ANZHL,            "Shuttle bus delay
         ANZHL28 TYPE ANZHL,            "Company event
         ANZHL29 TYPE ANZHL,            "Public
         ANZHL30 TYPE ANZHL,            "Benefit leave
         ANZHL31 TYPE ANZHL,            "Late meal subsidy
         ANZHL32 TYPE ANZHL,            "Afternoon shift allowance
         ANZHL33 TYPE ANZHL,            "Night shift allowance
         ANZHL34 TYPE ANZHL,            "Childcare Leave
         ANZHL35 TYPE ANZHL,            "Elderly Care Leave

         ANZHL36 TYPE ANZHL,            "Working Hours Bank
         ANZHL37 TYPE ANZHL,            "Actual Shift Hours
         ANZHL38 TYPE ANZHL,            "Standard Calendar Hours
       END OF TS_DATA.

TYPES: BEGIN OF TS_DATA_LINE,
         BEGDA TYPE D,
         ENDDA TYPE D,
         HOURS TYPE ANZHL,
       END OF TS_DATA_LINE.
TYPES: TT_DATA_LINE TYPE STANDARD TABLE OF TS_DATA_LINE WITH DEFAULT KEY.

DATA: GT_DATA_LINE TYPE TT_DATA_LINE.

FIELD-SYMBOLS: <FS_TABLE> TYPE STANDARD TABLE,
               <FS_STRUC> TYPE ANY.

DATA: GT_FIELDCAT TYPE LVC_T_FCAT,
      GT_SORT     TYPE LVC_T_SORT,
      GS_LAYOUT   TYPE LVC_S_LAYO.

DATA: GT_T552A TYPE TABLE OF T552A.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_COUNT RADIOBUTTON GROUP G1 USER-COMMAND UC1.
    SELECTION-SCREEN COMMENT 2(15) T_COUNT FOR FIELD P_COUNT VISIBLE LENGTH 10.
    PARAMETERS: P_DETAIL RADIOBUTTON GROUP G1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 19(15) T_DETAIL FOR FIELD P_DETAIL VISIBLE LENGTH 10.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP.

  PARAMETERS: P_DAY TYPE NUM DEFAULT '01' MODIF ID S1.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM FRM_INIT_SCREEN.

AT SELECTION-SCREEN OUTPUT.
  PERFORM FRM_SET_SCREEN_OUT.

START-OF-SELECTION.
  PERFORM FRM_CHECK_INPUT.
  PERFORM FRM_GET_OTHER_DATA.
  PERFORM FRM_CREATE_ALV.

GET PERNR.
  PERFORM FRM_GET_DATA_LINES.
  PERFORM FRM_APPEND_PERNR.

END-OF-SELECTION.
  PERFORM FRM_ALV_DISPLAY.


FORM FRM_INIT_SCREEN.
  %_P_DAY_%_APP_%    = '薪资结算开始日期'.
  T_COUNT   = '汇总表'.
  T_DETAIL  = '明细表'.
ENDFORM.

FORM FRM_SET_SCREEN_OUT.
  LOOP AT SCREEN.

    IF SCREEN-GROUP1 EQ 'S1'.
      IF P_COUNT EQ 'X'.
        SCREEN-ACTIVE = '0'.
      ELSE.
        SCREEN-ACTIVE = '1'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

FORM FRM_CHECK_INPUT.

  IF PN-BEGDA = '18000101' OR PN-ENDDA = '99991231'.
    MESSAGE 'Please enter the period.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    STOP.
  ENDIF.

  IF P_DETAIL IS NOT INITIAL AND P_DAY IS INITIAL.
    MESSAGE 'Please enter the split date.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    STOP.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Ready read data...'.

ENDFORM.

FORM FRM_GET_OTHER_DATA.

  IF P_COUNT EQ 'X'.
    P_DAY = '01'.
  ENDIF.

  REFRESH GT_T552A.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T552A FROM T552A
    WHERE ZEITY EQ '2' AND MOFID EQ 'CN' AND MOSID EQ '28' AND SCHKZ EQ 'W120' AND KJAHR GE PN-BEGDA+0(4) AND KJAHR LE PN-ENDDA+0(4).

ENDFORM.

FORM FRM_GET_DATA_LINES.

  DATA: LT_PERIODS LIKE GT_DATA_LINE.

  REFRESH GT_DATA_LINE.

  DATA(LV_INDEX) = 1.
  DATA(LT_P0000) = P0000[].
  SORT LT_P0000 BY BEGDA.

  WHILE LV_INDEX <= LINES( LT_P0000 ).

    DATA(LS_P0000) = LT_P0000[ LV_INDEX ].

    IF LV_INDEX EQ 1 AND PN-BEGDA GE LS_P0000-BEGDA.
      LS_P0000-BEGDA = PN-BEGDA.
    ENDIF.

    IF LV_INDEX EQ LINES( LT_P0000 ) AND LS_P0000-ENDDA GE PN-ENDDA.
      LS_P0000-ENDDA = PN-ENDDA.
    ENDIF.

    IF LS_P0000-STAT2 EQ '3'.
      APPEND INITIAL LINE TO LT_PERIODS ASSIGNING FIELD-SYMBOL(<FS_PERIODS>).
      <FS_PERIODS>-BEGDA = LS_P0000-BEGDA.
      <FS_PERIODS>-ENDDA = LS_P0000-ENDDA.

      DO.
        ADD 1 TO LV_INDEX.

        IF LV_INDEX > LINES( LT_P0000 ).
          EXIT.
        ENDIF.

        DATA(LS_NEXT) = LT_P0000[ LV_INDEX ].
        IF LV_INDEX EQ LINES( LT_P0000 ) AND LS_NEXT-ENDDA GE PN-ENDDA.
          LS_NEXT-ENDDA = PN-ENDDA.
        ENDIF.

        IF LS_NEXT-STAT2 NE '3'.
          EXIT.
        ELSE.
          <FS_PERIODS>-ENDDA = LS_NEXT-ENDDA.
        ENDIF.

      ENDDO.
    ENDIF.

    ADD 1 TO LV_INDEX.
  ENDWHILE.

  DATA: LT_DATA_LINE LIKE GT_DATA_LINE.
  LOOP AT LT_PERIODS INTO DATA(LS_PERIODS).
    REFRESH LT_DATA_LINE.
    PERFORM FRM_GET_MONTH_IN_TWO_DAYS USING LS_PERIODS-BEGDA LS_PERIODS-ENDDA P_DAY LT_DATA_LINE.

    LOOP AT LT_DATA_LINE ASSIGNING FIELD-SYMBOL(<FS_DATA_LINE>).
      PERFORM FRM_GET_STANDARD_HOURS TABLES GT_T552A USING <FS_DATA_LINE>-BEGDA <FS_DATA_LINE>-ENDDA <FS_DATA_LINE>-HOURS.
    ENDLOOP.

    IF P_COUNT EQ 'X'.
      APPEND INITIAL LINE TO GT_DATA_LINE ASSIGNING <FS_DATA_LINE>.
      <FS_DATA_LINE>-BEGDA = LS_PERIODS-BEGDA.
      <FS_DATA_LINE>-ENDDA = LS_PERIODS-ENDDA.
      LOOP AT LT_DATA_LINE INTO DATA(LS_DATA_LINE).
        ADD LS_DATA_LINE-HOURS TO <FS_DATA_LINE>-HOURS.
      ENDLOOP.
    ELSE.
      APPEND LINES OF LT_DATA_LINE TO GT_DATA_LINE.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM FRM_GET_MONTH_IN_TWO_DAYS USING IV_START_DATE IV_END_DATE IV_DAY ET_MONTH TYPE TT_DATA_LINE.

  DATA: LV_DATE_FIRST    TYPE D,
        LV_DATE_LAST     TYPE D,
        LV_DATE_LAST_TMP TYPE D.

  DATA: LV_START_DATE TYPE D,
        LV_END_DATE   TYPE D.

  DATA: LV_DAY     TYPE NUM,
        LV_DAY_TMP TYPE  NUM.

  CHECK IV_START_DATE LE IV_END_DATE.

  CHECK IV_DAY IS NOT INITIAL AND IV_DAY GE '1' AND IV_DAY LE '31'.

  LV_DAY = IV_DAY.
  LV_DAY_TMP = LV_DAY - 1.
  LV_START_DATE = IV_START_DATE.
  LV_END_DATE = IV_END_DATE.


  WHILE LV_START_DATE LE LV_END_DATE.

    APPEND INITIAL LINE TO ET_MONTH ASSIGNING FIELD-SYMBOL(<FS_MONTH>).
    <FS_MONTH>-BEGDA = LV_START_DATE.

    IF LV_DAY EQ 1.

      <FS_MONTH>-ENDDA = CL_HRPAD_DATE_COMPUTATIONS=>GET_LAST_DAY_IN_MONTH( <FS_MONTH>-BEGDA ).
      LV_START_DATE    = CL_HRPAD_DATE_COMPUTATIONS=>ADD_MONTHS_TO_DATE( START_DATE = LV_START_DATE MONTHS = 1 ).
      LV_START_DATE    = |{ LV_START_DATE+0(6) }01|.

    ELSE.

      LV_DATE_FIRST = |{ LV_START_DATE+0(6) }01|.

      IF LV_START_DATE+6(2) LT LV_DAY.

        LV_DATE_LAST  = CL_HRPAD_DATE_COMPUTATIONS=>GET_LAST_DAY_IN_MONTH( LV_DATE_FIRST ).
        LV_DATE_LAST_TMP = |{ LV_START_DATE+0(6) }{ LV_DAY_TMP ALPHA = IN  }|.

      ELSE.

        LV_DATE_FIRST = CL_HRPAD_DATE_COMPUTATIONS=>ADD_MONTHS_TO_DATE( START_DATE = LV_DATE_FIRST MONTHS = 1 ).
        LV_DATE_LAST  = CL_HRPAD_DATE_COMPUTATIONS=>GET_LAST_DAY_IN_MONTH( LV_DATE_FIRST ).
        LV_DATE_LAST_TMP = |{ LV_DATE_FIRST+0(6) }{ LV_DAY_TMP ALPHA = IN }|.

      ENDIF.

      IF LV_DATE_LAST_TMP GE LV_DATE_LAST.
        <FS_MONTH>-ENDDA = LV_DATE_LAST.
        LV_START_DATE = CL_HRPAD_DATE_COMPUTATIONS=>ADD_MONTHS_TO_DATE( START_DATE = LV_DATE_FIRST MONTHS = 1 ).
      ELSE.
        <FS_MONTH>-ENDDA = LV_DATE_LAST_TMP.
        LV_DATE_LAST_TMP = |{ LV_DATE_FIRST+0(6) }{ LV_DAY ALPHA = IN }|.
        IF LV_DATE_LAST_TMP GT LV_DATE_LAST.
          LV_START_DATE = CL_HRPAD_DATE_COMPUTATIONS=>ADD_MONTHS_TO_DATE( START_DATE = LV_DATE_FIRST MONTHS = 1 ).
        ELSE.
          LV_START_DATE = LV_DATE_LAST_TMP.
        ENDIF.
      ENDIF.

    ENDIF.

    IF <FS_MONTH>-ENDDA GE LV_END_DATE.
      <FS_MONTH>-ENDDA = LV_END_DATE.
    ENDIF.

  ENDWHILE.

ENDFORM.

FORM FRM_GET_STANDARD_HOURS TABLES IT_T552A USING IV_BEGDA IV_ENDDA EV_HOURS .
  DATA: LT_T552A      TYPE TABLE OF T552A,
        LV_FIELD_TPR  TYPE STRING,
        LV_FIELD_FTK  TYPE STRING,
        LV_FIELD_TIME TYPE NUM,
        LV_TIMES      TYPE I.


  CLEAR EV_HOURS.
  MOVE-CORRESPONDING IT_T552A[] TO LT_T552A.

  READ TABLE LT_T552A INTO DATA(LS_T552A_BEG) WITH KEY KJAHR = IV_BEGDA+0(4) MONAT = IV_BEGDA+4(2).
  IF SY-SUBRC EQ 0.
    LV_FIELD_TIME = IV_BEGDA+6(2).
    IF IV_BEGDA+0(6) EQ IV_ENDDA+0(6).
      LV_TIMES = IV_ENDDA+6(2) - LV_FIELD_TIME + 1.
    ELSE.
      LV_TIMES = 31 - LV_FIELD_TIME + 1.
    ENDIF.

    DO LV_TIMES TIMES.
      LV_FIELD_TPR = |TPR{ LV_FIELD_TIME ALPHA = IN }|.
      LV_FIELD_FTK = |FTK{ LV_FIELD_TIME ALPHA = IN }|.
      ASSIGN COMPONENT LV_FIELD_TPR OF STRUCTURE LS_T552A_BEG TO FIELD-SYMBOL(<FS_T552A_BEG_TPR>).
      ASSIGN COMPONENT LV_FIELD_FTK OF STRUCTURE LS_T552A_BEG TO FIELD-SYMBOL(<FS_T552A_BEG_FTK>).
      IF <FS_T552A_BEG_TPR> IS ASSIGNED AND <FS_T552A_BEG_FTK> IS ASSIGNED.
        IF <FS_T552A_BEG_FTK> NE '1' AND <FS_T552A_BEG_TPR> NE 'FREI' AND <FS_T552A_BEG_TPR> IS NOT INITIAL.
          ADD 1 TO EV_HOURS.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
      ADD 1 TO LV_FIELD_TIME.
    ENDDO.
  ENDIF.

  IF IV_BEGDA+0(6) NE IV_ENDDA+0(6).
    READ TABLE LT_T552A INTO DATA(LS_T552A_END) WITH KEY KJAHR = IV_ENDDA+0(4) MONAT = IV_ENDDA+4(2).
    IF SY-SUBRC EQ 0.
      LV_FIELD_TIME = IV_ENDDA+6(2).
      LV_TIMES = LV_FIELD_TIME.
      DO LV_TIMES TIMES.
        LV_FIELD_TPR = |TPR{ LV_FIELD_TIME ALPHA = IN }|.
        LV_FIELD_FTK = |FTK{ LV_FIELD_TIME ALPHA = IN }|.
        ASSIGN COMPONENT LV_FIELD_TPR OF STRUCTURE LS_T552A_END TO FIELD-SYMBOL(<FS_T552A_END_TPR>).
        ASSIGN COMPONENT LV_FIELD_FTK OF STRUCTURE LS_T552A_END TO FIELD-SYMBOL(<FS_T552A_END_FTK>).
        IF <FS_T552A_END_TPR> IS ASSIGNED AND <FS_T552A_END_FTK> IS ASSIGNED.
          IF <FS_T552A_END_FTK> NE '1' AND <FS_T552A_END_TPR> NE 'FREI' AND <FS_T552A_END_TPR> IS NOT INITIAL.
            ADD 1 TO EV_HOURS.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
        SUBTRACT 1 FROM LV_FIELD_TIME.
      ENDDO.
    ENDIF.
  ENDIF.

  EV_HOURS = EV_HOURS * 8.

ENDFORM.

FORM FRM_CREATE_ALV.

  PERFORM FRM_SET_FCAT.
  PERFORM FRM_SET_SORT.
  PERFORM FRM_SET_LAYOUT.

ENDFORM.

FORM FRM_SET_FCAT.

  DATA: LS_FIELDCAT TYPE LVC_S_FCAT,
        LV_POS      TYPE I.

  REFRESH GT_FIELDCAT.

  SET_FCAT: 'X'    'PERNR'    'Employee Number'                             'L'        '10'     ' '  ' '.
  SET_FCAT: 'X'    'ENAME'    'EN Name'                                     'L'        '40'     ' '  ' '.
  SET_FCAT: 'X'    'ALNAM'    'CN Name'                                     'L'        '40'     ' '  ' '.
  SET_FCAT: 'X'    'SCHKZ'    'WS Rule'                                     'L'        '40'     ' '  ' '.
  SET_FCAT: 'X'    'STEXT'    'Department'                                  'L'        '40'     ' '  ' '.
  SET_FCAT: 'X'    'BEGDA'    'Start Time'                                  'L'        '40'     ' '  ' '.
  SET_FCAT: 'X'    'ENDDA'    'End Time'                                    'L'        '40'     ' '  ' '.

  SET_FCAT: ' '    'ANZHL38'  'Standard Calendar Hours'                     'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL37'  'Actual Shift Hours'                          'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL36'  'Working Hours Bank'                          'L'        ' '      ' '  ' '.

  SET_FCAT: ' '    'ANZHL01'  'Plan Working Hours'                          'L'        ' '      ' '  'X'.
  SET_FCAT: ' '    'ANZHL02'  'Actual Working Hours'                        'L'        ' '      ' '  'X'.
  SET_FCAT: ' '    'ANZHL03'  'Late'                                        'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL04'  'Early Leave'                                 'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL05'  'Absenteeism'                                 'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL06'  'Annual Leave'                                'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL07'  'Personal Leave'                              'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL08'  'Sick Leave'                                  'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL09'  'Marriage Leave'                              'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL10'  'Maternity Leave'                             'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL11'  'Prenatal Leave'                              'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL12'  'Nursing Leave'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL13'  'The Nurses Leave'                            'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL14'  'Bereavement Leave'                           'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL15'  'Injury Leave'                                'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL34'  'Childcare Leave'                             'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL35'  'Elderly Care Leave'                          'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL16'  'False Temperature Leave'                     'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL17'  'Supplementary Holidays in Spring Festival'   'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL18'  'Time of in lieu'                             'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL19'  'Overtime 150%'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL20'  'Overtime 200%'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL21'  'Overtime 300%'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL22'  'Overtime Time Off'                           'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL23'  'Overtime Total'                              'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL24'  'Business Trip'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL25'  'Training'                                    'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL26'  'Out of Office'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL27'  'Shuttle Bus Delay'                           'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL28'  'Company Event'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL29'  'Public'                                      'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL30'  'Benefit Leave'                               'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL31'  'Late meal subsidy'                           'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL32'  'Afternoon shift allowance'                   'L'        ' '      ' '  ' '.
  SET_FCAT: ' '    'ANZHL33'  'Night shift allowance'                       'L'        ' '      ' '  ' '.

  DATA: LT_DATA TYPE REF TO DATA,
        LS_DATA TYPE REF TO DATA.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = LT_DATA.

  ASSIGN LT_DATA->* TO <FS_TABLE>.
  CREATE DATA LS_DATA LIKE LINE OF <FS_TABLE>.
  ASSIGN LS_DATA->* TO <FS_STRUC>.

ENDFORM.

FORM FRM_SET_SORT.

  DATA: LS_SORT TYPE LVC_S_SORT,
        LV_POS  TYPE I.

  REFRESH GT_SORT.

  SET_SORT: 'PERNR' 'X' 'X'.
  SET_SORT: 'ENAME' 'X' 'X'.
  SET_SORT: 'ALNAM' 'X' 'X'.
  SET_SORT: 'SCHKZ' 'X' 'X'.
  SET_SORT: 'STEXT' 'X' 'X'.
  SET_SORT: 'BEGDA' 'X' 'X'.
  SET_SORT: 'ENDDA' 'X' 'X'.

ENDFORM.

FORM FRM_SET_LAYOUT.

  CLEAR: GS_LAYOUT.

  GS_LAYOUT-CWIDTH_OPT   = 'X'.   "宽度自动调整
  GS_LAYOUT-SEL_MODE     = 'C'.

ENDFORM.

FORM FRM_APPEND_PERNR.

  DATA: LT_PSP           TYPE TABLE OF PTPSP,
        LV_RDCLUST       TYPE RDCLST VALUE 'X',
        LV_SWITCH_ACTIVE TYPE C VALUE '0'.
  CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
    EXPORTING
      PERNR             = PERNR-PERNR
      BEGDA             = PN-BEGDA
      ENDDA             = PN-ENDDA
      SWITCH_ACTIV      = LV_SWITCH_ACTIVE
      I0001_I0007_ERROR = '0'
      READ_CLUSTER      = LV_RDCLUST
    TABLES
      I0000             = P0000
      I0001             = P0001
      I0002             = P0002
      I0007             = P0007
      I2001             = P2001
      I2002             = P2002
      I2003             = P2003
      PERWS             = LT_PSP
    EXCEPTIONS
      ERROR_OCCURED     = 1
      ABORT_OCCURED     = 2
      OTHERS            = 3.
  DELETE LT_PSP WHERE ACTIV NE 'X'.

  DATA: LS_TIME_B2 TYPE HRF_TIM_B2.
  CALL FUNCTION 'HR_FORMS_TIM_GET_B2_RESULTS'
    EXPORTING
      PERNR  = PERNR-PERNR
      BEGDA  = PN-BEGDA
      ENDDA  = PN-ENDDA
    IMPORTING
      TIM_B2 = LS_TIME_B2.

  DATA: LV_DEL TYPE CHAR1.

  LOOP AT GT_DATA_LINE INTO DATA(LS_DATA_LINE).

    CLEAR <FS_STRUC>.

    RP-PROVIDE-FROM-LAST P0001 SPACE LS_DATA_LINE-BEGDA LS_DATA_LINE-ENDDA.
    RP-PROVIDE-FROM-LAST P0002 SPACE LS_DATA_LINE-BEGDA LS_DATA_LINE-ENDDA.
    RP-PROVIDE-FROM-LAST P0007 SPACE LS_DATA_LINE-BEGDA LS_DATA_LINE-ENDDA.
    RP-PROVIDE-FROM-LAST P0182 3     LS_DATA_LINE-BEGDA LS_DATA_LINE-ENDDA.

    PERFORM FRM_GET_PERNR_INFOR TABLES LT_PSP USING LS_DATA_LINE-BEGDA LS_DATA_LINE-ENDDA LS_TIME_B2 LS_DATA_LINE-HOURS LV_DEL.
    IF LV_DEL IS INITIAL.
      APPEND <FS_STRUC> TO <FS_TABLE>.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM FRM_GET_PERNR_INFOR TABLES IT_PSP USING IV_BEGDA TYPE D IV_ENDDA TYPE D IS_TIME_B2 IV_HOURS EV_DEL.

  DATA: LS_DATA TYPE TS_DATA.

  CLEAR EV_DEL.

**********************************************************************
  IF P0001-BUKRS NE '8300' AND P0001-BUKRS IS NOT INITIAL.
    REJECT.
  ENDIF.

**********************************************************************
  ASSIGN COMPONENT 'PERNR' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_PERNR>).
  <FS_PERNR> = P0001-PERNR.
  IF P0001-PERNR IS INITIAL OR P0001-PERNR EQ '00000000'.
    EV_DEL = 'X'.
    RETURN.
  ENDIF.

  ASSIGN COMPONENT 'ENAME' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_ENAME>).
  <FS_ENAME> = P0001-ENAME.

  ASSIGN COMPONENT 'ALNAM' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_ALNAM>).
  <FS_ALNAM> = P0182-ALNAM.
  IF <FS_ALNAM> IS INITIAL.
    <FS_ALNAM> = <FS_ENAME>.
  ENDIF.

  ASSIGN COMPONENT 'SCHKZ' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_SCHKZ>).
  <FS_SCHKZ> = P0007-SCHKZ.

  ASSIGN COMPONENT 'STEXT' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_STEXT>).
  SELECT SINGLE STEXT INTO <FS_STEXT> FROM HRP1000 WHERE
    PLVAR EQ '99' AND
    OTYPE EQ 'O' AND
    OBJID EQ P0001-ORGEH AND
    ISTAT EQ '1' AND
    BEGDA LE IV_ENDDA AND
    ENDDA GE IV_BEGDA AND
    LANGU EQ SY-LANGU.

  ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_BEGDA>).
  <FS_BEGDA> = IV_BEGDA.

  ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_ENDDA>).
  <FS_ENDDA> = IV_ENDDA.

**********************************************************************


  DATA: LT_PSP  TYPE TABLE OF PTPSP.
  LT_PSP = IT_PSP[].
  DELETE LT_PSP WHERE DATUM LT <FS_BEGDA> OR DATUM GT <FS_ENDDA> OR TPROG EQ 'FREI'.

  SORT LT_PSP BY STDAZ DESCENDING.
  READ TABLE LT_PSP INTO DATA(LS_PSP) INDEX 1.
  IF SY-SUBRC NE 0 OR ( SY-SUBRC EQ 0 AND LS_PSP-STDAZ EQ 0 ).
    EV_DEL = 'X'.
    RETURN.
  ENDIF.


  DATA: LT_P2001 TYPE TABLE OF P2001.
  LT_P2001 = P2001[].
  DELETE LT_P2001 WHERE BEGDA GT <FS_ENDDA> OR ENDDA LT <FS_BEGDA>.


  DATA: LT_P2002 TYPE TABLE OF P2002.
  LT_P2002 = P2002[].
  DELETE LT_P2002 WHERE BEGDA GT <FS_ENDDA> OR ENDDA LT <FS_BEGDA>.

  DATA: LS_HRF_TIM_B2 TYPE HRF_TIM_B2.
  LS_HRF_TIM_B2 = IS_TIME_B2.
  DELETE LS_HRF_TIM_B2-FT_ZES WHERE DATUM LT <FS_BEGDA> OR DATUM GT <FS_ENDDA>.
  DELETE LS_HRF_TIM_B2-FT_ZL  WHERE DATUM LT <FS_BEGDA> OR DATUM GT <FS_ENDDA>.

  DATA: LV_FIELD_NAME TYPE STRING.
  LOOP AT LT_PSP INTO LS_PSP.
    ADD LS_PSP-STDAZ TO LS_DATA-ANZHL01.

    IF LS_PSP-TPROG NE 'COFF' AND LS_PSP-FTKLA NE '1'.

      LOOP AT LT_P2001 INTO DATA(LS_P2001) WHERE BEGDA LE LS_PSP-DATUM AND ENDDA GE LS_PSP-DATUM.
        CLEAR LV_FIELD_NAME.

        CASE LS_P2001-SUBTY.
          WHEN 'C100'.
            LV_FIELD_NAME = 'ANZHL06'. "Annual
          WHEN 'C200'.
            LV_FIELD_NAME = 'ANZHL07'. "Personal
          WHEN 'C300' OR 'C310' OR 'C320' OR 'C330'.
            LV_FIELD_NAME = 'ANZHL08'. "Sick
          WHEN 'C400'.
            LV_FIELD_NAME = 'ANZHL09'. "Marriage
          WHEN 'C410'.
            LV_FIELD_NAME = 'ANZHL10'. "Maternity
          WHEN 'C420'.
            LV_FIELD_NAME = 'ANZHL11'. "Prenatal
          WHEN 'C430'.
            LV_FIELD_NAME = 'ANZHL12'. "Nursing
          WHEN 'C440'.
            LV_FIELD_NAME = 'ANZHL13'. "The Nurses
          WHEN 'C450'.
            LV_FIELD_NAME = 'ANZHL14'. "Bereavement
          WHEN 'C460'.
            LV_FIELD_NAME = 'ANZHL15'. "Injury
          WHEN 'C470'.
            LV_FIELD_NAME = 'ANZHL16'. "False Temperature
          WHEN 'C500'.
            LV_FIELD_NAME = 'ANZHL05'. "Absenteeism
          WHEN 'C600'.
            LV_FIELD_NAME = 'ANZHL18'. "Time of in lieu
          WHEN 'C480'.
            LV_FIELD_NAME = 'ANZHL34'. "Childcare
          WHEN 'C490'.
            LV_FIELD_NAME = 'ANZHL35'. "Elderly Care
        ENDCASE.

        IF LV_FIELD_NAME IS NOT INITIAL.
          ASSIGN COMPONENT LV_FIELD_NAME OF STRUCTURE LS_DATA TO FIELD-SYMBOL(<FS_ANZHL_2001>).
          IF <FS_ANZHL_2001> IS ASSIGNED.
            IF LS_P2001-BEGUZ NE '' AND LS_P2001-ENDUZ NE ''.
              ADD LS_P2001-ABRST TO <FS_ANZHL_2001>.
            ELSE.
              ADD 8 TO <FS_ANZHL_2001>.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.



      LOOP AT LT_P2002 INTO DATA(LS_P2002) WHERE BEGDA LE LS_PSP-DATUM AND ENDDA GE LS_PSP-DATUM.
        CLEAR LV_FIELD_NAME.

        CASE LS_P2002-SUBTY.
          WHEN 'C800'.
            LV_FIELD_NAME = 'ANZHL24'. "Trip
          WHEN 'C810'.
            LV_FIELD_NAME = 'ANZHL25'. "Tranining
          WHEN 'C820'.
            LV_FIELD_NAME = 'ANZHL26'. "Out
          WHEN 'C830'.
            LV_FIELD_NAME = 'ANZHL27'. "Bus delay
          WHEN 'C840'.
            LV_FIELD_NAME = 'ANZHL28'. "Event
          WHEN 'C850'.
            LV_FIELD_NAME = 'ANZHL29'. "Public
          WHEN 'C860'.
            LV_FIELD_NAME = 'ANZHL30'. "Benefit
        ENDCASE.

        IF LV_FIELD_NAME IS NOT INITIAL.
          ASSIGN COMPONENT LV_FIELD_NAME OF STRUCTURE LS_DATA TO FIELD-SYMBOL(<FS_ANZHL_2002>).
          IF <FS_ANZHL_2002> IS ASSIGNED.
            IF LS_P2002-BEGUZ NE '' AND LS_P2002-ENDUZ NE ''.
              ADD LS_P2002-STDAZ TO <FS_ANZHL_2002>.
            ELSE.
              ADD 8 TO <FS_ANZHL_2002>.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

  LOOP AT LS_HRF_TIM_B2-FT_ZL INTO DATA(LS_ZL).
    CLEAR LV_FIELD_NAME.

    CASE LS_ZL-LGART.
      WHEN '6010'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL19. "OT-150
      WHEN '6020'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL20. "OT-200
      WHEN '6030'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL21. "OT-300
      WHEN '3065'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL31. "Late meal subsidy
      WHEN '3110'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL32. "Afternoon shift allowance
      WHEN '3115'.
        ADD LS_ZL-ANZHL TO LS_DATA-ANZHL33. "Night shift allowance
    ENDCASE.

  ENDLOOP.

  LOOP AT LS_HRF_TIM_B2-FT_ZES INTO DATA(LS_ZES).

    CASE LS_ZES-ZTART.
      WHEN '2802'.
        ADD LS_ZES-ANZHL TO LS_DATA-ANZHL03. "Late
      WHEN '2804'.
        ADD LS_ZES-ANZHL TO LS_DATA-ANZHL04. "Early
      WHEN '2806'.
        ADD LS_ZES-ANZHL TO LS_DATA-ANZHL05. "Absenteeism
      WHEN '0410'.
        ADD LS_ZES-ANZHL TO LS_DATA-ANZHL22. "OT-Time off
    ENDCASE.

  ENDLOOP.

**********************************************************************
  DATA: LT_P2010 TYPE TABLE OF P2010.
  LT_P2010 = P2010[].
  DELETE LT_P2010 WHERE BEGDA GT <FS_ENDDA> OR ENDDA LT <FS_BEGDA> OR ( SUBTY NE '6010' AND SUBTY NE '6020' AND SUBTY NE '6030' ).

  DATA: LV_ANZHL_SUM TYPE P2010-ANZHL.
  LOOP AT LT_P2010 INTO DATA(LS_P2010).
    ADD LS_P2010-ANZHL TO LV_ANZHL_SUM.
  ENDLOOP.

  LS_DATA-ANZHL23 = LS_DATA-ANZHL19 + LS_DATA-ANZHL20 + LS_DATA-ANZHL21 + LS_DATA-ANZHL22 + LV_ANZHL_SUM.   "Overtime Total

  LS_DATA-ANZHL02 =  LS_DATA-ANZHL01 + LS_DATA-ANZHL23 + LV_ANZHL_SUM - (
                     LS_DATA-ANZHL03 + LS_DATA-ANZHL04 + LS_DATA-ANZHL05 + LS_DATA-ANZHL06 + LS_DATA-ANZHL07 +
                     LS_DATA-ANZHL08 + LS_DATA-ANZHL09 + LS_DATA-ANZHL10 + LS_DATA-ANZHL11 + LS_DATA-ANZHL12 +
                     LS_DATA-ANZHL13 + LS_DATA-ANZHL14 + LS_DATA-ANZHL15 + LS_DATA-ANZHL16 + LS_DATA-ANZHL17 +
                     LS_DATA-ANZHL18 + LS_DATA-ANZHL34 + LS_DATA-ANZHL35 ).

  LS_DATA-ANZHL38 = IV_HOURS.

  LS_DATA-ANZHL37 = LS_DATA-ANZHL01.

  LS_DATA-ANZHL36 = LS_DATA-ANZHL37 - LS_DATA-ANZHL38.


  DATA: LV_NUM TYPE NUM.
  DO 38 TIMES.
    LV_NUM = SY-INDEX.
    CLEAR LV_FIELD_NAME.
    CONCATENATE 'ANZHL' LV_NUM INTO LV_FIELD_NAME.
    ASSIGN COMPONENT LV_FIELD_NAME OF STRUCTURE LS_DATA TO FIELD-SYMBOL(<FS_DATA_ANZHL>).
    ASSIGN COMPONENT LV_FIELD_NAME OF STRUCTURE <FS_STRUC> TO FIELD-SYMBOL(<FS_STRUC_ANZHL>).
    IF <FS_DATA_ANZHL> IS ASSIGNED AND <FS_STRUC_ANZHL> IS ASSIGNED.
      <FS_STRUC_ANZHL> = <FS_DATA_ANZHL>.
      CONDENSE <FS_STRUC_ANZHL> NO-GAPS.
    ENDIF.
    UNASSIGN: <FS_DATA_ANZHL>, <FS_STRUC_ANZHL>.
  ENDDO.


ENDFORM.

FORM FRM_ALV_DISPLAY.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Ready for output...'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'FRM_CB_PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'FRM_CB_USER_COMMAND'
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT
      IT_SORT_LVC              = GT_SORT
      I_SAVE                   = 'U'
    TABLES
      T_OUTTAB                 = <FS_TABLE>
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.

FORM FRM_CB_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS  'STATUS' EXCLUDING RT_EXTAB.

ENDFORM.

FORM FRM_CB_USER_COMMAND USING UCOMM SELLINE TYPE SLIS_SELFIELD.

ENDFORM.
```

## 薪酬常用函数 ##

### 基本介绍 ###

工资模拟计算和工资发放.在实际项目中,PY模块很少有修改标准的程序,一般情况下,开发主要是涉及到薪酬报表或者是开发自定义的薪资过账.至于薪资核算过程中的计算规则等到,均可以通过`schema`来实现

### 名词解释 ###

| 名词     | 描述                                                         |
| -------- | ------------------------------------------------------------ |
| 工资项   | 工资类型,例如:基本工资(1000)/工龄补贴(1010)                  |
| 工资范围 | 具备相同工资发放制度的人的集合,是工资核算的最小单元          |
| schema   | 薪酬函数,可通过==PE01/PE02/PE03==来维护.薪资发放过程中,一般会要求以某种schema模拟运行 |

### 薪资核算状态 ###

工资范围的工资核算状态和期间对应两张表

`T569U`:   工资范围当前状态和核算期间

`T569V`:    工资范围工资核算日志

```abap
"查询当前工资范围的核算状态
SELECT STATE INTO @DATA(LV_STATE) FROM T569V WHERE ABKRS = ABKRS AND PABRJ = '年' AND PABRP = '月'. 
```



| 状态 | 描述                                          |
| ---- | --------------------------------------------- |
| 0    | 未创建                                        |
| 1    | 为薪资发放而释放,会锁定相关人员的薪资相关信息 |
| 2    | 工资发放改正                                  |
| 3    | 退出工资发放                                  |
| 4    | 检查发放结果,会锁定相关人员的薪资相关信息     |

### 薪资结果数据结构 ###

薪资结果的数据类型为`PAY99_RESULT`(国际通用) `PAYCN_RESULT`(中国),该类型为一个多层次嵌套类型:

- `PAY99_RESULT-INTER-RT`: 工资核算结果明细表,存储了员工的所有应发,实发,税额等等明细.一般薪酬报表开发中,都从该字表中读取对应的工资明细信息.
- `PAY99_RESULT-INTER-BT`:  实际支付金额,银行基本信息
- `PAYCN_RESULT-NAT-TCRT`:  税收累计（ 累计类型：CUMTY，( Y 为按年累计 ) ）

### 常用函数 ###

| 函数                            | 描述                                                         |
| ------------------------------- | ------------------------------------------------------------ |
| **`CU_READ_RGDIR`**             | 读取员工所有的薪资发放结果 PC261 <br />`BONDT`:  非周期性发放日期<br />`PAYTY`:  支付类型:  A奖金<br />`FPBEG`:  工资发放期间的开始(历经期)<br />`FPEND`:  工资发放期间的结束 (历经期间) |
| **`PYXX_GET_RELID_FROM_PERNR`** | 读取员工区域标示和国家分组                                   |
| **`PYXX_READ_PAYROLL_RESULT`**  | 读取员工某个期间的工资发放明细,类型  PAY99_RESULT / PAYCN_RESULT.<br />注意一定要设置参数 `READ_ONLY_INTERNATIONAL`,才能使用 PAY99_RESULT. |
|                                 |                                                              |
|                                 |                                                              |

```ABAP
*&---------------------------------------------------------------------*
*& Report ZTEST_0002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_0002.

DATA: ld_molga              TYPE t500l-molga,
      ld_persnr	            TYPE p0000-pernr,
      it_in_rgdir	          TYPE STANDARD TABLE OF pc261,
      wa_in_rgdir	          LIKE LINE OF it_in_rgdir,
      ld_buffer	            TYPE hrpay_buffer,
      ld_no_authority_check	TYPE xfeld.

CALL FUNCTION 'CU_READ_RGDIR'
  EXPORTING
    persnr             = ld_persnr
    buffer             = ld_buffer
    no_authority_check = ld_no_authority_check
  IMPORTING
    molga              = ld_molga
  TABLES
    in_rgdir           = it_in_rgdir
  EXCEPTIONS
    no_record_found    = 1.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ENDIF.

DATA: ld_relid    TYPE relid_pcl,
      ld_employee	TYPE persno.


CALL FUNCTION 'PYXX_GET_RELID_FROM_PERNR'
  EXPORTING
    employee                    = ld_employee
  IMPORTING
    relid                       = ld_relid
    molga                       = ld_molga
  EXCEPTIONS
    error_reading_infotype_0001 = 1
    error_reading_molga         = 2
    error_reading_relid         = 3.

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 2. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 3. "Exception
  "Add code for exception here
ENDIF.


DATA: ld_payroll_result	         TYPE string,
      ld_version_number_payvn	   TYPE string,
      ld_clusterid               TYPE pcl2-relid,
      ld_version_number_pcl2     TYPE string,
      ld_employeenumber	         TYPE pc200-pernr,
      ld_sequencenumber	         TYPE pc261-seqnr,
      ld_read_only_buffer	       TYPE string,
      ld_read_only_international TYPE string,
      ld_arc_group               TYPE arc_group,
      ld_check_read_authority	   TYPE string,
      ld_filter_cumulations	     TYPE string,
      ld_client	                 TYPE pcl2-client.


CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
  EXPORTING
    clusterid                    = ld_clusterid
    employeenumber               = ld_employeenumber
    sequencenumber               = ld_sequencenumber
    read_only_buffer             = ld_read_only_buffer
    read_only_international      = ld_read_only_international
    arc_group                    = ld_arc_group
    check_read_authority         = ld_check_read_authority
    filter_cumulations           = ld_filter_cumulations
    client                       = ld_client
  IMPORTING
    version_number_payvn         = ld_version_number_payvn
    version_number_pcl2          = ld_version_number_pcl2
  CHANGING
    payroll_result               = ld_payroll_result
  EXCEPTIONS
    illegal_isocode_or_clusterid = 1
    error_generating_import      = 2
    import_mismatch_error        = 3
    subpool_dir_full             = 4
    no_read_authority            = 5
    no_record_found              = 6
    versions_do_not_match        = 7
    error_reading_archive        = 8
    error_reading_relid          = 9
  .  "  PYXX_READ_PAYROLL_RESULT

IF sy-subrc EQ 0.
  "All OK
ELSEIF sy-subrc EQ 1. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 2. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 3. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 4. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 5. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 6. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 7. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 8. "Exception
  "Add code for exception here
ELSEIF sy-subrc EQ 9. "Exception
  "Add code for exception here
ENDIF.
```

### 两张簇表 ###

`PCL1`  主要存储一些信息类型的文本信息

`PCL2`  主要存储员工工资核算结果,时间评估数据

```abap
DATA:lv_key   TYPE pcl2-srtfd,
     lt_rgdir TYPE STANDARD TABLE OF pc261,
     lt_rt    TYPE STANDARD TABLE OF pc207, "工资结果表
     lt_tcrt  TYPE STANDARD TABLE OF pc2g5, "工资结果合计
     lt_wpbp  type standard table of pc205. "成本中心

lv_key = '70010065'."员工编号(需补零),

"可以从簇表 PCL2 中 import,id值就是员工号,但会抽出员工的所有工资记录
IMPORT rgdir = lt_rgdir FROM DATABASE pcl2(cu) ID lv_key.

"读取工资结果中复合条件的数据.
READ TABLE lt_rgdir INTO DATA(ls_rgdir) INDEX 1.
IF sy-subrc = 0.
  lv_key = '70010065' && ls_rgdir-seqnr.
ENDIF.

IMPORT rt = lt_rt FROM DATABASE pcl2(cn) ID lv_key.
IMPORT tcrt = lt_tcrt FROM DATABASE pc12(cn) ID lv_key.
import wpbp = lt_wpbp form database pcl2(cn) id lv_key.

BREAK-POINT.
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

## 信息类型中长文本操作

- **读取长文本:** 还可以使用函数 `HR_READ_INFTY_NOTE` 

  ```abap
    DATA: lv_tclas    TYPE tclas,
          ls_pskey    TYPE pskey,
          lt_text_tab TYPE hrpad_text_tab.
    
    MOVE-CORRESPONDING p0552 TO ls_pskey.
    ls_pskey-infty = '0552'.
    lv_tclas = 'A'.
  
    CALL METHOD cl_hrpa_text_cluster=>read( 
    	EXPORTING 
    		tclas         = lv_tclas 
    		pskey         = ls_pskey 
    		no_auth_check = 'X' 
    	IMPORTING 
    		text_tab      = lt_text_tab ).
    LOOP AT lt_text_tab INTO DATA(l_text_tab).
      gs_data-a0006 = l_text_tab .
      IF l_text_tab IS NOT INITIAL .
        EXIT.
      ENDIF.
    ENDLOOP.
  ```

- **写入长文本：**

  关于写入，暂时不知道， 可以参考

  1.  [Update long text in infotypes | SAP Blogs](https://blogs.sap.com/2013/04/30/update-long-text-in-infotypes/)
2. [长文本值未显示在 pa30 屏幕中 |SAP 社区](https://answers.sap.com/questions/7159166/long-text-value-not-displaying-in-pa30-screen.html)

## 读取 0041 时间数据

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





