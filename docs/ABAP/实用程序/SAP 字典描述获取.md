[TOC]

# SAP 字典描述获取 #

## 类创建 ##

```abap
CLASS ZCL_DOMAIN_GET_TEXT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      DOM_TAB TYPE TABLE OF DOMNAME .

    METHODS CONSTRUCTOR
      IMPORTING
        !DOM_TAB    LIKE DOM_TAB
        !DDLANGUAGE TYPE DDLANGUAGE .
    METHODS GET_DOMNAME_STEXT
      IMPORTING
        VALUE(DOMNAME)    TYPE DOMNAME
        VALUE(DOMVALUE_P) TYPE ANY
        VALUE(DOMVALUE_S) TYPE ANY OPTIONAL
      EXPORTING
        VALUE(DDTEXT)     TYPE ANY .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: BEGIN OF GS_DD07V,
            DOMNAME    TYPE DOMNAME,
            DOMVALUE_L TYPE DOMVALUE_L,
            DDTEXT     TYPE VAL_TEXT,
          END OF GS_DD07V.

    DATA: GT_DD07V LIKE SORTED TABLE OF GS_DD07V WITH UNIQUE KEY DOMNAME DOMVALUE_L,
          GT_T529T TYPE SORTED TABLE OF T529T    WITH UNIQUE KEY MASSN,
          GT_T530T TYPE SORTED TABLE OF T530T    WITH UNIQUE KEY MASSN MASSG,
          GT_T005T TYPE SORTED TABLE OF T005T    WITH UNIQUE KEY LAND1.

ENDCLASS.



CLASS ZCL_DOMAIN_GET_TEXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DOMAIN_GET_TEXT->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOM_TAB                        LIKE        DOM_TAB
* | [--->] DDLANGUAGE                     TYPE        DDLANGUAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.
    DATA: LR_DOMNA TYPE RANGE OF DOMNAME.

    LOOP AT DOM_TAB INTO DATA(LS_DOM_TAB).

      CASE LS_DOM_TAB.
        WHEN 'MASSN'. "操作类型
          IF GT_T529T IS INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T529T FROM T529T WHERE SPRSL EQ DDLANGUAGE.
          ENDIF.
          CONTINUE.
        WHEN 'MASSG'.  "操作原因
          IF GT_T530T IS INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T530T FROM T530T WHERE SPRSL EQ DDLANGUAGE.
          ENDIF.
          CONTINUE.
        WHEN 'NATIO' OR 'LAND1'.  "国家国籍
          IF GT_T5005T IS INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T005T FROM T005T WHERE SPRAS EQ DDLANGUAGE.
          ENDIF.
          CONTINUE.
      ENDCASE.


      IF LS_DOM_TAB+0(1) EQ 'Z'.
        LR_DOMNA[] = VALUE #( BASE LR_DOMNA SIGN = 'I' OPTION = 'EQ' ( LOW = LS_DOM_TAB ) ).
      ENDIF.

    ENDLOOP.

    CHECK LR_DOMNA IS NOT INITIAL.
    SELECT DOMNAME DOMVALUE_L DDTEXT INTO CORRESPONDING FIELDS OF TABLE GT_DD07V FROM DD07V WHERE DDLANGUAGE EQ DDLANGUAGE AND DOMNAME IN LR_DOMNA.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DOMAIN_GET_TEXT->GET_DOMNAME_STEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOMNAME                        TYPE        DOMNAME
* | [--->] DOMVALUE_P                     TYPE        ANY
* | [--->] DOMVALUE_S                     TYPE        ANY(optional)
* | [<---] DDTEXT                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_DOMNAME_STEXT.
    CASE DOMNAME.
      WHEN 'MASSN'.  "操作类型
        READ TABLE GT_T529T INTO DATA(LS_T529T) WITH KEY MASSN = DOMVALUE_P BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DDTEXT = LS_T529T-MNTXT.
        ENDIF.
        RETURN.
      WHEN 'MASSG'. "操作原因
        READ TABLE GT_T530T INTO DATA(LS_T530T) WITH KEY MASSN = DOMVALUE_S MASSG = DOMVALUE_P BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DDTEXT = LS_T530T-MGTXT.
        ENDIF.
        RETURN.
      WHEN 'NATIO'.  "国籍
        READ TABLE GT_T005T INTO DATA(LS_T005T) WITH KEY LAND1 = DOMVALUE_P BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DDTEXT = LS_T005T-NATIO.
        ENDIF.
        RETURN.
      WHEN 'LAND1'.  "国家
        READ TABLE GT_T005T INTO LS_T005T WITH KEY LAND1 = DOMVALUE_P BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DDTEXT = LS_T005T-LAND1.
        ENDIF.
        RETURN.

      WHEN OTHERS.
        READ TABLE GT_DD07V INTO DATA(LS_DD07V) WITH KEY DOMNAME = DOMNAME DOMVALUE_L = DOMVALUE_P BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DDTEXT = LS_DD07V-DDTEXT.
        ENDIF.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

## 方法调用示例 ##

```abap
DATA: G_ZCL_DOMAIN_GET_TEXT      TYPE REF TO ZCL_DOMAIN_GET_TEXT,
	  LT_DOM_TAB TYPE TABLE OF DOMNAME.
	  
GT_DOM_TAB = VALUE #( ( 'NATIO' ) ).
CREATE OBJECT G_ZCL_DOMAIN_GET_TEXT
    EXPORTING
      DOM_TAB    = GT_DOM_TAB
      DDLANGUAGE = '1'.
      
CALL METHOD G_ZCL_DOMAIN_GET_TEXT->GET_DOMNAME_STEXT
    EXPORTING
      DOMNAME    = 'NATIO'
      DOMVALUE_P = P0002-NATIO
    IMPORTING
      DDTEXT     = GS_ALV-NATIO.      

```

## 已实现字典字段 ##

| 字段描述     | 字段    | 字段描述     | 字段    |
| ------------ | ------- | ------------ | ------- |
| **操作类型** | `MASSN` | **操作原因** | `MASSG` |
| **国籍**     | `NATIO` | **国家**     | `LAND1` |
|              |         |              |         |
|              |         |              |         |
|              |         |              |         |
|              |         |              |         |
|              |         |              |         |
|              |         |              |         |
|              |         |              |         |

