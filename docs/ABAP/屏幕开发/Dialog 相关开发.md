[TOC]

# ALV 常用事件 #

## **强制触发 PBO**

```ABAP
CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
   EXPORTING
     NEW_CODE = 'PAI'.
```

## **搜索帮助后 强制触发 PBO**

```ABAP
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE                 = 'POV' "随便给值即可
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED       = 1
      OTHERS                       = 2.
```

## **获取屏幕值**

```abap
FORM FRM_GET_VALUE_FROM_SCREEN USING KEY VALUE.
  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  LT_DYNPFIELDS = VALUE #( ( FIELDNAME = KEY ) ).

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
      TRANSLATE_TO_UPPER   = 'X'
    TABLES
      DYNPFIELDS           = LT_DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.

  IF SY-SUBRC = 0.
    READ TABLE LT_DYNPFIELDS INTO DATA(LS_DYNPFIELDS) INDEX 1.
    VALUE = LS_DYNPFIELDS-FIELDVALUE. " 备注
  ENDIF.

ENDFORM.
```

## **将值更新到屏幕**

```abap
FORM FRM_SET_VALUE_TO_SCREEN USING KEY VALUE.
  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  LT_DYNPFIELDS = VALUE #( ( FIELDNAME = KEY FIELDVALUE = VALUE ) ).

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = LT_DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      UNDEFIND_ERROR       = 7
      OTHERS               = 8.

ENDFORM.
```

## **下拉框**

- 为屏幕字段设置下拉框

  ```abap
  FORM FRM_SET_VALUE_LIST_FOR_FIELD TABLES IT_ITAB USING IV_ID.
  	CALL FUNCTION 'VRM_SET_VALUES'
  		EXPORTING
  			ID     = IV_ID
  			VALUES = IT_ITAB[].
  ENDFORM.
  ```

- 获取下拉框值的描述

  ```abap
  FORM FRM_GET_VALUE_LIST_FOR_FIELD USING IV_ID CHANGING EV_TEXT.
    DATA: LV_ID     TYPE VRM_ID,
          LV_VALUE  TYPE CHAR40,
          LT_VALUES TYPE VRM_VALUES.
  
    CLEAR EV_TEXT.
    LV_ID = IV_ID.
  
    ASSIGN (IV_ID) TO FIELD-SYMBOL(<FS_VALUE>).
    CHECK <FS_VALUE> IS ASSIGNED.
    LV_VALUE = <FS_VALUE>.
  
    CALL FUNCTION 'VRM_GET_VALUES'
      EXPORTING
        ID           = LV_ID
      IMPORTING
        VALUES       = LT_VALUES
      EXCEPTIONS
        ID_NOT_FOUND = 1
        OTHERS       = 2.
  
    READ TABLE LT_VALUES INTO DATA(LS_VALUES) WITH KEY KEY = LV_VALUE.
    IF SY-SUBRC EQ 0.
      EV_TEXT = LS_VALUES-TEXT.
    ENDIF.
  
  ENDFORM.
  ```


## TC 更新

```abap
if tc_recipient-current_line <= lines( gt_email ).
  modify gt_email from gs_email index tc_recipient-current_line.
else.
  append gs_email to gt_email .
  clear gs_email.
endif.
```

