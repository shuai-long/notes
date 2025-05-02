[TOC]

# EDIT_MASK #

## 保留小数位数 ##

```abap
FUNCTION CONVERSION_EXIT_ZLWXS_INPUT.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"     VALUE(REFVAL) OPTIONAL
*"     VALUE(CURRENCY) OPTIONAL
*"  EXPORTING
*"     VALUE(OUTPUT)
*"--------------------------------------------------------------------

* When using RS_CONV_EX_2_IN_NO_DD we have to care for passing the
* the required type info to this FM.
* This is acchieved by doing useful settings into the FM's input parameter CONVERT.
  DATA: LS_CONVERT      TYPE RSCONVLITE,
        ODATA_FLAG      TYPE ABAP_BOOL VALUE ABAP_FALSE,
        LV_INPUT_STRING TYPE STRING,
        LR_INPUT        TYPE REF TO DATA.
  FIELD-SYMBOLS <LV_INPUT> TYPE ANY.

* indicate that CONVERT contains useful settings
  LS_CONVERT-ACTIVE   = 'X'.

* indicate that a sign is allowed (if necessary!)
  LS_CONVERT-SIGN = ABAP_TRUE.

  IF CURRENCY IS SUPPLIED.
    LV_INPUT_STRING = INPUT.
    DATA(LV_LEN) = STRLEN( LV_INPUT_STRING ).
    IF LV_LEN = 0.
      LV_LEN = 1.
    ENDIF.
    CREATE DATA LR_INPUT TYPE C LENGTH LV_LEN.
    ASSIGN LR_INPUT->* TO <LV_INPUT>.
    <LV_INPUT> = INPUT.
    REFVAL = CURRENCY.
    ODATA_FLAG = ABAP_TRUE.
  ELSE.
    ASSIGN INPUT TO <LV_INPUT>.
  ENDIF.

* set the allowed output length
* this should match the lentgh of the input field
  DESCRIBE FIELD <LV_INPUT> TYPE DATA(LV_SOURCE_TYPE).
  IF LV_SOURCE_TYPE = CL_ABAP_TYPEDESCR=>TYPEKIND_STRING.
    DESCRIBE FIELD OUTPUT OUTPUT-LENGTH LS_CONVERT-OLENGTH.
    ODATA_FLAG = ABAP_TRUE.
  ELSEIF LV_SOURCE_TYPE = CL_ABAP_TYPEDESCR=>TYPEKIND_PACKED.
    DESCRIBE FIELD <LV_INPUT> LENGTH DATA(LV_BYTELENGTH) IN BYTE MODE.
    LS_CONVERT-OLENGTH  = LV_BYTELENGTH * 2.
    ODATA_FLAG = ABAP_TRUE.
  ELSE.
    DESCRIBE FIELD <LV_INPUT> LENGTH LS_CONVERT-OLENGTH IN CHARACTER MODE.
  ENDIF.

* set decimals to the default for currency fields
  LS_CONVERT-DECIMALS = 6.

* no further settings to be done here

  CALL FUNCTION 'AFLE_CONVERSION_COMMON_CUR_IN'
    EXPORTING
      INPUT      = <LV_INPUT>
      REFVAL     = REFVAL
      CONVERT    = LS_CONVERT
      LENGTH     = 18
      ODATA_FLAG = ODATA_FLAG
      DECIMALS   = 6
    IMPORTING
      OUTPUT     = OUTPUT.


ENDFUNCTION.
```

## 符号前置 ##

```abap
FUNCTION CONVERSION_EXIT_ZLWXS_OUTPUT.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"     VALUE(REFVAL) OPTIONAL
*"     VALUE(CURRENCY) OPTIONAL
*"  EXPORTING
*"     VALUE(OUTPUT)
*"--------------------------------------------------------------------

IF currency IS NOT SUPPLIED.

    CALL FUNCTION 'AFLE_CONVERSION_COMMON_CUR_OUT'
      EXPORTING
        input  = input
        refval = refval
      IMPORTING
        output = output.

  ELSE.
    DATA lv_output_string TYPE string.
    "Gateway specific logic
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
      EXPORTING
        currency    = currency
        sap_amount  = input
      IMPORTING
        idoc_amount = lv_output_string.

    CONDENSE lv_output_string NO-GAPS.

    " Negative Currency Amount: '2.00-' => '-2.00'
    IF lv_output_string IS NOT INITIAL.
      SHIFT lv_output_string RIGHT CIRCULAR.
      IF lv_output_string(1) <> '-'.
        SHIFT lv_output_string LEFT CIRCULAR.
      ENDIF.
    ENDIF.
    output = lv_output_string.
  ENDIF.

ENDFUNCTION.
```

