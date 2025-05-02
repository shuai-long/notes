# FIELDCAT 设置 #

## 保留六位小数 ##

se37 创建 `CONVERSION_EXIT_ZLWXS_INPUT` 和 `CONVERSION_EXIT_ZLWXS_OUTPUT` 函数, 函数名更改 EXIT 和 OUTPUT 中间部分即可

```abap
DATA: LS_FCAT TYPE LVC_S_FCAT.
" SE37 创建 CONVERSION_EXIT_ZLWXS_INPUT
FUNCTION conversion_exit_zlwxs_input.
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
  DATA: ls_convert      TYPE rsconvlite,
        odata_flag      TYPE abap_bool VALUE abap_false,
        lv_input_string TYPE string,
        lr_input        TYPE REF TO data.
  FIELD-SYMBOLS <lv_input> TYPE any.

* indicate that CONVERT contains useful settings
  ls_convert-active   = 'X'.

* indicate that a sign is allowed (if necessary!)
  ls_convert-sign = abap_true.

  IF currency IS SUPPLIED.
    lv_input_string = input.
    DATA(lv_len) = strlen( lv_input_string ).
    IF lv_len = 0.
      lv_len = 1.
    ENDIF.
    CREATE DATA lr_input TYPE c LENGTH lv_len.
    ASSIGN lr_input->* TO <lv_input>.
    <lv_input> = input.
    refval = currency.
    odata_flag = abap_true.
  ELSE.
    ASSIGN input TO <lv_input>.
  ENDIF.

* set the allowed output length
* this should match the lentgh of the input field
  DESCRIBE FIELD <lv_input> TYPE DATA(lv_source_type).
  IF lv_source_type = cl_abap_typedescr=>typekind_string.
    DESCRIBE FIELD output OUTPUT-LENGTH ls_convert-olength.
    odata_flag = abap_true.
  ELSEIF lv_source_type = cl_abap_typedescr=>typekind_packed.
    DESCRIBE FIELD <lv_input> LENGTH DATA(lv_bytelength) IN BYTE MODE.
    ls_convert-olength  = lv_bytelength * 2.
    odata_flag = abap_true.
  ELSE.
    DESCRIBE FIELD <lv_input> LENGTH ls_convert-olength IN CHARACTER MODE.
  ENDIF.

* set decimals to the default for currency fields
  ls_convert-decimals = 6.

* no further settings to be done here

  CALL FUNCTION 'AFLE_CONVERSION_COMMON_CUR_IN'
    EXPORTING
      input      = <lv_input>
      refval     = refval
      convert    = ls_convert
      length     = 18
      odata_flag = odata_flag
      decimals   = 6
    IMPORTING
      output     = output.
ENDFUNCTION.

" SE37 创建 CONVERSION_EXIT_ZLWXS_OUTPUT 函数
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

# Layout 设置

禁止删除和新增行：

```ABAP
 LS_LAYOUT-NO_ROWINS = 'X'.
```

