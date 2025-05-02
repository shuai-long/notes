## 1. 获取 SMARTFORMS 对应函数名称

```abap
    data: lv_fmname type rs38l_fnam.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMMF009'
      importing
        fm_name            = lv_fmname
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
```

## 2. 调用 SMARTFORM

```abap
    data: ls_control_parameters type ssfctrlop,
          ls_output_options     type ssfcompop,
          ls_job_output_info    type ssfcrescl.

    ls_output_options-tddest = 'LP01'.
    ls_output_options-tdimmed  = 'X'.
    ls_output_options-tddelete = 'X'.
    ls_output_options-tdnoprev = 'X'.
    ls_control_parameters-no_dialog = 'X'.
    ls_control_parameters-getotf    = 'X'.
    
    call function lv_fmname
      exporting
        control_parameters = ls_control_parameters
        output_options     = ls_output_options
        user_settings      = 'X'
      importing
        job_output_info    = ls_job_output_info
      exceptions
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        others             = 5.
```

## 3. 转换 PDF

```abap
    data: lt_otf      type standard table of itcoo,
          lt_lines    type standard table of tline,
          lv_file     type xstring,
          lv_filesize type i.

    lt_otf[] = ls_job_output_info-otfdata[].
    call function 'CONVERT_OTF'
      exporting
        format                = 'PDF'
      importing
        bin_filesize          = lv_filesize
        bin_file              = lv_file
      tables
        otf                   = lt_otf
        lines                 = lt_lines
      exceptions
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        others                = 5.
```

