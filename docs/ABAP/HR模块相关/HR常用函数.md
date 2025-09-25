## 读取0041日期

```abap
methods get_date_for_it0041
  importing
            iv_datar         type any
            is_data          type any
  returning value(rv_result) type dats.
```

```abap
method get_date_for_it0041.

  data: lo_message_handler type ref to if_hrpa_message_handler,
        ls_p0041           type p0041,
        lv_datar           type datar,
        lv_date            type dats.

  move-corresponding is_data to ls_p0041.
  lv_datar = iv_datar.

  call function 'HR_ECM_READ_IT0041_DATE_TYPE'
    exporting
      datar           = lv_datar
      p0041           = ls_p0041
      message_handler = lo_message_handler
    importing
      date            = lv_date.

  rv_result = lv_date.

endmethod.
```

## 读取信息类型文本描述

- 使用函数`RH_TEXT_GET`获取（已过时）

  - 方法定义

    ```abap
    methods get_field_text
      importing
        !is_record       type any "PNNNN结构类型的数据
        !iv_tabname      type any "PNNNN
        !iv_field        type any "字段名
      returning
        value(rv_result) type string .
    ```

  - 方法实施

    ```abap
    method get_field_text.
    
      data: lv_tabname  type tabname,
            lt_fields   type table of rhtext_field,
            lt_text_tab type table of rhtext_tab.
    
      lv_tabname  = iv_tabname.
    
      lt_fields = value #( ( fieldname = iv_field ) ).
    
      call function 'RH_TEXT_GET'
        exporting
          tabname              = lv_tabname
          record               = is_record
          begdat               = gv_begda
          enddat               = gv_endda
          molga                = '28'
        tables
          fields               = lt_fields
          text_tab             = lt_text_tab
        exceptions
          ddic_entry_not_found = 1
          incorrect_call       = 2
          internal_error       = 3
          missing_text         = 4
          others               = 5.
    
      read table lt_text_tab into data(ls_text_tab) index 1.
      rv_result = ls_text_tab-text1.
    
    endmethod.
    ```

- 使用类 `cl_hr_text_identifier` 的方法 `read_text` 获取

  - 方法定义

    ```abap
    methods get_text_for_field
      importing
                iv_tabname       type any
                iv_fieldname     type any
                iv_begda         type dats optional
                iv_endda         type dats optional
                is_record        type any
      returning value(rv_result) type string.
    ```

  - 方法实施

    ```abap
    method get_text_for_field.
    
      data: lv_tabname                 type tabname,
            lv_fieldname               type fieldname,
            lv_text                    type text255,
            lt_special_parameters      type tfieldval,
            lv_text_for_value_read     type flag_x,
            lt_text_identifier_results type txid_t_text_identifier_result.
    
      lv_tabname = to_upper( iv_tabname ).
      lv_fieldname = to_upper( iv_fieldname ).
    
      field-symbols: <fs_begda> type dats,
                     <fs_endda> type dats.
    
      data: lv_begda type ref to data,
            lv_endda type ref to data,
            lv_molga type ref to data.
    
      if iv_begda is supplied.
        assign iv_begda to <fs_begda>.
      else.
        assign component 'BEGDA' of structure is_record to <fs_begda>.
      endif.
    
      if iv_endda is supplied.
        assign iv_endda to <fs_endda>.
      else.
        assign component 'ENDDA' of structure is_record to <fs_endda>.
      endif.
    
      get reference of <fs_begda> into lv_begda.
      get reference of <fs_endda> into lv_endda.
      get reference of '99' into lv_molga.
    
      lt_special_parameters = value #( ( fieldname = 'BEGIN_DATE' fieldvalue = lv_begda )
                                       ( fieldname = 'END_DATE'   fieldvalue = lv_endda )
                                       ( fieldname = 'MOLGA'      fieldvalue = lv_molga )
      ).
    
      new cl_hr_text_identifier( )->read_text(
        exporting
          tabname                    = lv_tabname
          fieldname                  = lv_fieldname
          record                     = is_record
          record_specified           = abap_true
          language                   = sy-langu
          special_parameters         = lt_special_parameters
        importing
          text                       = lv_text
          text_for_value_read        = lv_text_for_value_read
          text_identifier_results    = lt_text_identifier_results
        exceptions
          internal_error             = 1
          illegal_call               = 2
          illegal_table              = 3
          illegal_field              = 4
          no_text                    = 5
          record_required            = 6
          illegal_record             = 7
          table_record_required      = 8
          illegal_table_record       = 9
          special_parameter_mismatch = 10
          others                     = 11
      ).
    
      rv_result = lv_text.
    
    endmethod.
    ```

    

  



