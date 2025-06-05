## 结构化搜索帮助

```abap
class-methods set_f4_for_hrobjid
  importing
    iv_otype    type any
    iv_dynfield type any.
```

```abap
method set_f4_for_hrobjid.

  data: lv_otype      type otype,
        ls_sel_object type objec,
        lv_dynfield   type string.

  lv_otype = iv_otype.
  lv_dynfield = to_upper( iv_dynfield ).

  call function 'RH_OBJID_REQUEST'
    exporting
      plvar           = '01'
      otype           = lv_otype
      seark_begda     = sy-datum
      seark_endda     = sy-datum
      set_mode        = space
    importing
      sel_object      = ls_sel_object
    exceptions
      cancelled       = 1
      wrong_condition = 2
      nothing_found   = 3
      internal_error  = 4
      illegal_mode    = 5
      others          = 6.

  if sy-subrc eq 0.
    assign (lv_dynfield) to field-symbol(<fs_objid>).
    <fs_objid> = ls_sel_object-objid.
  endif.

endmethod.
```

## 考核文档搜索帮助

考核文档存储在HRP1000中，对象类型为VA

```abap
class-methods set_f4_for_template
  importing
    iv_dynfield type any.
```

```abap
method set_f4_for_template.

  data: lv_dynfield  type string,
        lt_templates type hap_t_templates,
        ls_return    type bal_s_msg.

  lv_dynfield = iv_dynfield.

  call function 'HRHAP_POPUP_F4_TEMPLATE'
    exporting
      authority_check    = '03'
      plan_version       = '01'
      from_date          = sy-datum
      to_date            = sy-datum
      multiple_selection = ' '
    importing
      t_templates        = lt_templates
      s_return           = ls_return.

  read table lt_templates into data(ls_template) index 1.
  if sy-subrc eq 0.
    assign (lv_dynfield) to field-symbol(<fs_objid>).
    <fs_objid> = ls_template-objid.
  endif.

endmethod.
```



