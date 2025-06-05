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





