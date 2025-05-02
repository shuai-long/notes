一句话将 CDS 视图以ALV形式输出:

```abap
* iv_cds_view_name 为 CDS的定义名称
cl_salv_gui_table_ida=>create_for_cds_view( iv_cds_view_name = 'ZZ_C_FLY' )->fullscreen( )->display( ).
```

