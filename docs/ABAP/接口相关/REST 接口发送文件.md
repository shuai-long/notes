```abap
"创建链接
data: lv_url type string.
cl_http_client=>create_by_url(
  exporting
    url                = lv_url
  importing
    client             = data(lo_http_client)
  exceptions
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
).

"设置 header 参数
data: lt_header_fields type tihttpnvp.
lt_header_fields = value #(  "仅示例，按照实际需求更改
   ( name = 'Authorize' value = '' ) "请求 token
   ( name = 'SystemID'  value = 's00' ) "系统标识
).
lo_http_client->request->set_header_fields( fields = lt_header_fields ).

"设置 请求方式 和 传输协议版本
lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

"设置请求内容类型
lo_http_client->request->if_http_entity~set_content_type( 'multipart/form-data' ).
lo_http_client->request->if_http_entity~set_formfield_encoding( cl_http_request=>if_http_entity~co_encoding_raw ).

"设置文件属性
data(lo_part) = lo_http_client->request->if_http_entity~add_multipart( ).
data(lv_filename) = |form-data; name="file";filename="{ '' }"|.
lo_part->set_header_field(
  exporting
    name  = 'content-disposition'
    value = lv_filename ).
lo_part->set_header_field(
  exporting
    name  = 'content-type'
    value = 'bin' ).

"设置文件内容
data: lv_xfile type xstring. "文件内容的十六进制
data(lv_file_size) = xstrlen( lv_xfile ).
lo_part->set_data(
  exporting
    data   = lv_xfile
    offset = 0
    length = lv_file_size ).

"发送数据
lo_http_client->send(
  exceptions
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4
    others                     = 5
).
if sy-subrc <> 0.
  lo_http_client->get_last_error( importing message = data(lv_message) ).
endif.

"接收返回结果
lo_http_client->receive(
  exceptions
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3 ).
if sy-subrc <> 0 .
  lo_http_client->get_last_error( importing message = lv_message ).
endif.

"获取结果内容
data(lv_resjson) = lo_http_client->response->get_cdata( ).

"关闭连接
lo_http_client->close( ).
```

