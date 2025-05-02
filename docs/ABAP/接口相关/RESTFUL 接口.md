[TOC]

# RESTFUL 接口

## 服务端

1. SE24 创建类,并继承 IF_HTTP_EXTENSION 接口．

2. 实现 HANDLE_REQUEST　方法

   ```ABAP
      DATA:  lv_verb   TYPE string,  "HTTP动作：post、get、put、delete
             lv_path   TYPE string,  "http请求路径
             path_info TYPE string,  "定义的服务
             params    TYPE string,  "其他参数
             req_json  TYPE string,  "接收报文
             lv_json   TYPE string.  "返回报文
   
       lv_verb = server->request->get_header_field( '~request_method' ) .
       lv_path = server->request->get_header_field( '~request_uri' ).
       SPLIT lv_path AT '?' INTO path_info params.
   
       IF lv_verb = 'POST' AND path_info = '/cctc/zfi_push_financ'.
         req_json = server->request->get_cdata( ).
       	 /ui2/cl_json=>deserialize( EXPORTING json = req_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = lt_data_in ).
   		
   	    lv_json = /ui2/cl_json=>serialize( data = ls_data_return  compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
   	    server->response->set_status( code = 200 reason = 'Ok' ).
   	    server->response->set_content_type( 'application/json' ).
   	    server->response->set_cdata( data = lv_json ).
       ENDIF.
   ```

3. SICF 创建服务

## 调用端

```ABAP
DATA: len         TYPE i, "发送报文长度
      lv_param    TYPE string,
      http_client TYPE REF TO if_http_client. "http客户端

  "创建http客户端
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  "设定传输请求内容格式以及编码格式
  http_client->request->set_content_type( content_type = 'application/json; charset=utf-8' ).

  "设置http method 为Get
  http_client->request->set_method( if_http_request=>co_request_method_post ).

  "IF_HTTP_ENTITY~CO_REQUEST_METHOD_GET

  "设置待传输内容长度
  len = strlen( gv_json_in ).

  "设置调用方法
  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'POST'.
      
  "设置header字段
  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = 'ClientId'
      value = 'com.cctc.fms'.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = 'OperationCode'
      value = 'com.cctc.ipms.saveRProplanFinancPayRec.post'.

  "设置传入字符串
  CALL METHOD http_client->request->set_cdata
    EXPORTING
      data   = gv_json_in
      offset = 0
      length = len.

  "发送
  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.

  "接收
  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  "提取返回字符串
  result = http_client->response->get_cdata( ).

  "将字符串中的回车符替换，否则abap将会识别为#
  REPLACE ALL OCCURRENCES OF REGEX '\n' IN result WITH space.
  "获取接口返回的数据
*  RESULT = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

```

