# ABAP 调用 CPI 接口

## 1. 证书导出

- **方式一：** 导出一个证书文件

  1. 进入 实用和租例->点击集成->点击集成和 API->点击全部![image-20240702162218789](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702162218789.png)

  2. 进入该界面，并导出该界面的证书![image-20240702164144100](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164144100.png)

     ![image-20240702164247223](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164247223.png)

     ![image-20240702164355589](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164355589.png)

     ![image-20240702164514164](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164514164.png)

- **方式二：** 导出三个证书文件

  1. 进入 实用和租例->点击集成->点击集成和 API->点击密匙库![image-20240702165054716](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165054716.png)
  2. 导出 sap_digicent global ca g2、sap_digicent global root ca、sap_digicent global root g2 三个证书。![image-20240702165212793](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165212793.png)
  3. 点击三个点，并选择下载。另外两个同理![image-20240702165404612](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165404612.png)

## 2. 证书导入

1. 进入 T-code：STRUST![image-20240702165711363](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165711363.png)
2. 选择： SSL 客户端 SSL 客户端（匿名）![image-20240702165829831](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165829831.png)
3. 点击更改![image-20240702165941496](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165941496.png)
4. 点击导入文件![image-20240702170233532](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170233532.png)
5. 选择证书文件（上一步导出的证书）![image-20240702170338686](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170338686.png)
6. 点击添加到证书列表![image-20240702170536493](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170536493.png)
7. 点击保存即可![image-20240702170616971](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170616971.png)

## 3. 创建代理

1. 进入 T-code： SM59![image-20240702170727816](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170727816.png)

2. 点击创建，输入目标信息![image-20240702170944685](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170944685.png)

3. 点击技术设置，维护目标系统设置信息![image-20240702171548410](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702171548410.png)

4. 点击技术设置，维护 HTTP 代理选项信息![image-20240702171811064](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702171811064.png)

5. 维护 登陆&安全性 信息，维护信息如图所示，其他不变。![image-20240702172334775](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702172334775.png)

   > **注：** 如果配置登陆用户的账号密码， 每次保存时都要重新输入密码，否则密码会清空。

6. 点击保存![image-20240702172505040](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702172505040.png)

## 4. 代码调用

```abap
********************************************************************** 创建连接
data: lv_url           type string,
      lv_proxy_host    type string value 'proxy',
      lv_proxy_service type string value '3128',
      lv_sslid         type ssfapplssl value 'ANONYM',
      lo_http_client   type ref to if_http_client.

cl_http_client=>create_by_url(
  exporting
    url                        = lv_url
    proxy_host                 = lv_proxy_host
    proxy_service              = lv_proxy_service
    ssl_id                     = lv_sslid
  importing
    client                     = lo_http_client
  exceptions
    argument_not_found         = 1
    plugin_not_active          = 2
    internal_error             = 3
    pse_not_found              = 4
    pse_not_distrib            = 5
    pse_errors                 = 6
    oa2c_set_token_error       = 7
    oa2c_missing_authorization = 8
    oa2c_invalid_config        = 9
    oa2c_invalid_parameters    = 10
    oa2c_invalid_scope         = 11
    oa2c_invalid_grant         = 12
    others                     = 13
).

if sy-subrc <> 0.
*  message id sy-msgid type sy-msgty number sy-msgno
*    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
endif.

********************************************************************** 设置连接的账号密码
data: lv_auth_type type i value 1,
      lv_username  type string, "CPI 接口的账号，可在 BTP 中查看
      lv_password  type string. "CPI 接口的密码，可在 BTP 中查看

lo_http_client->request->set_authorization(
  auth_type = lv_auth_type
  username  = lv_username
  password  = lv_password
).

********************************************************************** 设置 request 请求方式
lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

********************************************************************** 设置 request 传输协议
lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

********************************************************************** 设置 request 请求内容格式
lo_http_client->request->set_content_type( content_type = 'application/json; charset=utf-8' ).

********************************************************************** 设置 request 请求头
data: lt_header_fields type tihttpnvp.
lt_header_fields = value #( "可按需修改
    ( name = 'sap-client'       value = sy-mandt )
    ( name = '~server_protocol' value = 'HTTP/1.1' )
    ( name = 'Content-Type'     value = 'application/json' )
).
lo_http_client->request->set_header_fields( fields = lt_header_fields ).

********************************************************************** 设置 request 请求内容（二选一）
data: lv_reqjson type string. " 字符串报文
lo_http_client->request->set_cdata( data = lv_reqjson ).

*data: lv_xreqjson type xstring. " 16 进制报文
*lo_http_client->request->set_data( data = lv_xreqjson ).

********************************************************************** 发送数据
lo_http_client->send(
  exceptions
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4
    others                     = 5 ).
if sy-subrc <> 0.
  lo_http_client->get_last_error( importing message = data(lv_message) ).
endif.

********************************************************************** 获取回复
lo_http_client->receive(
  exceptions
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    others                     = 4 ).
if sy-subrc <> 0.
  lo_http_client->get_last_error( importing message = lv_message ).
endif.

********************************************************************** 获取回复状态码
lo_http_client->response->get_status( importing code = data(lv_code) ).

********************************************************************** 获取返回报文 （二选一即可）
data(lv_resjson) = lo_http_client->response->get_cdata( ).  "字符串返回报文
*data(lv_xresjson) = lo_http_client->response->get_data( ). "16 进制返回报文

********************************************************************** 关闭链接
lo_http_client->close( ).
```

