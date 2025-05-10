## CDS系统常量

| 描述      | 值                         |
| --------- | -------------------------- |
| SAP客户端 | `$session.client`          |
| 系统时间  | `$session.system_date`     |
| SAP语言   | `$session.system_language` |
| SAP用户   | `$session.user`            |

## CDS注解

<!-- tabs:start -->

<!-- tab:目录注解 -->

- **CDS视图名称**
  - 注解：`@AbapCatalog.sqlViewName: 'ZFIV_DOCUMENT'`
  - 描述：ABAP词典中CDS数据库视图的名称（即SE11查看的名称）。
  - 值：一个最多 16 位字符的字符串，由字母数字和下划线组成，并以命名空间前缀开头。
- **CDS视图描述**
  - `EndUserText.label: '会计凭证 CDS 视图'`
  - 描述：用户看到的文本（即 SE11 查看的描述 ）。
  - 值：是一个最多 40 位字符的字符串，

<!-- tab:语义注解 -->

- **货币和金额注解**

  - 货币注解：`@Semantics.currencyCode: true`

    该字段被标记为包含货币代码，这可以是 ISO 代码或 SAP 货币代码（数据类型 CUKY）。

  - 金额注解：`@Semantics.amount.currencyCode: 'bseg.h_waers'`

    该字段被标记为货币金额，值为货币字段的名称。

  货币注解和金额注解常一起使用，例如：

  ```abap
  @Semantics.amount.currencyCode: 'bseg.h_hwaer'
  bseg.dmbtr,
  @Semantics.currencyCode: true
  bseg.h_hwaer,
  ```

<!-- tabs:end -->

## CDS使用

<!-- tabs:start -->

<!-- tab:CDS输出 -->

一句话将CDS以ALV形式输出，其中`iv_cds_view_name`为CDS定义的名称

```abap
cl_salv_gui_table_ida=>create_for_cds_view( iv_cds_view_name = 'ZZ_C_FLY' )->fullscreen( )->display( ).
```



<!-- tabs:end -->
