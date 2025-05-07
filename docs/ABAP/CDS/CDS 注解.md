# 系统常量

- SAP 客户端

  ```abap
  $session.client
  ```

- 系统时间

  ```abap
  $session.system_date
  ```

- SAP 语言

  ```abap
  $session.system_language
  ```

- SAP 用户

  ```abap
  $session.user
  ```

# 目录注解

## CDS 视图名称

- 注解：`@AbapCatalog.sqlViewName: 'ZFIV_DOCUMENT'`

- 描述：ABAP 词典中 CDS 数据库视图的名称（即 SE11 查看的名称）
- 值：是一个最多 16 位字符的字符串，由字母数字和下划线组成，并以命名空间前缀开头。

## CDS 视图描述

- 注解：`EndUserText.label: '会计凭证 CDS 视图'`
- 描述：用户看到的文本（即 SE11 查看的描述 ）
- 值：是一个最多 40 位字符的字符串，

# 语意注解

## 货币和金额注解

- 货币注解：`@Semantics.currencyCode: true`

  该字段被标记为包含货币代码，这可以是 ISO 代码或 SAP 货币代码（数据类型 CUKY）。

- 金额注解：`@Semantics.amount.currencyCode: 'bseg.h_waers'`

  该字段被标记为货币金额，值为货币字段的名称。

- 货币注解和金额注解常一起使用，例如：

  ```abap
  @Semantics.amount.currencyCode: 'bseg.h_hwaer'
  bseg.dmbtr,
  @Semantics.currencyCode: true
  bseg.h_hwaer,
  ```

  

