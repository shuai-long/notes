## 权限检查

区别于常用的 PFCG 权限控制模式，结构化权限控制以组织结构对象为控制对象，并可以控制评估路径（根据评估路径，可获取不同的结构化数据）。然后再分配给特定的人员。权限控制更精准的方便。[官方帮助文档](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/c6c3ffd90792427a9fee1a19df5b0925/6903dd5321e8424de10000000a174cb4.html),[参考链接1](https://blog.csdn.net/ROYHAO/article/details/132556810)[参考链接二](https://blog.sina.com.cn/s/blog_6fe5fac90102ys87.html)

<!-- tabs:start -->

<!-- tab:结构化权限检查 -->

<!-- tabs:start -->

<!-- tab:结构化权限配置 -->

- 创建权限参数文件

  - 使用事物码：`OOSP`

    ![结构化权限参数文件](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/0d65da413e3a6e72f345fb80c22b381c.png)


  - 其中期间的选项释义如下：

    | 选项   | 描述     | 期间                                          |
    | ------ | -------- | --------------------------------------------- |
    | （空） | 全部     | 1900.01.01 - 9999.12.31                       |
    | `D`    | 关键日期 | 系统当前日期                                  |
    | `M`    | 当前月   | 系统当前日期所在月初一 - 系统当前日期所在月末 |
    | `Y`    | 当前年   | 系统当前日期所在年初一 - 系统当前日期所在年末 |
    | `P`    | 过期     | 1900.01.01 - 系统当前日期                     |
    | `F`    | 未来     | 系统当前日期 - 9999.12.31                     |


- 分配权限参数文件

  - 使用事物码：`OOSB`

    ![分配结构化参数文件](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/af3cc6184e9620742bcbacc17c076d64.png)


<!-- tab:结构化权限函数 -->

```abap
data:lv_fcode        type string,
     lv_plvar        type string,
     lv_otype        type string,
     lv_objid        type string,
     lv_with_base_ac type string.

call function 'RH_STRU_AUTHORITY_CHECK'
  exporting
    fcode                    = lv_fcode "默认DISP: DISP(显示) CUTI(定界) INSE(创建) DELO(删除) 具体值可查看T77FC
    plvar                    = lv_plvar "计划版本: 默认 01
    otype                    = lv_otype "对象类型: O/S/P
    objid                    = lv_objid "对象标识:
    with_base_ac             = lv_with_base_ac "默认 X
  exceptions
    no_stru_authority        = 1
    no_stru_authority_hyper  = 2
    no_stru_authority_at_all = 3
    no_base_authority        = 4
    others                   = 5.

```
<!-- tabs:end -->

<!-- tab:PFCG权限检查 -->

<!-- tabs:start -->

<!-- tab:PFCG权限对象配置 -->

- 角色配置使用`PFCG`，常用权限对象如下：

  | 权限对象  | 作用                                     | 示例                                                         |
  | --------- | ---------------------------------------- | ------------------------------------------------------------ |
  | `PLOG`    | 校验人员计划的权限                       | PLVAR：*<br/>OTYPE：C，O，P，S，T，US<br/>INFOTYP：1000，1001<br/>SUBTYP：\*<br/>ISTAT：\*<br/>PPFCODE： DISP，LISD |
  | `P_ABAP`  | 简化指定程序的权限检查                   | REPID：HCNCALC0<br/>COARS：全部值                            |
  | `P_ORGIN` | 校验人员信息类型的权限                   | INFTY：0000-9051<br/>SUBTY：*<br/>AUTHC：M，R<br/>PERSA：9005<br/>PERSG： *<br/>PERSK： *<br/>VDSK1： * |
  | `P_PCLX`  | 校验PCLx（ x =1、 2、 3、4 ） 簇表的权限 | RELID：\*<br/>AUTHC：*                                       |
  | `P_PCR`   | 校验工资核算范围的权限                   | ABRKS：MS<br/>ACTVT：*                                       |
  | `P_PERNR` | 校验人员编号权限                         | AUTHC：\*<br/>PSIGN：\*<br/>INFTY：\* <br/>SUBTY：\*         |


<!-- tab:PFCG权限函数 -->

- 基本权限检查

  ```abap
  data: lv_fcode type string,
        lv_plvar type string,
        lv_otype type string,
        lv_infty type string,
        lv_subty type string,
        lv_istat type string.
  
  call function 'RH_BASE_AUTHORITY_CHECK'
    exporting
      fcode             = lv_fcode  "默认DISP: DISP(显示) CUTI(定界) INSE(创建) DELO(删除) 具体值可查看T77FC
      plvar             = lv_plvar  "计划版本: 默认 01
      otype             = lv_otype  "对象类型: O/S/P
      infty             = lv_infty  "信息类型:
      subty             = lv_subty  "子类型:
      istat             = lv_istat  "
    exceptions
      no_base_authority = 1
      others            = 2.
  
  ```

- PA权限检查

  ```abap
  DATA: lv_tclas TYPE pspar-tclas,
        lt_i0001 TYPE STANDARD TABLE OF p0001,
        lv_pernr TYPE prelp-pernr,
        lv_infty TYPE prelp-infty,
        lv_subty TYPE prelp-subty,
        lv_begda TYPE prelp-begda,
        lv_endda TYPE prelp-endda,
        lv_level TYPE authc_d,
        lv_uname TYPE syuname.
  
  CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY'
    EXPORTING
      tclas            = lv_tclas  "默认A
      pernr            = lv_pernr
      infty            = lv_infty
      subty            = lv_subty
      begda            = lv_begda
      endda            = lv_endda
      level            = lv_level  "默认R
      uname            = lv_uname 
    TABLES
      i0001            = lt_i0001
    EXCEPTIONS
      no_authorization = 1
      internal_error   = 2
      OTHERS           = 3.
  
  ```

<!-- tabs:end -->

<!-- tab:授权主开关配置 -->

- 使用事物码：`OOAC`

![授权主开关配置](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/ccd2ac5cc3b05e09e333254582804851.png)

- 其中常用配置释义如下：

  | 语义缩写 | 描述                    | 作用                                                     |
  | -------- | ----------------------- | -------------------------------------------------------- |
  | ADAYS    | HR：权限检查的时间容差  | 设置一个天数，默认是15，权限调整后这个天数内依然保留权限 |
  | ORGIN    | HR: 主数据              | 是否检查权限对象P_ORGIN                                  |
  | ORGPD    | HR：结构权限检查        | 是否启用结构化授权检查                                   |
  | ORGXX    | HR：主数据 - 扩展的检查 | 是否检查权限对象P_ORGXX                                  |
  | PERNR    | HR：主数据 - 个人号检查 | 是否检查权限对象P_PERNR                                  |


<!-- tabs:end -->
