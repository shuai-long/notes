## OM-组织架构管理

- **数据表特性**

  - 主键区域：一般引用结构`HRIKEY`。
  - 控制区域：一般引用结构`HRIADMIN`。
  - 信息类型主数据：HRI+信息类型编号。
  - 可增强区域：CI_P+信息类型编号，所有带CI_PXXXX的信息类型原则上都可以增强。

- **文本存储**

  在组织管理OM和PA人事管理两个模块中，组织单位、职位和职务的文本描述是相互分开的。

  | 模块/文本类型 | 组织单位      | 职位          | 职务          |
  | ------------- | ------------- | ------------- | ------------- |
  | OM            | HRP1000-STEXT | HRP1000-STEXT | HRP1000-STEXT |
  | PA            | T527X-ORGTX   | T528T-PLSTX   | T513S-STLTX   |

- **常用函数**


### 创建对象并创建关系

```abap
data: lt_hri1001    type table of hri1001,
      ls_hrp1000    type hrp1000,
      lv_commit_flg type c.

"--------------------> 创建对象
call function 'RH_OBJECT_CREATE'
  exporting
    plvar = '01'
    otype = 'S'
    short = ls_hrp1000-short
    stext = ls_hrp1000-stext
    begda = ls_hrp1000-begda
  importing
    objid = ls_hrp1000-objid.

"--------------------> 创建关系
lt_hri1001 = value #( plvar = '01'
                      otype = 'S'
                      objid = <fs_flow_item>-plans
                      infty = '1001'
                      rsign = 'A'
                      relat = '003'
                      istat = '1'
                      begda = <fs_flow_item>-begda
                      endda = '99991231'
                      varyf = |O { <fs_flow_item>-orgeh }|
                      seqnr = '000'
                      aedtm = sy-datum
                      uname = sy-uname
                      sclas = 'O'
                      sobid = <fs_flow_item>-orgeh
                      prozt = '000' ).
call function 'RH_INSERT_INFTY_1001_EXT'
  exporting
    fcode                   = 'INSE'
    vtask                   = 'D'
    commit_flg              = lv_commit_flg
  tables
    innnn                   = lt_hri1001
  exceptions
    no_authorization        = 1
    error_during_insert     = 2
    relation_not_reversible = 3
    corr_exit               = 4
    begda_greater_endda     = 5
    others                  = 6.
```

### 信息类型增删改操作

```abap
data: lv_act_fcode       type t77fc-fcode,
      lv_act_infty       type t778t-infty,
      lv_act_subty       type t778u-subty,
      ls_objec           type objec,
      ls_act_pnnnn       type p0001,
      "lt_act_hrtnnnn     type standard table of hrtxxxx,
      lv_suppress_dialog type pppar-dsupr value '2',
      lt_act_mess_info   type hrrhad_msg,
      lv_commit_flg      type c.

call function 'RH_PNNNN_MAINTAIN'
  exporting
    act_fcode           = lv_act_fcode
    act_infty           = lv_act_infty
    act_subty           = lv_act_subty
    act_plvar           = ls_objec-plvar
    act_otype           = ls_objec-otype
    act_objid           = ls_objec-objid
    act_istat           = ls_objec-istat
    act_begda           = ls_objec-begda
    act_endda           = ls_objec-endda
    act_pnnnn           = ls_act_pnnnn
    suppress_dialog     = lv_suppress_dialog
    act_vtask           = 'D'
    act_commit_flg      = lv_commit_flg
  importing
    act_mess_info       = lt_act_mess_info
    "tables
    "act_hrtnnnn         = lt_act_hrtnnnn
  exceptions
    infty_not_valid     = 1
    no_plvar            = 2
    object_not_defined  = 3
    otype_not_valid     = 4
    no_authority        = 5
    action_rejected     = 6
    no_gdate            = 7
    fcode_not_supported = 8.
```

### 其他常用函数

| 函数                          | 描述                                                         |
| ----------------------------- | ------------------------------------------------------------ |
| `HR_ENQUEUE_OBJECT`           | 锁定待操作的OM对象                                           |
| `HR_DEQUEUE_OBJECT`           | 解锁待操作的OM对象                                           |
| `RH_CLEAR_BUFFER`             | 清空缓存                                                     |
| `RH_DELETE_INFTY`             | 删除信息类型数据                                             |
| `RH_INSERT_INFTY`             | 信息类型插入数据                                             |
| `RH_UPDATE_INFTY`             | 更新信息类型数据                                             |
| `RH_UPDATE_DATABASE`          | 提交数据库.如果上述`delete/insert/update`需要整体提交,可在调用时设置参数`VTASK='B'`.然后调佣该函数进行提交 |
| ` RH_READ_INFTY`              | 读取OM信息类型数据                                           |
| `RH_DELETE_OBJECT`            | 删除组织对象（岗位，单位，部门等）                           |
| `RH_READ_INFTY_1000`          | 读取信息类型1000的数据                                       |
| `RH_READ_INFTY_1001`          | 读取信息类型1001的数据                                       |
| `HR_READ_FOREIGN_OBJECT_TEXT` | 返回对象文本                                                 |

> [!Note]
>
> 拿`RH_INSERT_INFTY`举例，其中参数VTASK有如下几种
>
> | VTASK | 介绍                                                         |
> | ----- | ------------------------------------------------------------ |
> | S     | 同步模式,其实这个描述不正确,应该是实时更新模式.具体来说,当完成更新后.系统会自动调用`commit work and wait`直接提交,不受参数`commit_flg`的制约 |
> | V     | 异步更新,更新完成后,系统会判断`commit_flg`是否设置,如果设置会`commit work`.然后继续执行,不会等待更新是否完成 |
> | B     | 更新buffer,需要调用函数`RH_UPDATE_DATABASE`进行提交数据库操作,可用于每次更新多信息类型数据的需求 |
> | D     | Dialog模式,受`commit_flg`制约,看代码,好像和S和V后台都差不多  |

### 常用的对象关系

OM各对象之间均使用对象间关系实现架构搭建，关系由两部分组成：`关系方向/关系类型 ＋ 对象关系`，一般情况下，当创建关系数据时，标准都会在双方的关系数据中产生正向关系和反向关系数据。

- 关系方向包含:

  - 自上而下-A

  - 自下而上-B

- 常用对象关系:

  | 对象关系 | 描述和作用                                                   |
  | -------- | ------------------------------------------------------------ |
  | `002`    | 报告到，即组织单位上下级关系，主要是O和O之间的关系。         |
  | `003`    | 属于，表示O和S之间的管理关系。可理解为该组织单位O下有哪些职位S，或者是该职位S直属于哪个组织单位O。 |
  | `012`    | 管理，表示该组织单位O的管理职位S是哪个（或者哪些）。         |
  | `008`    | 持有人，表示职位S和持有人P的关系                             |
  | `007`    | 描述，表示职位S和岗位描述G之间的从属关系。一般情况下，一个职位只有一个岗位描述，而一个岗位描述对应多个职位。 |

## 读取组织架构


### 代码示例

```abap
data:lv_act_plvar        type objec-plvar,
     lv_act_otype        type objec-otype,
     lt_result_tab       type standard table of swhactor,
     lv_act_objid        type string,
     lt_result_objec     type standard table of objec,
     lv_act_wegid        type gdstr-wegid,
     lt_result_struc     type standard table of struc,
     lv_act_int_flag     type hrrhas-77aw_int,
     lv_act_begda        type objec-begda,
     lv_act_endda        type objec-endda,
     lv_act_tdepth       type hrrhas-tdepth,
     lv_act_tflag        type hrrhas-tflag,
     lv_act_vflag        type hrrhas-vflag,
     lv_authority_check  type hrrhas-authy,
     lv_text_buffer_fill type hrpp0c-test,
     lv_buffer_mode      type flag.

call function 'RH_STRUC_GET'
  exporting
    act_otype        = lv_act_otype        "对象类型 O/S/P
    act_objid        = lv_act_objid        "对象编号
    act_wegid        = lv_act_wegid        "评估路径
    act_int_flag     = lv_act_int_flag     "???未知
    act_plvar        = lv_act_plvar        "活动版本: 01
    act_begda        = lv_act_begda        "开始日期
    act_endda        = lv_act_endda        "结束日期
    act_tdepth       = lv_act_tdepth       "取数层级,默认为0取全部层级,(一般自身算一层,若只读下一层则填2)
    act_tflag        = lv_act_tflag        "提供文本,默认 'X'
    act_vflag        = lv_act_vflag        "提供关系信息,默认'X'
    authority_check  = lv_authority_check  "权限检查,默认'X'
    text_buffer_fill = lv_text_buffer_fill "????未知
    buffer_mode      = lv_buffer_mode      "????未知
  tables
    result_tab       = lt_result_tab        "结果表:所有对象
    result_objec     = lt_result_objec      "结果表:所有对象的基础信息(例如:文本信息)
    result_struc     = lt_result_struc      "结果表:所有对象的层次架构关系
  exceptions
    no_plvar_found   = 1
    no_entry_found   = 2
    others           = 3.
```

### 常用评估路径

在开发过程中，会使用函数`rh_struc_get`来读取评估路径数据，评估路径存储表`T778A`,评估路径文本存储表`T778T`.

| 评估路径   | 描述                           |
| ---------- | ------------------------------ |
| `O-O_DOWN` | 读取组织单位下所有的组织单位   |
| `ORGA-UP`  | 读取组织单位的组织架构（完整） |
| `O-O-S`    | 读取组织单位下所有的职位       |
| `O-O-P`    | 读取组织单位下所有的员工       |
| `O-O`      | 读取组织单位的直属上级组织单位 |
| `S-O`      | 职位所属部门                   |
| `P_S_S_C`  | 人员相关的职位职务信息         |
| `P-S-C-O`  | 人员相关的职位职务组织单位信息 |
| `P-S-O-O`  | 人员的组织架构信息(完整)       |
| `O-P`      | 查询直接挂在当前组织下的员工   |

### 常用事务码

| 事物码          | 描述                                               |
| --------------- | -------------------------------------------------- |
| `PP01`          | 对象信息维护                                       |
| `PP02`          | 对象信息维护（专家模式），每次仅能维护一个信息类型 |
| `PPOCE`         | 创建根组织                                         |
| `PPOME`/`PPOSE` | 更改/查看组织架构                                  |
| `PO10`          | 组织单位维护                                       |
| `PO13`          | 职位维护                                           |
| `PPCM`          | 信息类型增强OM                                     |
| `PPCI`          | OM信息类型创建                                     |
| `RE_RHRHDL00`   | 删除OM对象                                         |