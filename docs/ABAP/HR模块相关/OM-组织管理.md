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

### 常用事务码
| 事物码          | 描述                                               |
| --------------- | -------------------------------------------------- |
| `PP01`          | 对象信息维护                                       |
| `PP02`          | 对象信息维护（专家模式），每次仅能维护一个信息类型 |
| `PPOME`/`PPOSE` | 更改/查看组织架构                                  |
| `PO10`          | 组织单位维护                                       |
| `PO13`          | 职位维护                                           |
| `PPCM`          | 信息类型增强OM                                     |
| `PPCI`          | OM信息类型创建                                     |
| `RE_RHRHDL00`   | 删除OM对象                                         |