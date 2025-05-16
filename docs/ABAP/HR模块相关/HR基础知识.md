## OM-组织架构管理

- **数据表特性**

  - 主键区域：一般引用结构`HRIKEY`。
  - 控制区域：一般引用结构`HRIADMIN`。
  - 信息类型主数据：HRI+信息类型编号。
  - 可增强区域：CI_P+信息类型编号，所有带CI_PXXXX的信息类型原则上都可以增强。

- 

  

## 读取组织架构

<!-- tabs:start -->

<!-- tab:代码示例 -->

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

<!-- tab:常用评估路径 -->

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

<!-- tabs:end -->

## 附录

<!-- tabs:start -->

<!-- tab:常用事物码 -->

HR模块常用事物码如下：

| 事物码 | 描述                       | 事物码 | 描述                   |
| ------ | -------------------------- | ------ | ---------------------- |
| `PO10` | 组织单位维护               | `PO13` | 职位维护               |
| `OOSP` | 创建结构化权限参数文件     | `OOSB` | 分配结构化权限参数文件 |
| `OOAC` | 授权权限对象主开关配置使用 |        |                        |

<!-- tab:常用的对象关系 -->

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
  

<!-- tabs:end -->