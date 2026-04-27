## PY-薪酬管理

薪酬报表开发可参考标准Demo: `EXAMPLE_PNP_GET_PAYROLL`

- **薪资核算状态**

  - 薪资核算状态，`PA03`可查询每个工资范围的核算状态及期间

    | 状态 | 描述                                          |
    | ---- | --------------------------------------------- |
    | 1    | 为薪资发放而释放,会锁定相关人员的薪资相关信息 |
    | 2    | 工资发放改正                                  |
    | 3    | 退出工资发放                                  |
    | 4    | 检查发放结果,会锁定相关人员的薪资相关信息     |

  - 存储表

    - `T569U`  工资范围当前状态和核算期间

    - `T569V`  工资范围工资核算日志

  - 查询当前工资范围的核算状态

    ```abap
    select state into @data(lv_state) from t569v where abkrs eq @lv_abkrs and pabrj eq @lv_pabrj and pabrp eq @lv_pabrp.
    ```

- **获取薪资模拟核算结果**

  1. 调用工资核算程序

     ```abap
     submit hcncalc0
      with pnpxabkr = p_xabkr
      with pnptimra = 'X'
      with pnptimr9 = ' '
      with pnppabrp = p_abrp0
      with pnppabrj = p_abrj0
      with pnppernr in pnppernr
      with pnpabkrs in pnpabkrs

      "with ocrsn    = p_ocrsn "非周期工资核算的原因
      "with payty    = p_payty "非周期的工资核算
      "with payid    = p_payid "非周期的工资核算
      "with bondt    = p_bondt "非周期的工资核算--非周期工资发放付款日期

      with schema   = 'ZN28'
      with tst_on   = 'X'
      with test     = 'NOUPD/RT/OFF' "程序中有判断 test 中包含 RT 会抛内存出来
      with ecalled  = 'X'  "保存缓冲区到内存中
      with sw_spool = ' '  "使用提交启动的计算
      with prt_prot = ' '  "不打印日志
      and return.
     ```

  2. 获取内存拿值

     ```abap
     data rt type table of pc207 with header line.
     import rt = rt from memory id 'RT'.
     free memory id 'RT'.
     ```

- **获取薪资结果**

  薪资结果的数据类型为`PAY99_RESULT`(国际通用)、`PAYCN_RESULT`(中国),该类型为一个多层次嵌套类型:

  - `PAY99_RESULT-INTER-RT`: 工资核算结果明细表,存储了员工的所有应发,实发,税额等等明细.一般薪酬报表开发中,都从该字表中读取对应的工资明细信息.
  - `PAY99_RESULT-INTER-BT`:  实际支付金额,银行基本信息
  - `PAYCN_RESULT-NAT-TCRT`:  税收累计（累计类型：CUMTY，（Y 为按年累计））

  > [!Warning]
  >
  > 读取员工某个期间的工资发放明细,类型 PAY99_RESULT / PAYCN_RESULT.一定要设置参数`READ_ONLY_INTERNATIONAL`,才能使用PAY99_RESULT.

  读取员工所有的薪资发放结果`PC261`

  - `BONDT`:  非周期性发放日期

  - `PAYTY`:  支付类型:  A奖金

  - `SRTZA`:  当前标识符， O: 第一条数据; P: 中间数据; 'A': 最新一条数据

  - 薪资期间：

    - `FPPER`:  工资核算历经期 (YYYYMM)
    - `FPBEG`:  工资发放期间的开始(历经期)
    - `FPEND`:  工资发放期间的结束(历经期)
    - `INPER`:  工资发放所在期 (YYYYMM)
    - `IPEND`:  工资发放期间的结束(所在期间)


  > [!Note]
  >
  > 1. CRT:  累计结果（月度、季度、年度），参考：PC208
  >    TCRT: 记录和税相关的工资项 & 影响上发薪和下发薪，参考： PC2G5
  >    上发薪：一年的税对应当年1至12月公司发放的工资
  >    下发薪：一年的税对应去年12月至当年11月公司发放的工资
  >
  > 2. 下图结果是带回算数据：
  >
  >    核算五月工资 --> 六月回算五月数据 --> 七月回算五月和六月数据 --> 八月回算六月和七月数据
  >
  >    ![image-20250825171537722](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250825171537722.png)
  >




### 调用函数

```abap
data: lt_rgdir     type table of pc261,
      lt_payresult type paycn_result,
      ls_rt        type pc207,
      lv_nr        type pc261-seqnr.
```

```abap
call function 'CU_READ_RGDIR'
  exporting
    persnr          = ls_data_in-pernr
  tables
    in_rgdir        = lt_rgdir
  exceptions
    no_record_found = 1
    others          = 2.
```

```abap
read table lt_rgdir into gs_rgdir with key fpper = ls_data_in-fpper.
if sy-subrc = 0.
  lv_nr = sy-tabix.

  call function 'PYXX_READ_PAYROLL_RESULT'
    exporting
      clusterid                    = 'CN'
      employeenumber               = ls_data_in-pernr
      sequencenumber               = lv_nr
    changing
      payroll_result               = lt_payresult
    exceptions
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      error_reading_archive        = 8
      error_reading_relid          = 9
      others                       = 10.
endif.
```

### import

```abap
data: lv_key   type pcl2-srtfd,
      lv_pernr type pernr_d,
      lt_rgdir type standard table of pc261,
      lt_rt    type standard table of pc207,
      lt_tcrt  type standard table of pc2g5.
```

```abap
 lv_key = |{ ls_alv-pernr alpha = in }|.
 import rgdir = lt_rgdir from database pcl2(cu) id lv_key.
```

```abap
loop at lt_rgdir into data(ls_rgdir).
  "---------------------> 过滤条件

  "---------------------> 取数
  lv_key = |{ lv_pernr }{ ls_rgdir-seqnr }|.
  import rt = lt_rt from database pcl2(cn) id lv_key.
  import tcrt = lt_tcrt from database pcl2(cn) id lv_key.

endloop.
```

> [!Note]
>
> - `PCL1`  主要存储一些信息类型的文本信息
>
> - `PCL2`  主要存储员工工资核算结果,时间评估数据
> - `PCL3`  待补充
> - `PCL4`  待补充

### 其他常用函数

| 函数                        | 描述                       |
| --------------------------- | -------------------------- |
| `PYXX_GET_RELID_FROM_PERNR` | 读取员工区域标示和国家分组 |
|                             |                            |


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
