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
  
  
  
  <!-- tabs:start -->
  
  <!-- tab:调用函数 -->
  
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
  
  <!-- tab:import -->
  
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
  
  <!-- tab:其他常用函数 -->
  
  | 函数                        | 描述                       |
  | --------------------------- | -------------------------- |
  | `PYXX_GET_RELID_FROM_PERNR` | 读取员工区域标示和国家分组 |
  |                             |                            |
  
  <!-- tabs:end -->

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

<!-- tabs:start -->

<!-- tab:OM模块 -->

| 事物码          | 描述                                               |
| --------------- | -------------------------------------------------- |
| `PP01`          | 对象信息维护                                       |
| `PP02`          | 对象信息维护（专家模式），每次仅能维护一个信息类型 |
| `PPOME`/`PPOSE` | 更改/查看组织架构                                  |
| `PO10`          | 组织单位维护                                       |
| `PO13`          | 职位维护                                           |
| `PPCM`          | 信息类型增强OM                                     |
| `PPCI`          | OM信息类型创建                                     |
|                 |                                                    |

<!-- tab:PA模块 -->

| 事物码        | 描述                  |
| ------------- | --------------------- |
| `PA20`/`PA30` | 员工个人信息查询/维护 |
| `PA40`        | 员工事件维护          |
| `PU22`        | HR数据归档            |
| `PU00`        | 删除员工信息          |
| `PM01`        | 信息类型增强 PA       |
| `OAAD`        | 员工头像上载          |
| `PA04`        | 员工编号范围维护      |
|               |                       |

<!-- tab:PT模块 -->

| 事物码      | 描述             |
| ----------- | ---------------- |
| `PT60`      | 时间评估         |
| `PT61`      | 事件报表         |
| `PT62`      | 出勤列表         |
| `PT63`      | 个人工作计划查询 |
| `PT64`      | 缺勤列表         |
| `PA51`      | 显示员工时间记录 |
| `PA61`      | 显示员工日历     |
| `PT_CLSTB2` | 时间评估结果查询 |
|             |                  |

<!-- tab:PY模块 -->

| 事物码                | 描述                                                         |
| --------------------- | ------------------------------------------------------------ |
| `PE01`                | 模式创建和维护                                               |
| `PE02`                | 计算规则创建和维护                                           |
| `PE04`                | 薪酬函数创建和维护，一般新建的需要放在 include rpcburz0中，可新建一个Z的include，然后form放于其中 |
| `PDSY`                | 说明文档查询和维护                                           |
| `PA03`                | 工资发放控制，可查询当前工资范围的核算区间、状态             |
| `PC00_M99_PA03_RELEA` | 发布工资发放                                                 |
| `PC00_M28_CALC_SIMU`  | 工资核算模拟（中国）                                         |
| `PC00_M28_CALC`       | 工资核算（中国）                                             |
| `PC00_M99_PA03_CHECK` | 检查结果                                                     |
| `PC00_M99_PA03_CORR`  | 更正                                                         |
| `PC00_M99_PA03 _END`  | 退出工资核算                                                 |
| `PC_PAYRESULT`        | 显示工资核算结果                                             |
| `PU01`                | 删除当前的工资发放结果                                       |
| `PU03`                | 更改员工工资核算状态                                         |
| `PC00_M99_CIPE`       | 创建过帐运行                                                 |
| `PC00_M28_CEDT`       | 薪酬报表（中国）                                             |
| `RPUDEL20`            | (程序),批量删除指定员工的工资核算                            |

<!-- tab:权限 -->

| 事物码 | 描述                       |
| ------ | -------------------------- |
| `OOSP` | 创建结构化权限参数文件     |
| `OOSB` | 分配结构化权限参数文件     |
| `OOAC` | 授权权限对象主开关配置使用 |
|        |                            |

<!-- tabs:end -->

<!-- tab:常用表 -->

<!-- tabs:start -->

<!-- tab:Others -->

- 信息类型相关: `T582A`

  - 子类型对应字段 `T582A-NAMST`

  - 可用子类型存储表 `T582A-STYPT` 

  - 可用子类型文本表 `T582A-SYTXT`

  - 子类型时间限制表 `T582A-ZBTAB`

  - 信息类型时间限制 `T582A-ZEITB`

    ```abap
    select single zeitb into @data(lv_zeitd) from t582a where infty eq @lv_infty.
    ```

    - 1: 记录存在必须无间断，无重复
    - 2: 记录可含间断，不能重复
    - 3: 记录可含间断并且可以存在不只一次，可重复
    - T: 时间约束基于子类型或子类型表，子类型时间限制
    - Z: 时间管理信息类型的时间约束种类 -> T554Y
    - A: 从1800年1月1日到9999年十二月12日信息类别仅存在一次
    - B: 自1800年1月1日到9999年12月12日中IT最多存在一次

- 定额分组相关

  - 员工子组时间定额分组维护视图: `V_503_E`
  - 人事子范围时间定额分组维护视图: `V_001P_I`

- 工资项相关

  - 判断工资项是否累加项 `T512W`。*(T54C3是做什么的？)*

    ```abap
    select single kumul into @data(lv_kumul_raw) from t512w
      where molga eq '28' and lgart eq @lv_lgart and begda le @sy-datum and endda ge @sy-datum.
    
    write lv_kumul_raw to lv_kumul.
    if lv_kumul+22(1) eq '4'.
      "第23位为4则为累计项
    endif.
    ```

  - 判断工资项是否扣减项 `T511`

    `T511-OPKEN=' '` 付款

    `T511-OPKEN='A'` 扣减

    ```abap
    select count(*) from t511 where molag = '28' and lgart eq lv_lgart and endda = '99991231'.
    ```

  - 工资项对于员工子组和人事范围的有效性 `T511`

    - `T511-ABTYZ` 对应员工子组分组 (`V_503_ALL` 查看员工组子组分组)
    - `T511-WKTYZ` 对应人事子范围分组 (`V_001P_ALL` 查看人事子范围分组)

    ![V_511_B配置视图](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611174250139.png)

- 屏幕控制相关：

  - PA20/PA30 控制屏幕字段显示与否`V_T588M`。
  - 员工入职函数必填校验：`V_T588MFPROPS` 和 `V_T588MFPROPC`。


<!-- tab:0000 -->

| 字段    | 描述     | 值表    | 文本表-字段     | 备注                                                         |
| ------- | -------- | ------- | --------------- | ------------------------------------------------------------ |
| `massn` | 操作类型 | `T529A` | `T529T`-`MNTXT` | `t588b`(pa40人事事件：`mntyp = 'M' and menue = '01' and userg = '28'`) |
| `massg` | 操作原因 | `T530`  | `T530T`-`MGTXT` | `t530_delimit`表存储操作原因的有效期                         |

<!-- tab:0001 -->

| 字段    | 描述       | 值表    | 文本表-字段     | 备注                              |
| ------- | ---------- | ------- | --------------- | --------------------------------- |
| `persk` | 员工组     | `T501`  | `T501T`-`PTEXT` |                                   |
| `persg` | 员工子组   | `T503`  | `T503T`-`PTEXT` | `t503z`表存储员工子组和国家的关系 |
| `werks` | 人事范围   | `T500P` | `T500P`-`NAME1` |                                   |
| `btrtl` | 人事子范围 | `T001P` | `T001P`-`BTEXT` |                                   |
| `abkrs` | 工资范围   | `T549A` | `T549T`-`ATEXT` |                                   |
|         |            |         |                 |                                   |
|         |            |         |                 |                                   |
|         |            |         |                 |                                   |
|         |            |         |                 |                                   |

<!-- tabs:end -->

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