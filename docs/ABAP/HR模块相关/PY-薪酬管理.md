## 核算状态与模拟核算

薪酬报表开发可参考标准Demo: `EXAMPLE_PNP_GET_PAYROLL`

### 薪资核算状态

- 薪资核算状态，`PA03`可查询每个工资范围的核算状态及期间

  | 状态 | 描述                                          |
  | ---- | --------------------------------------------- |
  | 1    | 为薪资发放而释放,会锁定相关人员的薪资相关信息 |
  | 2    | 工资发放改正                                  |
  | 3    | 退出工资发放                                  |
  | 4    | 检查发放结果,会锁定相关人员的薪资相关信息     |

- 存储表

  - `T569U`  工资范围工资核算日志

  - `T569V`  工资范围当前状态和核算期间 

- 查询当前工资范围的核算状态

  ```abap
  select state into @data(lv_state) from t569v where abkrs eq @lv_abkrs and pabrj eq @lv_pabrj and pabrp eq @lv_pabrp.
  ```

### 获取薪资模拟核算结果

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

### 常用薪资结果表

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



## 获取薪酬结果

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

## 其他常用函数

| 函数                        | 描述                       |
| --------------------------- | -------------------------- |
| `PYXX_GET_RELID_FROM_PERNR` | 读取员工区域标示和国家分组 |
|                             |                            |

## 常用事务码

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
| `pc00_m99_cwtr`       | 薪酬回算结果报表                                             |

## 常用信息类型

| 信息类型 | 描述            | 备注 |
| -------- | --------------- | ---- |
| `0008`   | 基本工资        |      |
| `0014`   | 经常性支付/扣除 |      |
| `0015`   | 额外支付款      |      |
| `0267`   | 附加非周期支付  |      |
| ``       |                 |      |

