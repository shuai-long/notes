## PT-时间管理

时间管理模块主要用来管理员工的时间信息、考勤、缺勤、出差、加班等等信息。从SAP HR的角度上看分为正向考勤和逆向考勤。

- 正向考勤：正向考勤及记录员工所有和时间相关的记录，如上下班打卡记录、缺勤记录、加班记录、出差记录。

- 逆向考勤：逆向考勤，即不考虑员工的员工的上下班时间，只记录与员工计划工作时间相违背的时间数据。例如加班、缺勤、替班等等。

在每月计算工资前，需先运行考勤评估（事物码`PT60`），考勤评估过程中，也是用schema来运行计算的。通常情况下正向考勤可参考`TM00`，逆向考勤可参考`TM04`进行调整和修改。

### **常用表**

- `T552A` : 考勤记录表, 该表按月记录了考勤记录，且每日的考勤记录分别对应`TPR`和`FTK`的字段。类似于0041的日期记录。
- `T550A` : 员工每日工作计划表

### **常用函数**

<!-- tabs:start -->

<!-- tab:获取日工作计划 -->

<!-- tabs:start -->

<!-- tab:获取单人工作计划 -->

```abap
data: lt_psp           type table of ptpsp,
      lv_rdclust       type rdclst value 'X',
      lv_switch_active type c value '0'.
call function 'HR_PERSONAL_WORK_SCHEDULE'
  exporting
    pernr             = pernr-pernr
    begda             = pn-begda
    endda             = pn-endda
    switch_activ      = lv_switch_active
    i0001_i0007_error = '0'
    read_cluster      = lv_rdclust
  tables
    i0000             = p0000
    i0001             = p0001
    i0002             = p0002
    i0007             = p0007
    i2001             = p2001
    i2002             = p2002
    i2003             = p2003
    perws             = lt_psp
  exceptions
    error_occured     = 1
    abort_occured     = 2
    others            = 3.
```

<!-- tab:批量获取日工作计划 -->

```abap
data: lt_pernr  type table of pdpnr ,   "功能模块调用的含人员编号的结构
      lt_psp    type table of pdpsp,    "包括缺勤/出勤/待命责任等的人员轮班日程表
      lt_daypsp type table of pdsppsp . " 缺勤/出勤少于一天的人员

lt_pernr = value #( ( pernr = '' ) ).
call function 'HR_PERSON_READ_WORK_SCHEDULE'
  exporting
    begin_date         = pn-begda
    end_date           = pn-endda
*   grouping_dws       =
*   grouping_attendence       =
*   grouping_substitute       =
*   read_from_database = space
*   im_read_no_locked_records =
  tables
    pernr_tab          = lt_pernr
    psp                = lt_psp
    day_psp            = lt_daypsp
*  changing
*   ch_auth_infty_tab  =
  exceptions
    error_in_build_psp = 1
    others             = 2.
```

<!-- tabs:end -->

<!-- tab:读取个人考勤记录 -->

```abap
data: ls_time_b2 type hrf_tim_b2.
call function 'HR_FORMS_TIM_GET_B2_RESULTS'
  exporting
    pernr  = pernr-pernr
    begda  = pn-begda
    endda  = pn-endda
  importing
    tim_b2 = ls_time_b2.
```

<!-- tab:计算缺勤时长 -->

`HR_ABS_ATT_TIMES_AT_ENTRY`,函数关键填充，0000，0001，0002，0007，2001，2002，2003等信息类型数据。[参考链接1](https://blog.csdn.net/wl8511/article/details/142291441)

```abap
types: begin of ty_input,
          pernr type string,
          awart type string,
          begda type string,
          endda type string,
          beguz type string,
          enduz type string,
       end of ty_input.
       
types: begin of ty_output,
          stdaz   type string,
          abwtg   type string,
          msgtype type string,
          msgtext type string,
        end of ty_output .
        
methods calc_vacation
  importing
  	value(is_input)  type ty_input
  returning
  	value(rs_output) type ty_output .   
```

```abap
method calc_vacation.
  data:
    lt_0001 type tyt_0001,
    lt_0007 type tyt_0007,
    lt_2001 type tyt_2001,
    lt_2002 type tyt_2002,
    lt_2003 type tyt_2003.
  data:
    l_abwtg type p2001-abwtg,
    l_stdaz type p2001-stdaz.
  data:
    lt_abs_quota   type table of hrf_time_quota_au,
    lt_abs_quota_s type table of hrf_time_quota_au,
    l_ktart        type p2006-ktart,
    l_subrc        type sy-subrc.


  if is_input-begda > is_input-endda
    or is_input-begda = is_input-endda and is_input-beguz > is_input-enduz.
    rs_output-msgtype = 'E'.
    rs_output-msgtext = '结束时间要大于开始时间'.
    return.
  endif.

  types: begin of ty_period,
           begda type pa2001-begda,
           endda type pa2001-endda,
           beguz type pa2001-beguz,
           enduz type pa2001-enduz,
         end of ty_period.
  data: lt_period type table of ty_period,
        ls_period type ty_period.
  "第一天
  ls_period-begda = is_input-begda.
  ls_period-beguz = is_input-beguz.
  ls_period-endda = is_input-begda.
  if ls_period-endda = is_input-endda.
    ls_period-enduz = is_input-enduz.
  else.
    ls_period-enduz = '240000'.
  endif.
  if ls_period-begda = ls_period-endda and
     ls_period-beguz = ls_period-enduz.
    "特殊情况，需排除，因为若开始时间与结束时间相同则标准函数计算时默认从开始时间到下班时间
  else.
    append ls_period to lt_period.
  endif.

  "中间天
  ls_period-begda = ls_period-begda + 1.
  if ls_period-begda < is_input-endda.
    ls_period-beguz = '000000'.
    ls_period-endda = is_input-endda.
    ls_period-endda = ls_period-endda - 1.
    if ls_period-begda <= is_input-endda.
      ls_period-enduz = '240000'.
      append ls_period to lt_period.
    endif.
  endif.

  "最后一天
  if is_input-begda < is_input-endda.
    ls_period-begda = is_input-endda.
    ls_period-beguz = '000000'.
    ls_period-endda = is_input-endda.
    ls_period-enduz = is_input-enduz.
    if ls_period-enduz <> '000000'.
      append ls_period to lt_period.
    endif.
  endif.


  data:
    lt_0000          type table of p0000,
    lt_0002          type table of p0002,
    lt_times_per_day type table of ptm_times_per_day,
    l_vtken          type p2001-vtken.
  loop at lt_period into ls_period.
    clear: lt_0001[],lt_0007[],lt_2002[],lt_2003[],lt_0000[],lt_0002[],lt_times_per_day[],l_abwtg,l_stdaz.
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = conv pernr_d( is_input-pernr )
        infty           = '0001'
        begda           = ls_period-begda
        endda           = ls_period-endda
      importing
        subrc           = l_subrc
      tables
        infty_tab       = lt_0001
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.
    if lt_0001 is initial.
      rs_output-msgtype = 'E'.
      rs_output-msgtext = '人员编号不存在'.
      continue.
    endif.

*检查0007计划时间
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = conv pernr_d( is_input-pernr )
        infty           = '0007'
        begda           = ls_period-begda
        endda           = ls_period-endda
      importing
        subrc           = l_subrc
      tables
        infty_tab       = lt_0007
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.
    if lt_0007 is initial.
      rs_output-msgtype = 'E'.
      rs_output-msgtext = '计划工作时间不存在'.
      continue.
    endif.

*获取出勤
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = conv pernr_d( is_input-pernr )
        infty           = '2002'
        begda           = ls_period-begda
        endda           = ls_period-endda
      importing
        subrc           = l_subrc
      tables
        infty_tab       = lt_2002
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.

*替代
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = conv pernr_d( is_input-pernr )
        infty           = '2003'
        begda           = ls_period-begda
        endda           = ls_period-endda
      importing
        subrc           = l_subrc
      tables
        infty_tab       = lt_2003
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.


*计算缺勤时数
    call function 'HR_ABS_ATT_TIMES_AT_ENTRY'
      exporting
        pernr             = conv pernr_d( is_input-pernr )
        awart             = conv awart( is_input-awart )
        begda             = ls_period-begda
        endda             = ls_period-endda
        use_variant       = 'X'
      importing
        abwtg             = l_abwtg
      tables
        m0000             = lt_0000
        m0001             = lt_0001
        m0002             = lt_0002
        m0007             = lt_0007
        m2001             = lt_2001
        m2002             = lt_2002
        m2003             = lt_2003
        times_per_day     = lt_times_per_day
      changing
        beguz             = ls_period-beguz
        enduz             = ls_period-enduz
        vtken             = l_vtken
        stdaz             = l_stdaz
      exceptions
        it0001_missing    = 1
        customizing_error = 2
        error_occurred    = 3
        end_before_begin  = 4
        others            = 5.
    if sy-subrc <> 0.
      rs_output-msgtype = 'E'.
      rs_output-msgtext = '计算实际缺勤时间时错误!'.
      continue.
    endif.
    rs_output-abwtg += l_abwtg.
    rs_output-stdaz += l_stdaz.
  endloop.

  if lt_period[] is initial.
    rs_output-abwtg = 0.
    rs_output-stdaz = 0.
  endif.
  condense: rs_output-abwtg,rs_output-stdaz.
endmethod.
```

<!-- tab:其他常用函数 -->

| 函数                          | 描述                     |
| ----------------------------- | ------------------------ |
| `HR_TIME_RESULTS_IN_INTERVAL` | 读取考勤评估记录(常用)   |
| `HR_READ_TIMEDATA_PSP`        | 读取员工每日计划工作时间 |
| `HR_HK_DIFF_BT_2_DATES`       | 计算两个日期的差别       |
| `MONTH_NAMES_GET`             | 月份名称获取             |
| `HOLIDAY_CALENDAR_GET`        | 读取公共假日列表         |
| `LAST_DAY_OF_MONTHS`          | 计算指定月份的最后一天   |

<!-- tabs:end -->

### 正向考勤(2011，时间事件)

- 正向考勤的打卡数据存储表为`teven`, 表中的数据不会被删除，而是把`STOKZ`打上标识X。

  ```abap
  select * into table @data(lt_teven) from teven where
    pernr in @s_pernr and ldate in @s_date and stokz <> 'X'.
  ```

- 正向考勤数据回写

  ```abap
  data: lt_timeevent type table of cc1_timeevent.
  
  call function 'HR_CC1_TIMEEVENT_INSERT'
  	tables
  		timeevent          = lt_timeevent
    exceptions
      number_range_error = 1                              
      others             = 2.
  
  check sy-subrc = 0.
  
  data: lv_posted_timeevents   like sy-dbcnt,
        lv_faulty_timeevents   like sy-dbcnt,
        lv_locked_timeevents   like sy-dbcnt,
        lv_total_timeevents    like sy-dbcnt,
        lv_uploaded_timeevents like sy-dbcnt,
        lv_skipped_timeevents  like sy-dbcnt.
        
   call function 'HR_CC1_TIMEEVENT_POST'
      exporting
        update             = 'X'
        update_uname_aedtm = 'X'
      importing
        posted_timeevents  = lv_posted_timeevents
        faulty_timeevents  = lv_faulty_timeevents
        locked_timeevents  = lv_locked_timeevents
        skipped_timeevents = lv_skipped_timeevents   
      exceptions
        others             = 0.
  ```
  
  > [!Note]
  >
  > 表`cc1_timeevent`常用字段如下：
  >
  > | 字段名      | 字段描述                                       |
  > | ----------- | ---------------------------------------------- |
  > | `pernr`     | 人员编号                                       |
  > | `ldate`     | 打卡日期                                       |
  > | `ltime`     | 打卡时间                                       |
  > | `terid`     | 终端标识，长度4，无值域                        |
  > | `satza`     | 刷卡类型，常用值`P10`-上班; `P20`下班          |
  > | `erdat`     | 写入日期                                       |
  > | `ertim`     | 写入时间                                       |
  > | `indeu`     | 操作标识，常用值：`I`-插入; `D`-删除; `U`-更改 |
  > | `dallf`     | 刷卡时间，常用值：`-`-前一天; `+`-当天         |
  > | `abwgr`     | 出/缺勤原因，值表`T705A`                       |
  > | `origf`     | 刷卡数据来源（使用方法未知）                   |
  > | `pdc_usrup` | 备注字段                                       |
  >

- 正向考勤数据删除

  ```abap
  data: lv_srtfd like pcl1-srtfd,
        lv_relid like pcl1-relid value 'B1'.
  
  loop at gt_timeevent into gs_timeevent.
    lv_srtfd = gs_timeevent-pernr.
    select count( * ) from pcl1 where relid eq lv_relid and srtfd eq lv_srtfd.
    if sy-subrc eq 0.
      delete from pcl1 where relid eq lv_relid and srtfd eq lv_srtfd.
    endif.
    delete from teven  where pernr eq gs_timeevent-pernr
                         and ldate eq gs_timeevent-ldate
                         and ltime eq gs_timeevent-ltime
                         and satza eq gs_timeevent-satza
                         and abwgr eq gs_timeevent-abwgr
                         and dallf eq gs_timeevent-dallf
                         and stokz ne 'X'.
    delete from cc1tev where pernr eq gs_timeevent-pernr.
    commit work.
  endloop.
  ```

  



