## 附录



### 常用表


#### Others

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


#### 0000

| 字段    | 描述     | 值表    | 文本表-字段     | 备注                                                         |
| ------- | -------- | ------- | --------------- | ------------------------------------------------------------ |
| `massn` | 操作类型 | `T529A` | `T529T`-`MNTXT` | `t588b`(pa40人事事件：`mntyp = 'M' and menue = '01' and userg = '28'`) |
| `massg` | 操作原因 | `T530`  | `T530T`-`MGTXT` | `t530_delimit`表存储操作原因的有效期                         |

#### 0001

| 字段    | 描述       | 值表    | 文本表-字段     | 备注                              |
| ------- | ---------- | ------- | --------------- | --------------------------------- |
| `persk` | 员工组     | `T501`  | `T501T`-`PTEXT` |                                   |
| `persg` | 员工子组   | `T503`  | `T503T`-`PTEXT` | `t503z`表存储员工子组和国家的关系 |
| `werks` | 人事范围   | `T500P` | `T500P`-`NAME1` |                                   |
| `btrtl` | 人事子范围 | `T001P` | `T001P`-`BTEXT` |                                   |
| `abkrs` | 工资范围   | `T549A` | `T549T`-`ATEXT` |                                   |





#### 其他
- 同步员工供应商程序：`/SHCM/RH_SYNC_BUPA_EMPL_SINGLE`
- 同步员工供应商日志：事务码为`SLG1`，对象类型为`SHCM_EE_INTEGRATION`
