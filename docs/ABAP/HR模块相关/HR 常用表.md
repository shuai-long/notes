# 配置视图 V_511_B 的数据存储表

如下图所示，该界面数据存储在 T511 中。

```abap
"ABTYZ 对应员工子组分组。          V_503_ALL  查看员工组子组分组
"WKTYZ 对应人事子范围分组。        V_001P_ALL  查看人事子范围分组
```

![V_511_B配置视图](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611174250139.png)

# 判断工资项是否是累加项

```abap
data: lv_kumul type char255.

SELECT SINGLE kumul INTO @DATA(lv_kumul_raw) FROM t512w
  WHERE molga eq '28' AND lgart eq @lv_lgart AND begda LE @sy-datum AND endda GE @sy-datum.

write lv_kumul_raw TO lv_kumul.
IF lv_kumul+22(1) EQ '4'.
  	"第23位为4则为累计项
ENDIF.
```

# 判断工资项是否是扣减项

T511-OPKEN: ' '=付款;'A'=扣件

```abap
select count(*) from t511 where molag = '28' and lgart eq lv_lgart and endda = '99991231'.
```

# 获取信息类型可用工资项

T512Z

```abap
select * into table @data(lt_t512z) from t512z where infty eq lv_infty and molga eq '28'.
```

# 获取信息类型可用子类型

T591A

```abap
select * into table @data(lt_t591a) from t591a where infty eq lv_infty.
```

