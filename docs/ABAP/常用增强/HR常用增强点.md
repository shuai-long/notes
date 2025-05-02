[TOC]

# HR 常用增强点 #

## 工资核算驱动程序 选择屏幕值校验 ##

中国对应的主程序: `HCNCALC0`  中 包含程序 `RPCHRT09`为程序开始执行的地方:

![image-20220901173211999](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220901173211999.png)

```abap
 PERFORM frm_check_flow IN PROGRAM ZHR_HCNCALC0_CUST_VALIDATION1 IF FOUND   TABLES pnpabkrs  "工资范围
                                                                            USING  pn-pabrj  "年
                                                                                   pn-pabrp  "月
                                                                                   payty     "工资核算类型
                                                                                   payid     "工资核算标识
                                                                                   bondt     "非周期工资发放付款日期
                                                                                   tst_on.   "是否测试运行
```

![image-20220901173749637](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220901173749637.png)

## PA40 选择屏幕值校验 ##

```abap
"该FORM在程序 FP50GE00  
FORM rp_infotyp USING rpi_pernr rpi_actio
                      rpi_infty rpi_subty rpi_objps
                      rpi_begda rpi_endda
                      rpi_rcode LIKE pspar-rcode.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Form RP_INFOTYP, Start                                                                                                                            A
*$*$-Start: (1) Program: SAPFP50G include bound-------------------------------------------------$*$*
ENHANCEMENT 1  ZHR_HCNCALC0_CUST_VALIDATION1.    "active version
*
    IF sy-tcode = 'PA40' AND rpi_infty = '0000' AND rpi_subty = 'A1'.
      BREAK zhangsl.
      DATA: lt_value  TYPE TABLE OF sval,
            ls_value  TYPE sval,
            lv_rtn_cd,
            lv_icnum  TYPE p0185-icnum.

      CLEAR: ls_value ,lt_value.
      ls_value-tabname = 'PA0185'.
      ls_value-fieldname = 'ICNUM'.
      APPEND ls_value TO lt_value.

      CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = '请输入身份证号'
      IMPORTING
        returncode  = lv_rtn_cd
      TABLES
        fields      = lt_value.

      IF lv_rtn_cd IS INITIAL.
        READ TABLE lt_value INTO ls_value INDEX 1.
        IF SY-SUBRC = 0 AND ls_value-value IS NOT INITIAL.
          CLEAR lv_icnum.
          SELECT SINGLE icnum INTO lv_icnum FROM pa0185 WHERE icnum = ls_value-value.
          IF lv_icnum is NOT INITIAL.
            MESSAGE '该人员信息已存在，请勿重复录入' TYPE 'E'.
          ENDIF.
        ELSE.
          MESSAGE '请输入身份证号' TYPE 'E'.
        ENDIF.
      ELSE.
          MESSAGE '用户已取消输入' TYPE 'E'.
      ENDIF.
   ENDIF.
ENDENHANCEMENT.
```

##  HRP1000 & HRP1001 写入增强

对于 HRP1000 与 HRP1001 数据的写入，可以在数据写入前进行修改。程序：`SAPLRHAP` 包含文件：`LRHAPF1R` 子例程：`update_database`

![image-20231114105610264](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20231114105610264.png)

![image-20231114105843013](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20231114105843013.png)

## 信息类型写入检验增强

CMOD 创建项目实现 PBAS0001，包含 PBO组件（EXIT_SAPFP50M_001）, PAI 组件(EXIT_SAPFP50M_002)
