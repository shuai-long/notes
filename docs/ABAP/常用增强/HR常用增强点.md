## 工资核算选择屏幕值校验 ##

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

##  IT1000/IT1001 写入增强

对于 HRP1000 与 HRP1001 数据的写入，可以在数据写入前进行修改。程序：`SAPLRHAP` 包含文件：`LRHAPF1R` 子例程：`update_database`

![image-20231114105610264](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20231114105610264.png)

![image-20231114105843013](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20231114105843013.png)

## 薪资凭证过账行项目更改

HR 标准的薪资过账程序，对生成的凭证行项目进行更改：函数 `AC_DOCUMENT_CREATE` 首行添加隐式增强:
![image-20250804094102809](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250804094102809.png)

以下需求按科目汇总行项目:

![image-20250804094218935](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250804094218935.png)

```abap
form frm_collect_item tables t_acchd structure acchd
                             t_accit structure accit
                             t_acccr structure acccr.

  data: lt_accit type standard table of accit,
        lt_acccr type standard table of acccr.

  data: lv_error type c,
        lv_clear type c.

  data: begin of ls_posnr,
          bschl type accit-bschl,
          ktosl type accit-ktosl,
          blart type accit-blart,
          hkont type accit-hkont,
          kostl type accit-kostl,
          posnr type accit-posnr,
        end of ls_posnr.
  data: lt_posnr like hashed table of ls_posnr with unique key bschl ktosl blart kostl hkont.


  "判断是否是人力薪资凭证
  if line_exists( t_acchd[ awtyp = 'HRPAY' ] ).
    loop at t_accit.

      "凭证类型不为 G3 则退出不处理
      if t_accit-blart ne 'G3'.
        lv_error = 'X'.
        exit.
      endif.

      "若初始凭证行项目存在错误则退出不处理
      read table t_acccr with key awtyp = t_accit-awtyp awref = t_accit-awref posnr = t_accit-posnr.
      if sy-subrc ne 0.
        lv_error = 'X'.
        exit.
      endif.

      "HRC，G3，221101* 的行项目，如果是则清空成本中心，并汇总金额，不是则直接添加。
      clear lv_clear.
      if t_accit-ktosl eq 'HRC' and t_accit-blart eq 'G3' and ( t_accit-hkont+0(6) eq '221101' or t_accit-hkont+0(6) eq '221103'  ).
        lv_clear = 'X'.
        clear t_accit-kostl.
      endif.

      "记录当前的行项目号
      read table lt_posnr into ls_posnr with key bschl = t_accit-bschl ktosl = t_accit-ktosl blart = t_accit-blart kostl = t_accit-kostl hkont = t_accit-hkont.
      if sy-subrc ne 0.
        clear ls_posnr.
        move-corresponding t_accit to ls_posnr.
        insert ls_posnr into table lt_posnr.
      endif.

      if lv_clear is initial.
        append t_accit to lt_accit.
        append t_acccr to lt_acccr.
      else.

        read table lt_accit into data(ls_accit) with key bschl = t_accit-bschl ktosl = t_accit-ktosl blart = t_accit-blart hkont = t_accit-hkont.
        if sy-subrc ne 0.
          append t_accit to lt_accit.
          append t_acccr to lt_acccr.
        else.
          read table lt_acccr assigning field-symbol(<fs_acccr>) with key awtyp = t_accit-awtyp awref = t_accit-awref posnr = ls_posnr-posnr.
          if sy-subrc eq 0.
            add t_acccr-wrbtr to <fs_acccr>-wrbtr.
          endif.
        endif.

      endif.

    endloop.

    loop at lt_accit assigning field-symbol(<fs_accit>).
      <fs_accit>-posnr = sy-tabix.
    endloop.

    loop at lt_acccr assigning <fs_acccr>.
      <fs_acccr>-posnr = sy-tabix.
    endloop.

    check lv_error is initial.
    t_accit[] = lt_accit[].
    t_acccr[] = lt_acccr[].

  endif.

endform.
```

## 信息类型增强

- CMOD 创建项目实现 PBAS0001，包含 PBO组件（EXIT_SAPFP50M_001）, PAI 组件(EXIT_SAPFP50M_002)

- SE18 实现BADI。人事： `HRPAD00INFTY`， 组织：``HRBAS00INFTY`

## 缺勤定额生成增强

缺勤定额生成事务码 `PT_QTA00` 或使用 `PT60`

CMOD 创建项目实现 HRPTIM03，实现组件 `EXIT_SAPLHRLV_011` 或 `EXIT_SAPLHRLV_012`





