# 1. QA32 检验说明带出

MIGO 收获，过账后 QA32的检验说明不能正常带出（如果物料的检验计划只有一个则能正常带出，如果有多个则不能）

MIGO 界面：![image-20241014183700759](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20241014183700759.png)



QA32 正常带出界面：![image-20241014183820573](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20241014183820573.png)



QA32 不能正常带出界面：![image-20241014183936376](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20241014183936376.png)



SMOD 查找 QPAP0002，并实现 EXIT_SAPLQPAP_002 函数![image-20241014184215884](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20241014184215884.png)



代码如下：

```abap
*&---------------------------------------------------------------------*
*& 包含               ZXQPAU04
*&---------------------------------------------------------------------*

  field-symbols: <fs_goitem> type goitem,
                 <fs_ekko>   type ekko.
  data: lv_ebeln type ebeln.

  data(lt_maplb) = t_maplb[].
  data(lt_plkob) = t_plkob[].

  sort lt_maplb by plnnr.
  delete adjacent duplicates from lt_maplb comparing plnnr.
  sort lt_plkob by plnnr.
  delete adjacent duplicates from lt_plkob comparing plnnr.
  if lines( lt_maplb ) gt 1 or lines( lt_plkob ) gt 1.

    select single matkl into @data(lv_matkl) from mara
      where matnr eq @i_rcpse-matnr.
      
    assign ('(SAPLMIGO)GOITEM') to <fs_goitem>.
    if <fs_goitem> is assigned.
      lv_ebeln = <fs_goitem>-ebeln.
    endif.

    assign ('(SAPMV50A)EKKO') to <fs_ekko>.
    if <fs_ekko> is assigned.
      lv_ebeln = <fs_ekko>-ebeln.
    endif.

    if lv_ebeln is not initial.
      select single case when bsart = 'NB' or bsart = 'NPO' or bsart = 'NG03' or bsart = 'COP' then '1'
                         when bsart = 'CER' then '2'
        end as bsart from ekko where ebeln eq @lv_ebeln
        into @data(lv_bsart).
      select single admoi
        from ekpo
        where ebeln = @lv_ebeln
          and matnr = @i_rcpse-matnr
      into @data(lv_old_new).

      select single plnnr
        from zmmt_ip
        where matkl eq @lv_matkl
          and zbt eq @lv_bsart
          and new_old_part = @lv_old_new
        into @data(lv_plnnr).

      if lv_plnnr is not initial.
        delete t_maplb where plnnr ne lv_plnnr.
        delete t_plkob where plnnr ne lv_plnnr.
      endif.
    endif.

  endif.
```

