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

  field-symbols: <fs_goitem> type goitem.

  break-point.
  assign ('(SAPLMIGO)GOITEM') to <fs_goitem>.
  if <fs_goitem> is assigned.

    data(lt_maplb) = t_maplb[].
    data(lt_plkob) = t_plkob[].

    sort lt_maplb by plnnr.
    delete adjacent duplicates from lt_maplb comparing plnnr.
    sort lt_plkob by plnnr.
    delete adjacent duplicates from lt_plkob comparing plnnr.

    if lines( lt_maplb ) gt 1 or lines( lt_plkob ) gt 1.

      select single matkl into @data(lv_matkl) from mara
        where matnr eq @<fs_goitem>-matnr.

      select single case when bu_group in ( 'Z001', 'Z003' ) then '1'
                         when bu_group in ( 'Z002', 'Z004' ) then '2'
       end as bu_group from but000 where partner eq @<fs_goitem>-lifnr
       into @data(lv_bu_group) .

      select single case when bsart in ( 'NF','NH','NM','NZ' ) then '2' else '1'
        end as bsart from ekko where ebeln eq @<fs_goitem>-ebeln
        into @data(lv_bsart).

      select single plnnr from zmmt_ip
        where matkl eq @lv_matkl and zipcc eq @lv_bu_group and zbt eq @lv_bsart
        into @data(lv_plnnr).

      if lv_plnnr is not initial.
        delete t_maplb where plnnr ne lv_plnnr.
        delete t_plkob where plnnr ne lv_plnnr.
      endif.

    endif.
  endif.
```

