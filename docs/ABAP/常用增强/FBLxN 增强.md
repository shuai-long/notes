# FBL*N 增强 #

FBL1N / FBL3N / FBL5N / IDCNAR / IDCNAP 增加**利润中心**权限检查, 使用 BADI `FI_ITEMS_CH_DATA` 实现.

1. 根据用户名找角色:

   根据 `AGR_USERS-UNAME = SY-UANME AND FROM_DAT LE @SY-DATUM AND TO_DAT GE @SY-DATUM`, 查找到所有的条目

2. 检查利润中心的权限, 权限对象：`K_PCA`，

   CO-OM 授权检查 CO_ACTION：如果为 * 或含有 0003 ，则表示有查询报表的权限；

   成本要素 `KSTAR`：表示能够查看的报表科目范围，如果为 *，则表示所有；

   字段名：`RESPAREA`，后十位为需要检查的利润中心编码：

示例代码如下:

```abap
  METHOD if_ex_fi_items_ch_data~change_items.
    DATA:lv_len      TYPE int4, "字符串长度
         lv_len_need TYPE  int4. "从字符串多少位截取
         
    DATA:lv_low(10),
         lv_high(10).
         
    DATA:lt_prctr type ranges OF cepc-prctr,
    	 ls_prctr like line OF lt_prctr.

    "根据当前用户获取角色
    SELECT *
      INTO TABLE @DATA(lt_agr_users)
      FROM agr_users
      WHERE uname = @sy-uname
      AND from_dat LE @sy-datum
      AND to_dat GE @sy-datum.

    IF lt_agr_users[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(lt_agr_1251)
        FROM agr_1251
        FOR ALL ENTRIES IN @lt_agr_users
        WHERE agr_name = @lt_agr_users-agr_name
        AND object = 'K_PCA'
        AND field = 'RESPAREA'
        AND deleted = ''.

      ls_prctr+0(3) = 'EEQ'.
      ls_prctr-low = ''.
      APPEND ls_prctr TO lt_prctr.

      LOOP AT lt_agr_1251 INTO DATA(ls_agr_1251).

        CLEAR ls_prctr.
        IF ls_agr_1251-low IS NOT INITIAL AND ls_agr_1251-low EQ '*'.
          ls_prctr+0(3) = 'EBT'.
          ls_prctr-low = '0000000000'.
          ls_prctr-high = '9999999999'.
          APPEND ls_prctr TO lt_prctr.
        ELSE.
          IF ls_agr_1251-low IS NOT INITIAL AND ls_agr_1251-high IS INITIAL.

            lv_len = strlen( ls_agr_1251-low ).
            lv_len_need = lv_len - 10.
            lv_low = ls_agr_1251-low+lv_len_need(10).

            ls_prctr+0(3) = 'EEQ'.
            ls_prctr-low = lv_low.
            APPEND ls_prctr TO lt_prctr.
          ELSEIF ls_agr_1251-low IS NOT INITIAL AND ls_agr_1251-high IS NOT INITIAL.

            lv_len = strlen( ls_agr_1251-low ).
            lv_len_need = lv_len - 10.
            lv_low = ls_agr_1251-low+lv_len_need(10).

            lv_len = strlen( ls_agr_1251-high ).
            lv_len_need = lv_len - 10.
            lv_high = ls_agr_1251-high+lv_len_need(10).

            ls_prctr+0(3) = 'EBT'.
            ls_prctr-low = lv_low.
            ls_prctr-high = lv_high.
            APPEND ls_prctr TO lt_prctr.
          ENDIF.
        ENDIF.
      ENDLOOP.
      
      DELETE ct_items WHERE prctr IN lt_prctr.
    ENDIF.

  ENDMETHOD.
```





