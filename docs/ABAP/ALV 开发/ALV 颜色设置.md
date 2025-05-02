[TOC]

# ALV颜色设置 #

<img src="https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220901165722040.png" alt="ALV颜色"  />

## 行颜色设置 ##

```abap
"在构成 alv 数据的内表中，添加一个char型4位的字段（color），用来记录颜色代码
TYPES: BEGIN OF ty_alv,
         color TYPE char4, "控制行颜色
       END OF ty_alv.
ls_layout-info_fname = 'COLOR'.  "行颜色代码的字段
```



## 列颜色设置 ##

```abap
"在构成 alv 字段的 fieldcat 内表中有一个字段是 emphasize，将一个char型4位的颜色代码分配到 fieldcat 内表这个字段即可。
DEFINE m_fcat.
  CLEAR ls_fcat.
 ls_fcat-fieldname = &1.
 ls_fcat-coltext = &2.
 ls_fcat-emphasize = &3 . "列颜色
 APPEND ls_fcat TO ct_fcat.
END-OF-DEFINITION.
m_fcat: 'COLOR' '颜色' 'C710'.
```

## 单元格颜色设置 ##

```ABAP
"在构成 alv 数据的内表中，添加一个内表 cellcolor，用来记录颜色代码，需要设置颜色的字段名
TYPES: BEGIN OF ty_alv,
         bm        TYPE char4,
         cellcolor TYPE lvc_t_scol, "单元格颜色
       END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

"layout 结构中coltab_fieldname的值指定为cellcolor
gs_layout-ctab_fname = 'CELLCOLOR'. " 单元格颜色字段


*设置样式
FORM frm_layout.

  DATA:ls_cellcolor TYPE lvc_s_scol.

  "color 是一个结构：col      alv 控制: 颜色
  "coL      alv 控制: 颜色代码
  "int      alv 控制: 强化    1/0
  "inv      alv 控制: 相反    1/0    设置颜色是前景，或者是背景

  LOOP AT gt_alv INTO gs_alv.
    IF gs_alv-bm = '销售部门'.          "行: 部门为销售部门的行
      ls_cellcolor-fname = 'BM'.      "列: 采购部门为‘销售部门’的行的‘BM’字段颜色为 黄色
      ls_cellcolor-color-col = 3.     "ALV 控制：颜色代码
      ls_cellcolor-color-int = 1.     "ALV 控制：强化  1/0
      ls_cellcolor-color-inv = 0.     "ALV 控制：相反  1/0  设置颜色是前景，或者是背景
      APPEND ls_cellcolor TO gs_alv-cellcolor.
      MODIFY gt_alv FROM gs_alv.
    ENDIF.
  ENDLOOP.

ENDFORM.
```

