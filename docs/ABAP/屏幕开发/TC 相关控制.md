[TOC]

# TC 相关控制 #

## 设置某列不可输入 ##

```abap
"控制不可输入的代码必须写在PBO模块，可以写在 LOOP...ENDLOOP 下边
MODULE z_lists_not_enter."设置字段不可输入
MODULE z_lists_not_enter OUTPUT.
	LOOP AT SCREEN.
		IF	SCREEN-NAME = '表名-字段名'.
			SCREEN-INPUT = 0.
		ENDIF.
		MODIFY SCREEN.
	ENDLOOP.
ENDMODULE.

```

## 根据值带出描述 ##

该模块需要写在 PAI 事件的 LOOP 循环中,当用户更改某列的值触发

```abap
"MODULE 要包含在 CHAIN...ENDCHAIN 中
MODULE z_get_text.
MODULE z_get_text INPUT.
	"只需将取到的值给到屏幕即可(表名-字段名)
ENDMODULE.
```

## 固定列 ##

通过设置 Table Control 自带的属性，定义最左边的某些列不可滚动。在Screen Layout中，双击Table Control的右上角，弹出“表控制”属性，即可设置。如下图所示：

<img src="https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220905152805951.png" alt="image-20220905152805951"  />

## 隐藏列 ##

通过CODING修改tabctrl-cols下的某字段可见长度。该 MODULE 需要写在 PBO 事件中:

```abap
"TABEL CONTROL-COLS 可通过1.3双击的地方查看名称
DATA: ls_col LIKE LINE OF tctrl_zfit_003-cols.
LOOP AT tctrl_zfit_003-cols INTO ls_col WHERE screen-name = 'ZFIT_003-LANGU'
                                            OR screen-name = 'ZFIT_003-FRFAREA'
                                            OR screen-name = 'ZFIT_003-TRFAREA'
                                            OR screen-name = 'ZFIT_003-RSTGR'
                                            OR screen-name = 'ZFIT_003-KURST'
                                            OR screen-name = 'ZFIT_003-SHKZGS'
                                            OR screen-name = 'ZFIT_003-SHKZGH'
                                            OR screen-name = 'ZFIT_003-SHKZGH'.

    ls_col-vislength = 0.

  MODIFY tctrl_zfit_003-cols FROM ls_col.
ENDLOOP.
 
```

## 获取双击行

```abap
get cursor line lv_row field lv_field. "获取TC的双击行和字段
data(lv_line) = tc_fields-top_line + lv_row - 1. "获取真实的内表行
read table gt_field_information into data(ls_field_information) index lv_line."读取数据
```

## 更新行

```abap
if tc_recipient-current_line <= lines( gt_email ).
  modify gt_email from gs_email index tc_recipient-current_line.
else.
  append gs_email to gt_email .
  clear gs_email.
endif.
```

