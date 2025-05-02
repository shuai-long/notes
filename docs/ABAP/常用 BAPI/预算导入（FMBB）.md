## 预算导入（FMBB）

```abap
 "FMBBC/FMEDD-------------预算工作台-创建/显示凭证：下达预算，同时产生凭证，基金凭证
 "FMBB--------------预算工作台。
 "下达预算同时也可以批量创建凭证：BAPI_0050_CREATE.
 
 DATA: ls_fmarea	      TYPE bapi_0050_fields-fm_area,  "三个导出参数
        ls_documentyear   TYPE bapi_0050_fields-doc_year,
        ls_documentnumber TYPE bapi_0050_fields-document,
		ls_header_data    TYPE bapi_0050_header,          "抬头入参
        lt_item_data      TYPE STANDARD TABLE OF bapi_0050_item,"行项目入参
        ls_item_data      LIKE LINE OF it_item_data,
        lt_period_data    TYPE STANDARD TABLE OF bapi_0050_period,
        ls_period_data    LIKE LINE OF lt_period_data,
        it_return	      TYPE STANDARD TABLE OF bapiret2,"返回结果
        wa_return	      LIKE LINE OF it_return.
        
"抬头信息
ls_header_data-fm_area = 'CCTC'."财务管理范围
ls_header_data-version = '000'."预算版本
ls_header_data-docdate = sy-datum."凭证日期
*ls_header_data-pstng_date = sy-datum."凭证中的过账日期
ls_header_data-doctype = 'CCTC'."预算分录凭证类型（CCTC）
ls_header_data-docstate = '1'."一个预算分录凭证状态
ls_header_data-process = 'ENTR'."预算过程  ENTR 输入  RETN 返回  SUPL补充
*ls_header_data-external_number = ."预算分录凭证的外部号

"行项目信息
CLEAR ls_item_data.
ls_item_data-item_num   = gt_data_show-buku_cocnr. "预算分录凭证行
ls_item_data-fisc_year   = gt_data_show-gjahr. "会计年度
ls_item_data-budcat   = '9F'. "预算类别
ls_item_data-budtype   = 'CCTC'. "预算类型
ls_item_data-funds_ctr   = gt_data_show-fundsctr. "基金中心
ls_item_data-cmmt_item   = gt_data_show-cmmtitem. "承诺项目
*ls_item_data-func_area   = gt_data_show-buku_cocnr. "基金计划
*ls_item_data-grant_nbr   = gt_data_show-buku_cocnr. "同意
*ls_item_data-trans_curr   = gt_data_show-buku_cocnr. "交易货币
ls_item_data-trans_curr_iso   = 'CNY'. "ISO 货币码
ls_item_data-total_amount   = gt_data_show-tval01. "BAPI 接口中的货币金额
ls_item_data-distkey   = '1'. " 分配代码（0）
ls_item_data-item_text   = gt_data_show-text50. "项目文本
ls_item_data-valtype   = 'B1'. "BCS值类型
*ls_item_data-cash_year   = gt_data_show-buku_cocnr. "现金有效年份
*ls_item_data-budget_period   =  ''. "预算期间
*ls_item_data-userdim   = gt_data_show-buku_cocnr. "FM 实际和承诺数据的客户字段
APPEND wa_item_data TO lt_item_data.

CALL FUNCTION 'BAPI_0050_CREATE'
    EXPORTING
        language       = sy-langu
        header_data    = ls_header_data
        testrun        = space "是否需要测试运行
        update_mode    = '1'
    IMPORTING
        documentnumber = ls_documentnumber
        fmarea         = ls_fmarea
        documentyear   = ls_documentyear
    TABLES
        item_data      = lt_item_data
        *period_data    = lt_period_data
        return         = it_return.
        
"FMZ1/FMZ2/FMZ3----创建/更改/显示基金承诺（产生基金凭证），创建基金凭证的同时，会占用掉预算创建基金承诺（称预算占用凭证），
"也可以使用相应的FUNCTION：'FMFR_CREATE_FROM_DATA'。例：      

"释放预算，即从占用转为消耗掉预算也有相应的 FUNCTION:'FMFR_CHANGE_COMPLETION_FLAG' 例:

```

