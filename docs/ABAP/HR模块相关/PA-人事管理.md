## PA-人事管理

- **数据表特性**

  - 主键区域：一般引用结构`PAKEY`。
  - 控制区域：一般引用结构`PSHD1`。
  - 信息类型主数据：PS+信息类型编号。
  - 可增强区域：CI_P+信息类型编号，所有带CI_PXXXX的信息类型原则上都可以增强。

- **常用函数**

  <!-- tabs:start -->

  <!-- tab:员工入职 -->

  - 将信息类型转换为PNNNN

    ```abap
    methods append_data_to_pnnnn
      importing
        is_primary_record   type any
    	  is_secondary_record type any option
    	changing
    	  ct_prelp            type prelp_tab.
    ```
  
    ```abap
    method append_data_to_pnnnn.
      check is_primary_record is not initial.
      
      if is_secondary_record is not supplied.
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp(
          exporting
            pnnnn = is_primary_record
          importing
            prelp = data(ls_prelp) ).
      else.
        cl_hr_pnnnn_type_cast=>view_to_prelp(
          exporting
            primary_record   = is_primary_record
            secondary_record = is_secondary_record
          importing
            prelp            = ls_prelp ).
      endif.
    
      append ls_prelp to ct_prelp.
    endmethod.
    ```
  
  - 员工入职
  
    ```abap
    types: begin of ty_data_in,
             pernr       type pernr_d,
             begda       type begda,
             massn       type massn,
             massg       type massg,
             plans       type plans,
             nocommit    type flag,
             pnnnn_tab   type prelp_tab,
          end of ty_data_in.
          
    methods hire_employee
      importing
        !is_data type ty_data_in.
    ```
    
    ```abap
    method hire_employee.
    
      data: lt_return_tab        type hrpad_return_tab,
            lt_bapipakey_tab     type hrpad_bapipakey_tab,
            lv_is_ok             type boole_d,
            lt_modified_keys_tab type hrpad_pskey_tab.
    
      call function 'HR_PAD_HIRE_EMPLOYEE'
        exporting
          employeenumber    = is_data-pernr
          hiringdate        = is_data-begda
          actiontype        = is_data-massn
          reasonforaction   = is_data-massg
          pnnnn_tab         = is_data-pnnnn_tab
          nocommit          = is_data-nocommit
        importing
          return_tab        = lt_return_tab
          bapipakey_tab     = lt_bapipakey_tab
          is_ok             = lv_is_ok
          modified_keys_tab = lt_modified_keys_tab.
    
      if lv_is_ok is initial.
        "Create failed
      endif.
    
    endmethod.
    ```
  
  <!-- tab:信息类型增删改操作 -->
  
  <!-- tabs:start -->
  
  <!-- tab:获取第二信息类型 -->

  ```abap
  class-methods get_secend_infty
    importing
              iv_infty         type infty
    returning value(rs_result) type t777d.
  ```
  
  ```abap
  method get_secend_infty.
  
    select single * into @data(ls_t582v) from t582v
      where molga eq '28' and infty eq @iv_infty.
  
    check ls_t582v is not initial.
  
    select * into table @data(lt_t582w) from t582w
      where vinft eq @ls_t582v-vinft.
  
    read table lt_t582w into data(ls_t582w) with key seqnr = '02'.
    if sy-subrc eq 0.
      select single * into @data(ls_t777d) from t777d
        where infty eq @ls_t582w-infty.
    endif.
  
    rs_result = ls_t777d.
  
  endmethod.
  ```
  
  <!-- tab:信息类型增删改查 -->
  
  ```abap
  methods call_bapi_to_write_data
    importing
              is_data          type any
              is_second_data   type any optional
              iv_commit        type c default 'X'
    exporting
              es_message       type string
    returning value(rv_result) type boole.
  ```
  
  ```abap
  method call_bapi_to_write_data.
  
    data: ls_hrkey    type pskey,
          lv_nocommit type c,
          ls_key      type bapipakey,
          ls_return   type bapireturn1.
  
    move-corresponding is_data to ls_hrkey.
  
    call function 'BAPI_EMPLOYEE_ENQUEUE'
      exporting
        number = ls_hrkey-pernr
      importing
        return = ls_return.
  
    if ls_return ca 'EAX'.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_message).
      return.
    endif.
  
    if iv_commit is initial.
      lv_nocommit = 'X'.
    endif.
    
    call function 'HR_INITIALIZE_BUFFER'.
  
    call function 'HR_PSBUFFER_INITIALIZE'.
  
    if is_second_data is not initial.
      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty            = ls_hrkey-infty
          number           = ls_hrkey-pernr
          subtype          = ls_hrkey-subty
          validityend      = ls_hrkey-endda
          validitybegin    = ls_hrkey-begda
          record           = is_data
          operation        = 'INS'
          nocommit         = lv_nocommit
          view_identifier  = '28'
          secondary_record = is_second_data
        importing
          return           = ls_return
          key              = ls_key.
    else.
      call function 'HR_INFOTYPE_OPERATION'
        exporting
          infty         = ls_hrkey-infty
          number        = ls_hrkey-pernr
          subtype       = ls_hrkey-subty
          validityend   = ls_hrkey-endda
          validitybegin = ls_hrkey-begda
          record        = is_data
          operation     = 'INS'
          nocommit      = lv_nocommit
        importing
          return        = ls_return
          key           = ls_key.
    endif.
  
    if ls_return ca 'EAX'.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_message.
      return.
    endif.
  
    call function 'BAPI_EMPLOYEE_DEQUEUE'
      exporting
        number = ls_hrkey-pernr.
        
    if iv_commit is not initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    endif.
  
    rv_result = abap_true.
  
  endmethod.
  ```
  
  <!-- tabs:end -->
  
  <!-- tab:成本分配 -->

  0014和0015中成本分配存储的表是:`ASSOB`和`ASSHR`. 其数据库视图是`ASSOB_HR`

  - 新增时获取成本分配号码

    ```abap
    data: lv_range    like inri-nrrangenr value '01',
          lv_object   like inri-object    value 'PD_SEQ_NR',
          lv_quantity like inri-quantity  value 1,
          lv_retcode  like inri-returncode,
          lv_number   like pdsnr-pdsnr.
    
    call function 'NUMBER_GET_NEXT'
      exporting
        object                  = lv_object
        nr_range_nr             = lv_range
        quantity                = lv_quantity
      importing
        number                  = lv_number
        returncode              = lv_retcode
      exceptions
        object_not_found        = 1
        interval_not_found      = 2
        number_range_not_intern = 3.
    ```
  
  - 更新成本分配
  
    ```abap
    data: lv_opera type syst_msgty value 'U', "U: 更改 I:插入 D:删除
          lv_pdsnr type pdsnr-pdsnr, "要操作的编号
          ls_pref  type pref. "操作的数据
    
    ls_pref = value #( pernr = '70023000'
                       infty = '0015'
                       subty = '3007'
                       begda = '20220124'
                       endda = '20220124'
                       bukrs = '1100'
                       kokrs = 'CCTC'
                       posnr = '00014503'
                       kostl = '' ).
    
    call function 'RP_PLANT_DATA_UPDATE_TABLES'
      exporting
        ipdsnr                         = lv_pdsnr
        ipref                          = ls_pref
        iopera                         = lv_opera
      exceptions
        insert_asshr_not_possible      = 1
        insert_coift_not_possible      = 2
        insert_assob_not_possible      = 3
        delete_asshr_not_possible      = 4
        delete_assob_not_possible      = 5
        delete_coift_not_possible      = 6
        update_assob_not_possible      = 7
        nearly_last_entry_out_of_range = 8
        last_entry_out_of_number_range = 9
        no_more_numbers_available      = 10
        interval_not_found             = 11
        number_range_not_found         = 12
        object_not_found               = 13
        quantity_is_0                  = 14
        unknown_error                  = 15
        delete_pdsnr_not_possible      = 16
        insert_pdsnr_not_possible      = 17
        insert_teven_more_not_possible = 18
        delete_teven_more_not_possible = 19
        others                         = 20.
    ```
  
  <!-- tab:信息类型长文本 -->
  
  - 读取长文本（还可以使用函数：`HR_READ_INFTY_NOTE`）

    ```abap
    data: lv_tclas    type tclas value 'A',
          ls_pskey    type pskey,
          lt_text_tab type hrpad_text_tab.
    
    move-corresponding p0552 to ls_pskey.
    ls_pskey-infty = '0552'.
    
    call method cl_hrpa_text_cluster=>read(
      exporting
        tclas         = lv_tclas
        pskey         = ls_pskey
        no_auth_check = 'X'
      importing
        text_tab      = lt_text_tab ).
    ```
  
  - 写入长文本（暂未研究），可参考 [Update long text in infotypes](https://blogs.sap.com/2013/04/30/update-long-text-in-infotypes/)、[长文本值未显示在 pa30 屏幕中 |SAP 社区](https://answers.sap.com/questions/7159166/long-text-value-not-displaying-in-pa30-screen.html)
  
  <!-- tab:其他常用函数 -->

  | 函数                             | 描述                                                         |
  | -------------------------------- | ------------------------------------------------------------ |
  | `BAPI_EMPLOYEE_ENQUEUE`          | 锁定员工                                                     |
  | `BAPI_EMPLOYEE_DEQUEUE`          | 解锁员工                                                     |
  | `HR_PSBUFFER_INITIALIZE`         | 清空缓存.在使用`HR_INFOTYPE_OPERATION`循环批量更新信息类型时,需要用于清空缓存,否则有可能会出现意想不到的问题 |
  | `HR_INFOTYPE_OPERATION`          | 信息类型数据更新,更新或者删除时,请指定全关键字<br />INS: 插入数据<br />DEL: 删除数据<br />MOD: 更新执行<br />CHK: 模拟执行 |
  | `HR_READ_INFOTYPE_AUTHC_DISABLE` | 跳过读权限,如果需要跳过权限,每次抵用`HR_READ_INFOTYPE`前都需要调用一次 |
  | `HR_READ_INFOTYPE`               | 读取某个员工的某个信息类型数据                               |
  | `HR_ECM_READ_IT0041_DATE_TYPE`   | 查询0041的日期                                               |
  |                                  |                                                              |
  
  <!-- tabs:end -->
