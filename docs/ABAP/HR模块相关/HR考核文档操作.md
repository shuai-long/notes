[参考链接](https://community.sap.com/t5/application-development-and-automation-blog-posts/hr-appraisal-function-modules-with-screenshots/ba-p/13235651)

- **获取考核文档列表**

  HRHAP_DOCUMENT_GET_LIST_XXL 

  ```abap
    data: gt_pernr type range of pernr_d,
    			gt_data  type table of hap_s_documents.
    
    methods get_users_evaluation_docs.
    method get_users_evaluation_docs.
  
      data: lv_sel_filter          type c,
            lv_add_on_application  type hap_add_on_application,
            lv_plan_version        type hap_plan_version,
            lt_categories          type hap_t_c,
            lt_templates           type hap_t_hrobject,
            lt_tmpl_detail         type hap_t_sel_tmpl_detail,
            lt_appraisers          type hap_t_hrsobid,
            ls_and_or              type hap_s_sel_option_and_or,
            lt_appraisees          type hap_t_hrsobid,
            lt_part_appraisers     type hap_t_hrsobid,
            lt_others              type hap_t_hrsobid,
            ls_sel_date            type hap_s_sel_dates,
            ls_sel_status          type hap_s_sel_status,
            lt_sel_status_sub      type hap_t_status_sub,
            ls_sel_with_or_without type hap_s_sel_with_or_without,
            ls_document_fill       type hap_s_document_fill,
            lv_extract_m_n         type char1,
            lv_no_dialog           type boole_d,
            lt_documents           type hap_t_documents,
            ls_return              type bal_s_msg.
  
      check gt_pernr is not initial.
  
      ls_and_or-apper_and_appee = 'X'.
      loop at s_tempid into data(ls_tempid).
        lt_templates = value #( base lt_templates ( plvar = '01' otype = 'VA' objid = ls_tempid-low ) ).
      endloop.
      lv_plan_version = '01'.
      ls_sel_status = 'XXXXXXXXX'.
  
      loop at gt_pernr into data(ls_pernr).
        lt_appraisees = value #( base lt_appraisees ( plvar = '01' otype = 'P' sobid = ls_tempid-low ) ).
      endloop.
  
      lt_sel_status_sub = value #( ( ap_status = '2' ap_status_sub = '1' )
                                   ( ap_status = '2' ap_status_sub = '2' )
                                   ( ap_status = '2' ap_status_sub = '3' )
                                   ( ap_status = '2' ap_status_sub = 'A' )
                                   ( ap_status = '3' ap_status_sub = '1' )
                                   ( ap_status = '3' ap_status_sub = '2' )
                                   ( ap_status = '3' ap_status_sub = 'A' )
                                   ( ap_status = '4' ap_status_sub = ' ' )
                                   ( ap_status = '4' ap_status_sub = '1' )
                                   ( ap_status = '4' ap_status_sub = '2' )
                                   ( ap_status = '4' ap_status_sub = '3' )
                                   ( ap_status = '4' ap_status_sub = '4' )
                                   ( ap_status = '4' ap_status_sub = '5' )
                                   ( ap_status = '4' ap_status_sub = '6' )
                                   ( ap_status = '4' ap_status_sub = '7' )
                                   ( ap_status = '4' ap_status_sub = '8' )
                                   ( ap_status = '4' ap_status_sub = '9' )
                                   ( ap_status = '4' ap_status_sub = 'A' )
                                   ( ap_status = '4' ap_status_sub = 'B' ) ).
  
      lv_add_on_application = 'PA'.
  
      ls_sel_with_or_without-sel_display_existing = 'X'.
  
      call function 'HRHAP_DOCUMENT_GET_LIST_XXL'
        exporting
          sel_filter            = lv_sel_filter
          add_on_application    = lv_add_on_application
          plan_version          = lv_plan_version
          t_categories          = lt_categories
          t_templates           = lt_templates
          t_tmpl_detail         = lt_tmpl_detail
          t_appraisers          = lt_appraisers
          s_and_or              = ls_and_or
          t_appraisees          = lt_appraisers
          t_part_appraisers     = lt_part_appraisers
          t_others              = lt_others
          s_sel_date            = ls_sel_date
          s_sel_status          = ls_sel_status
          t_sel_status_sub      = lt_sel_status_sub
          s_sel_with_or_without = ls_sel_with_or_without
          s_document_fill       = ls_document_fill
          extract_m_n           = lv_extract_m_n
          no_dialog             = lv_no_dialog
        importing
          t_documents           = lt_documents
          s_return              = ls_return.
  
      move-corresponding lt_documents to gt_data.
  
    endmethod.
  ```

  

- **获取文档列表**

  BDC_GOS_CONNECTIONS_GET 

  ```abap
    methods call_bds_gos_connections_get
      importing
                iv_objkey        type any
      returning value(rt_result) type tt_gos_connections.
      
    method call_bds_gos_connections_get.
  
      data: lv_objkey      type swo_typeid,
            lt_connections type table of bdn_con.
  
      lv_objkey = iv_objkey.
      call function 'BDS_GOS_CONNECTIONS_GET'
        exporting
          classname          = ''
          objkey             = lv_objkey
        tables
          gos_connections    = lt_connections
        exceptions
          no_objects_found   = 1
          internal_error     = 2
          internal_gos_error = 3
          others             = 4.
  
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
  
      rt_result = lt_connections.
  
    endmethod.
  ```

- **获取文档内容**

  SO_DOCUMENT_READ_API1

  ```abap
    methods call_so_document_read_api1
    	importing
                iv_document_id   type any
      returning value(rt_result) type tt_solix.
      
    method call_so_document_read_api1.
  
      data: lv_document_id     type so_entryid,
            ls_document_data   type sofolenti1,
            lt_object_header   type standard table of solisti1,
            lt_object_content  type standard table of solisti1,
            lt_object_para     type standard table of soparai1,
            lt_object_parb     type standard table of soparbi1,
            lt_attachment_list type standard table of soattlsti1,
            lt_receiver_list   type standard table of soreclsti1,
            lt_contents_hex    type standard table of solix.
  
      lv_document_id = iv_document_id.
      call function 'SO_DOCUMENT_READ_API1'
        exporting
          document_id                = lv_document_id
        importing
          document_data              = ls_document_data
        tables
          object_header              = lt_object_header
          object_content             = lt_object_content
          object_para                = lt_object_para
          object_parb                = lt_object_parb
          attachment_list            = lt_attachment_list
          receiver_list              = lt_receiver_list
          contents_hex               = lt_contents_hex
        exceptions
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          others                     = 4.
  
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
  
      rt_result = lt_contents_hex.
  
    endmethod.
  ```

## 整体代码

```abap

tables: pernr, hap_s_dynp_report_1000.

nodes: peras.

infotypes: 0000, 0001.

types: tt_solix           type standard table of solix with default key,
       tt_gos_connections type standard table of bdn_con with default key.
constants: gc_report_title type string value '考核文档批量下载'.

selection-screen begin of block b1 with frame title text-001.
  select-options: s_tempid for hap_s_dynp_report_1000-template_id no intervals.
selection-screen end of block b1.

class cl_report definition.
  public section.

    class-methods init_screen_values.

    class-methods set_f4_for_tempid_low.

    class-methods set_infotype_fetch_time.

    methods constructor.

    methods get_user_information.

    methods get_users_evaluation_docs.

    methods display_evaluation_docs_list.

    methods download_evaluation_document
      importing
        iv_index type any.

    methods call_bds_gos_connections_get
      importing
                iv_objkey        type any
      returning value(rt_result) type tt_gos_connections.

    methods call_so_document_read_api1
      importing
                iv_document_id   type any
      returning value(rt_result) type tt_solix.

    methods select_folder_and_save_file
      importing
        iv_filename type any
        it_filedata type tt_solix.

    methods set_fcat.

    methods set_layo.

    methods handle_toolbar
      for event toolbar of cl_gui_alv_grid
      importing
        !e_object
        !e_interactive .
    methods handle_user_command
      for event user_command of cl_gui_alv_grid
      importing
        !e_ucomm .

  private section.
    data: gt_pernr type range of pernr_d.

    data: go_alv_grid type ref to cl_gui_alv_grid,
          gt_data     type table of hap_s_documents,
          gt_fcat     type lvc_t_fcat,
          gs_layo     type lvc_s_layo,
          gs_stbl     type lvc_s_stbl value 'XX'.

endclass.

initialization.
  cl_report=>init_screen_values( ).

at selection-screen on value-request for s_tempid-low.
  cl_report=>set_f4_for_tempid_low( ).

start-of-selection.
  cl_report=>set_infotype_fetch_time( ).
  data(go_report) = new cl_report( ).

get peras.
  go_report->get_user_information( ).

get peras late.
  go_report->get_users_evaluation_docs( ).

end-of-selection.
  call screen 9000.

module status_9000 output.

  set titlebar 'STATUS' with gc_report_title.

  set pf-status 'STAND'.

  go_report->display_evaluation_docs_list( ).

endmodule.

module user_command_9000 input.

  case sy-ucomm.
    when 'BACK' or '%EX' or 'RW'.
      set screen 0.
  endcase.

endmodule.

class cl_report implementation.

  method init_screen_values.

    %_s_tempid_%_app_%-text = |评价模板|.

  endmethod.

  method set_f4_for_tempid_low.

    data: lt_selected  type hap_t_templates,
          lt_templates type hap_t_templates,
          ls_return    type bal_s_msg.

    select * into corresponding fields of table lt_selected
      from hrp1000 where plvar eq '01' and otype eq 'VA' and objid in s_tempid.

    call function 'HRHAP_POPUP_F4_TEMPLATE'
      exporting
        authority_check    = '03'
        plan_version       = '01'
        multiple_selection = 'X'
        t_selected         = lt_selected
      importing
        t_templates        = lt_templates
        s_return           = ls_return.

    clear: s_tempid, s_tempid[].
    loop at lt_templates into data(ls_templates).
      if sy-tabix eq 1.
        s_tempid = value #( sign = 'I' option = 'EQ' low = ls_templates-objid ).
      endif.

      s_tempid[] = value #( base s_tempid[] sign = 'I' option = 'EQ' ( low = ls_templates-objid ) ).
    endloop.

  endmethod.

  method set_infotype_fetch_time.
*    rp-set-data-interval

  endmethod.

  method constructor.

    set_fcat( ).
    set_layo( ).

  endmethod.

  method get_user_information.

    rp_provide_from_last p0000 space pn-begda pn-endda.
    if p0000-stat2 not in pnpstat2. return. endif.

    rp_provide_from_last p0001 space pn-begda pn-endda.
    if p0001-werks not in pnpwerks. return. endif.
    if p0001-btrtl not in pnpbtrtl. return. endif.
    if p0001-persg not in pnppersg. return. endif.
    if p0001-persk not in pnppersk. return. endif.

    gt_pernr = value #( base gt_pernr sign = 'I' option = 'EQ' ( low = pernr-pernr ) ).

  endmethod.

  method get_users_evaluation_docs.

    data: lv_sel_filter          type c,
          lv_add_on_application  type hap_add_on_application,
          lv_plan_version        type hap_plan_version,
          lt_categories          type hap_t_c,
          lt_templates           type hap_t_hrobject,
          lt_tmpl_detail         type hap_t_sel_tmpl_detail,
          lt_appraisers          type hap_t_hrsobid,
          ls_and_or              type hap_s_sel_option_and_or,
          lt_appraisees          type hap_t_hrsobid,
          lt_part_appraisers     type hap_t_hrsobid,
          lt_others              type hap_t_hrsobid,
          ls_sel_date            type hap_s_sel_dates,
          ls_sel_status          type hap_s_sel_status,
          lt_sel_status_sub      type hap_t_status_sub,
          ls_sel_with_or_without type hap_s_sel_with_or_without,
          ls_document_fill       type hap_s_document_fill,
          lv_extract_m_n         type char1,
          lv_no_dialog           type boole_d,
          lt_documents           type hap_t_documents,
          ls_return              type bal_s_msg.

    check gt_pernr is not initial.

    ls_and_or-apper_and_appee = 'X'.
    loop at s_tempid into data(ls_tempid).
      lt_templates = value #( base lt_templates ( plvar = '01' otype = 'VA' objid = ls_tempid-low ) ).
    endloop.
    lv_plan_version = '01'.
    ls_sel_status = 'XXXXXXXXX'.

    loop at gt_pernr into data(ls_pernr).
      lt_appraisees = value #( base lt_appraisees ( plvar = '01' otype = 'P' sobid = ls_tempid-low ) ).
    endloop.

    lt_sel_status_sub = value #( ( ap_status = '2' ap_status_sub = '1' )
                                 ( ap_status = '2' ap_status_sub = '2' )
                                 ( ap_status = '2' ap_status_sub = '3' )
                                 ( ap_status = '2' ap_status_sub = 'A' )
                                 ( ap_status = '3' ap_status_sub = '1' )
                                 ( ap_status = '3' ap_status_sub = '2' )
                                 ( ap_status = '3' ap_status_sub = 'A' )
                                 ( ap_status = '4' ap_status_sub = ' ' )
                                 ( ap_status = '4' ap_status_sub = '1' )
                                 ( ap_status = '4' ap_status_sub = '2' )
                                 ( ap_status = '4' ap_status_sub = '3' )
                                 ( ap_status = '4' ap_status_sub = '4' )
                                 ( ap_status = '4' ap_status_sub = '5' )
                                 ( ap_status = '4' ap_status_sub = '6' )
                                 ( ap_status = '4' ap_status_sub = '7' )
                                 ( ap_status = '4' ap_status_sub = '8' )
                                 ( ap_status = '4' ap_status_sub = '9' )
                                 ( ap_status = '4' ap_status_sub = 'A' )
                                 ( ap_status = '4' ap_status_sub = 'B' ) ).

    lv_add_on_application = 'PA'.

    ls_sel_with_or_without-sel_display_existing = 'X'.

    call function 'HRHAP_DOCUMENT_GET_LIST_XXL'
      exporting
        sel_filter            = lv_sel_filter
        add_on_application    = lv_add_on_application
        plan_version          = lv_plan_version
        t_categories          = lt_categories
        t_templates           = lt_templates
        t_tmpl_detail         = lt_tmpl_detail
        t_appraisers          = lt_appraisers
        s_and_or              = ls_and_or
        t_appraisees          = lt_appraisers
        t_part_appraisers     = lt_part_appraisers
        t_others              = lt_others
        s_sel_date            = ls_sel_date
        s_sel_status          = ls_sel_status
        t_sel_status_sub      = lt_sel_status_sub
        s_sel_with_or_without = ls_sel_with_or_without
        s_document_fill       = ls_document_fill
        extract_m_n           = lv_extract_m_n
        no_dialog             = lv_no_dialog
      importing
        t_documents           = lt_documents
        s_return              = ls_return.

    move-corresponding lt_documents to gt_data.

  endmethod.

  method display_evaluation_docs_list.

    if go_alv_grid is initial.

*      go_alv_grid = new cl_gui_alv_grid( new cl_gui_docking_container(
*        repid = sy-repid
*        dynnr = '9000'
*        ratio = '95' ) ).

      go_alv_grid = new cl_gui_alv_grid( i_parent = cl_gui_container=>default_screen ).

      set handler handle_toolbar for go_alv_grid.
      set handler handle_user_command for go_alv_grid.

      go_alv_grid->set_toolbar_interactive( ).
      go_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
      go_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).

      go_alv_grid->set_table_for_first_display(
        exporting
          i_save                        = 'A'
          is_layout                     = gs_layo
        changing
          it_outtab                     = gt_data
          it_fieldcatalog               = gt_fcat
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4 ).

    else.
      go_alv_grid->refresh_table_display( is_stable = gs_stbl ).
    endif.

  endmethod.

  method download_evaluation_document.

    data: lv_index    type i,
          lv_filename type string.

    lv_index = iv_index.

    read table gt_data into data(ls_data) index lv_index.
    if sy-subrc eq 0.

      data(lt_connections) = call_bds_gos_connections_get( iv_objkey = ls_data-appraisal_id ).

      loop at lt_connections into data(ls_connection).

        data(lt_contents) = call_so_document_read_api1( ls_connection-loio_id ).

        select_folder_and_save_file(
          iv_filename = lv_filename
          it_filedata = lt_contents
        ).

      endloop.

    endif.

  endmethod.

  method call_bds_gos_connections_get.

    data: lv_objkey      type swo_typeid,
          lt_connections type table of bdn_con.

    lv_objkey = iv_objkey.
    call function 'BDS_GOS_CONNECTIONS_GET'
      exporting
        classname          = ''
        objkey             = lv_objkey
      tables
        gos_connections    = lt_connections
      exceptions
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        others             = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    rt_result = lt_connections.

  endmethod.

  method call_so_document_read_api1.

    data: lv_document_id     type so_entryid,
          ls_document_data   type sofolenti1,
          lt_object_header   type standard table of solisti1,
          lt_object_content  type standard table of solisti1,
          lt_object_para     type standard table of soparai1,
          lt_object_parb     type standard table of soparbi1,
          lt_attachment_list type standard table of soattlsti1,
          lt_receiver_list   type standard table of soreclsti1,
          lt_contents_hex    type standard table of solix.

    lv_document_id = iv_document_id.
    call function 'SO_DOCUMENT_READ_API1'
      exporting
        document_id                = lv_document_id
      importing
        document_data              = ls_document_data
      tables
        object_header              = lt_object_header
        object_content             = lt_object_content
        object_para                = lt_object_para
        object_parb                = lt_object_parb
        attachment_list            = lt_attachment_list
        receiver_list              = lt_receiver_list
        contents_hex               = lt_contents_hex
      exceptions
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        others                     = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    rt_result = lt_contents_hex.

  endmethod.

  method select_folder_and_save_file.

    data: lv_fullpath type string,
          lv_filename type string,
          lv_filepath type string.

    cl_gui_frontend_services=>file_save_dialog(
      exporting
        default_file_name         = lv_filename
        prompt_on_overwrite       = 'X'
      changing
        filename                  = lv_filename
        path                      = lv_filepath
        fullpath                  = lv_fullpath
      exceptions
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        others                    = 5 ).

    check lv_fullpath is not initial.

    data: lv_filelength type i,
          lt_filedata   type solix_tab.

    lt_filedata = it_filedata.

    cl_gui_frontend_services=>gui_download(
      exporting
        filename                = lv_fullpath
        filetype                = 'BIN'
        confirm_overwrite       = 'X'
      importing
        filelength              = lv_filelength
      changing
        data_tab                = lt_filedata
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others                  = 24
    ).

  endmethod.

  method handle_toolbar.

    e_object->mt_toolbar = value #( base e_object->mt_toolbar ( function = 'DOWNLOAD' text = |文档批量下载| ) ).

  endmethod.

  method handle_user_command.

    data: lt_rows type lvc_t_row.

    go_alv_grid->get_selected_rows( importing et_index_rows = lt_rows ).

    case e_ucomm.
      when 'DOWNLOAD'.

        if lt_rows is initial.
          message s001(00) with |请至少选择一条数据| display like 'E'.
        else.
          loop at lt_rows into data(ls_row).
            download_evaluation_document( ls_row-index ).
          endloop.
        endif.

    endcase.

  endmethod.

  method set_layo.

    gs_layo-sel_mode = 'A'.
    gs_layo-zebra    = 'X'.

  endmethod.

  method set_fcat.
    gt_fcat = value #( ( fieldname = 'counter'             reptext = '编号' )
                       ( fieldname = 'appraisal_name'      reptext = '评估文档名称' )
                       ( fieldname = 'appraisal_name'      reptext = '评估文档类型' )
                       ( fieldname = 'APPRAISEE_TYPE'      reptext = 'Appraisee Type' )
                       ( fieldname = 'APPRAISEE_TYPE_TEXT' reptext = 'Appraisee Type Text' )
                       ( fieldname = 'Appraisee_Name'      reptext = 'Appraisee Name' )
                       ( fieldname = 'AP_STATUS'           reptext = 'Appraisal Status' )
                       ( fieldname = 'appraisal_name'      reptext = '期间' )
                       ( fieldname = 'appraisal_name'      reptext = '至' )
                       ( fieldname = 'appraisal_name'      reptext = '日期' )
                       ( fieldname = 'appraisal_name'      reptext = '日期' )
                       ( fieldname = 'appraisal_name'      reptext = '评估日期' )
                       ( fieldname = 'appraisal_name'      reptext = '最后更改日期' )
                       ( fieldname = 'appraisal_name'      reptext = '修改时间' )
                       ( fieldname = 'appraisal_name'      reptext = '上次更改者' ) ).

    loop at gt_fcat assigning field-symbol(<fs_fcat>).
      <fs_fcat>-coltext = <fs_fcat>-scrtext_l = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_s = <fs_fcat>-reptext.
      <fs_fcat>-fieldname = to_upper( <fs_fcat>-fieldname ).
    endloop.

  endmethod.

endclass.
```

