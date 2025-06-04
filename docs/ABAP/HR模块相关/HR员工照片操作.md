## 员工照片工具类

<!-- tabs:start -->

<!-- tab:类定义 -->

```abap
class zcl_staff_photo_tool definition create private.
  public section.

    constants: gc_ar_object  type saeobjart value 'HRICOLFOTO',
               gc_sap_object type saeanwdid value 'PREL',
               gc_doc_type   type saedoktyp value 'JPG',
               gc_archiv_id  type char2 value 'Z1'.

    types: tt_files type standard table of file_info with default key.


    class-methods factory
      importing
                iv_pernr         type pernr_d
                iv_infty         type infty default '0002'
      returning value(ro_result) type ref to zcl_staff_photo_tool.

    "--------------------> 16进制转2进制
    class-methods xstring_to_binary
      importing
                iv_xstring       type any
      exporting
                ev_length        type i
      returning value(rt_result) type sdokcntbins.

    "--------------------> 2进制转16进制
    class-methods binary_to_xstring
      importing
                iv_length        type i
                it_bintab        type sdokcntbins
      returning value(rv_result) type xstring.

    "--------------------> 获取照片文件夹
    class-methods get_photos_folder
      returning value(rv_result) type string.

    "--------------------> 获取文件夹文件
    class-methods directory_list_files
      importing
                iv_path         type any
      exporting
                et_files_no_ext type table
      returning value(rt_files) type tt_files.

    "--------------------> 获取文件名
    class-methods get_file_name_no_ext
      importing
                iv_filename      type any
      returning value(rv_result) type string.

    "--------------------> 判断是否为人员编号
    class-methods is_string_employee_id
      importing
                iv_string        type any
      returning value(rv_result) type boole.

    "--------------------> 判断照片是否存在
    methods is_image_exists
      exporting
                es_connect_info  type toav0
      returning value(rv_result) type boole.

    "--------------------> 获取员工照片
    methods get_staff_photo
      exporting
                ev_file          type xstring
                et_file_content  type sdokcntbins
      returning value(rv_result) type boole.

    "--------------------> 上传员工照片
    methods archiv_create_file
      importing
                iv_file          type any optional
                iv_path          type any optional
      exporting
                ev_message       type any
      returning value(rv_result) type boole.

    "--------------------> 删除员工照片
    methods archiv_delete_meta.


  private section.
    class-data: begin of gs_object,
                  pernr  type pernr_d,
                  object type ref to zcl_staff_photo_tool,
                end of gs_object,
                gt_object like hashed table of gs_object with unique key pernr.

    data: gv_pernr        type pernr_d,
          gv_infty        type infty,
          gv_exists       type c,
          gs_connect_info type toav0,
          gv_doc_id       type char40,
          gv_file         type xstring,
          gv_length       type i,
          gt_bin_content  type sdokcntbins.

    methods constructor
      importing
        iv_pernr type any
        iv_infty type any.

    methods archiv_create_file_by_path
      importing
                iv_path          type any
      exporting
                ev_message       type any
      returning value(rv_result) type boole.

    methods archiv_create_file_by_file
      importing
                iv_file          type any
      exporting
                ev_message       type any
      returning value(rv_result) type boole.

    methods scms_doc_create
      exporting
                ev_message       type any
      returning value(rv_result) type boole.

    methods archiv_connection_insert
      exporting
                ev_message       type any
      returning value(rv_result) type boole.

endclass.
```

<!-- tab:类实现 -->

```abap
class zcl_staff_photo_tool implementation.

  method factory.

    read table gt_object into gs_object with key pernr = iv_pernr.
    if sy-subrc ne 0.
      gs_object = value #( pernr  = iv_pernr
                           object = new zcl_staff_photo_tool( iv_pernr = iv_pernr iv_infty = iv_infty ) ).
      insert gs_object into table gt_object.
    endif.

    ro_result = gs_object-object.

  endmethod.

  method xstring_to_binary.

    data: lv_xstring    type xstring,
          lv_length     type i,
          lt_binary_tab type sdokcntbins.


    lv_xstring = iv_xstring.
    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = lv_xstring
      importing
        output_length = lv_length
      tables
        binary_tab    = lt_binary_tab.

    ev_length = lv_length.
    rt_result = lt_binary_tab.

  endmethod.

  method binary_to_xstring.

    data: lv_length     type i,
          lv_xstring    type xstring,
          lt_binary_tab type sdokcntbins.

    lt_binary_tab = it_bintab.
    lv_length = iv_length.

    call function 'SCMS_BINARY_TO_XSTRING'
      exporting
        input_length = lv_length
      importing
        buffer       = lv_xstring
      tables
        binary_tab   = lt_binary_tab
      exceptions
        failed       = 1
        others       = 2.

    rv_result = lv_xstring.

  endmethod.

  method get_photos_folder.

    data: lv_selected_folder type string.

    call method cl_gui_frontend_services=>directory_browse
      changing
        selected_folder      = lv_selected_folder
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4.

    if sy-subrc ne 0.
      message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    endif.

    rv_result = lv_selected_folder.

  endmethod.

  method directory_list_files.

    data: lt_files            type tt_files,
          lt_files_no_ext     type table of localfile,
          lv_directory        type string,
          lv_files_only       type abap_bool value 'X',
          lv_directories_only type abap_bool,
          lv_count            type i.

    lv_directory = iv_path.

    cl_gui_frontend_services=>directory_list_files(
      exporting
        directory                   = lv_directory
        filter                      = '*.*'
        files_only                  = lv_files_only
        directories_only            = lv_directories_only
      changing
        file_table                  = lt_files
        count                       = lv_count
      exceptions
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        others                      = 6 ).

    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    endif.

    delete lt_files where ishidden is not initial or issystem is not initial.

    loop at lt_files into data(ls_file).
      append initial line to lt_files_no_ext assigning field-symbol(<fs_line>).
      <fs_line> = get_file_name_no_ext( ls_file-filename ).
    endloop.

    rt_files = lt_files.
    et_files_no_ext = lt_files_no_ext.

  endmethod.

  method get_file_name_no_ext.

    data: lv_len      type i,
          lv_pos      type i,
          lv_fullname type char1024.

    lv_fullname = iv_filename.

    lv_len = strlen( lv_fullname ).
    lv_pos = lv_len.

    while lv_fullname+lv_pos(1) <> '.' and lv_pos > 0.
      lv_pos = lv_pos - 1.
    endwhile.

    rv_result = lv_fullname+0(lv_pos).
    condense rv_result no-gaps.

  endmethod.

  method is_string_employee_id.

    data: lv_string type string.

    lv_string = iv_string.

    rv_result = abap_true.

    if strlen( lv_string ) gt 8.
      clear rv_result.
      return.
    endif.

    data: lv_htype type char4.
    call function 'NUMERIC_CHECK'
      exporting
        string_in = lv_string
      importing
        htype     = lv_htype.

    if lv_htype ne 'NUMC'.
      clear rv_result.
      return.
    endif.

    select count(*) from pa0000 where pernr eq lv_string.
    if sy-subrc ne 0.
      clear rv_result.
    endif.

  endmethod.

  method is_image_exists.

    call function 'HR_IMAGE_EXISTS'
      exporting
        p_pernr               = gv_pernr
      importing
        p_exists              = gv_exists
        p_connect_info        = gs_connect_info
      exceptions
        error_connectiontable = 1
        others                = 2.

    rv_result = gv_exists.
    es_connect_info = gs_connect_info.

  endmethod.

  method get_staff_photo.

    data: lv_stor_cat    type sdok_stcat,
          lt_access_info type standard table of scms_acinf,
          lt_content_txt type standard table of sdokcntasc,
          lt_content_bin type standard table of sdokcntbin.

    rv_result = is_image_exists(  ).

    check rv_result eq abap_true.

    call function 'SCMS_DOC_READ'
      exporting
        stor_cat              = lv_stor_cat
        crep_id               = gs_connect_info-archiv_id
        doc_id                = gs_connect_info-arc_doc_id
        signature             = 'X'
      tables
        access_info           = lt_access_info
        content_txt           = lt_content_txt
        content_bin           = lt_content_bin
      exceptions
        bad_storage_type      = 1
        bad_request           = 2
        unauthorized          = 3
        comp_not_found        = 4
        not_found             = 5
        forbidden             = 6
        conflict              = 7
        internal_server_error = 8
        error_http            = 9
        error_signature       = 10
        error_config          = 11
        error_format          = 12
        error_parameter       = 13
        error                 = 14
        others                = 15.

    read table lt_access_info into data(ls_access_info) index 1.
    if sy-subrc eq 0.

      gv_length = ls_access_info-comp_size.

      gv_file = binary_to_xstring(
        exporting
          iv_length = gv_length
          it_bintab = lt_content_bin ).

      gt_bin_content = xstring_to_binary( gv_file ).

    endif.

  endmethod.

  method archiv_create_file.

    if iv_path is initial and iv_file is initial.
      rv_result = abap_false.
      return.
    endif.

    if iv_path is not initial.
      rv_result = archiv_create_file_by_path(
        exporting
          iv_path    = iv_path
        importing
          ev_message = ev_message
      ).
    else.
      rv_result = archiv_create_file_by_file(
        exporting
          iv_file    = iv_file
        importing
          ev_message = ev_message
      ).
    endif.

  endmethod.

  method archiv_delete_meta.

*    call function 'ARCHIV_DELETE_META'

  endmethod.

  method constructor.

    gv_pernr = iv_pernr.
    gv_infty = iv_infty.
    if gv_infty is initial.
      gv_infty = '0002'.
    endif.

  endmethod.

  method archiv_create_file_by_path.

    data: lv_object_id type saeobjid,
          lv_path      type saepfad.

    lv_object_id = |{ gv_pernr alpha = in }{ gv_infty }|.
    lv_path = iv_path.

    call function 'ARCHIV_CREATE_FILE'
      exporting
        ar_object               = gc_ar_object
        sap_object              = gc_sap_object
        doc_type                = gc_doc_type
        object_id               = lv_object_id
        path                    = lv_path
      exceptions
        error_conectiontable    = 1
        error_parameter         = 2
        error_archiv            = 3
        error_upload            = 4
        error_kernel            = 5
        no_entry_possible       = 6
        error_comunicationtable = 7
        blocked_by_policy       = 8
        others                  = 9.

    if sy-subrc <> 0.
      rv_result = abap_false.
      message id sy-msgid type 'S' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into ev_message.
    else.
      rv_result = abap_true.
      clear ev_message.
    endif.

  endmethod.

  method archiv_create_file_by_file.

    "--------------------> 16进制转二进制
    data: lv_length type i.
    gt_bin_content = xstring_to_binary(
      exporting
        iv_xstring = gv_file
      importing
        ev_length  = gv_length ).

    "--------------------> 上传文件
    rv_result = scms_doc_create(
      importing
        ev_message = ev_message
    ).

    "--------------------> 建立文档和人员关系
    rv_result = archiv_connection_insert(
      importing
        ev_message = ev_message
    ).

  endmethod.

  method scms_doc_create.

    data: lv_stor_cat    type sdok_stcat,
          lv_crep_id     type char2,
          lt_access_info type table of scms_acinf,
          lt_content_bin type sdokcntbins.

    check gv_length is not initial.

    lv_crep_id = gc_archiv_id.
    lt_content_bin = gt_bin_content.
    lt_access_info = value #( ( comp_id    = 'data'
                                mimetype   = 'image/jpeg'
                                comp_size  = gv_length
                                binary_flg = 'X'
                                first_line = 1
                                last_line  = lines( lt_content_bin ) ) ).

    call function 'SCMS_DOC_CREATE'
      exporting
        stor_cat              = lv_stor_cat
        crep_id               = lv_crep_id
      importing
        doc_id_out            = gv_doc_id
      tables
        access_info           = lt_access_info
        content_bin           = lt_content_bin
      exceptions
        bad_storage_type      = 1
        bad_request           = 2
        unauthorized          = 3
        forbidden             = 4
        conflict              = 5
        internal_server_error = 6
        error_http            = 7
        error_signature       = 8
        error_config          = 9
        error_hierarchy       = 10
        error_format          = 11
        error_parameter       = 12
        error                 = 13
        blocked_by_policy     = 14
        others                = 15.

    if sy-subrc <> 0.
      rv_result = abap_false.
      message id sy-msgid type 'S' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into ev_message.
    else.
      rv_result = abap_true.
      clear ev_message.
    endif.

  endmethod.

  method archiv_connection_insert.

    data: lv_archiv_id  type saearchivi,
          lv_arc_doc_id type saeardoid,
          lv_object_id  type saeobjid.

    check gv_doc_id is not initial.

    lv_archiv_id = gc_archiv_id.
    lv_arc_doc_id = gv_doc_id.
    lv_object_id = |{ gv_pernr alpha = in }{ gv_infty }|.

    call function 'ARCHIV_CONNECTION_INSERT'
      exporting
        ar_object             = gc_ar_object
        sap_object            = gc_sap_object
        doc_type              = gc_doc_type
        arc_doc_id            = lv_arc_doc_id
        archiv_id             = lv_archiv_id
        object_id             = lv_object_id
      exceptions
        error_connectiontable = 1
        others                = 2.
    if sy-subrc <> 0.
      rv_result = abap_false.
      message id sy-msgid type 'S' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into ev_message.
    else.
      rv_result = abap_true.
      clear ev_message.
    endif.

  endmethod.

endclass.

```

<!-- tabs:end -->

## 照片存储库信息

可在T-code:`OAC0`中查看相关配置，例如工具类使用的Z1:![照片文件存储表-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240522160244617.png)

![照片文件存储表-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240522160321473.png)
