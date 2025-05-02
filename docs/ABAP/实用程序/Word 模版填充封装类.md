```abap
class zcl_fill_stencil_word definition
  public
  final
  create private.

  public section.
    types:
      begin of ty_object,
        uuid    type sysuuid_c32,
        stencil type ref to zcl_fill_stencil_word,
      end of ty_object .

    class-methods: factory
      importing
        !iv_uuid         type sysuuid_c32 optional
        !iv_objid        type wwwdatatab-objid optional
      exporting
        !ev_uuid         type sysuuid_c32
      returning
        value(eo_result) type ref to zcl_fill_stencil_word
      raising
        cx_uuid_error.

    methods: get_main_string
      returning value(ev_result) type string.

    methods: get_file_xstring
      importing
                iv_pdf           type c default ' '
      returning value(ev_result) type xstring.

    methods: set_main_string
      importing
        iv_value type string
      raising
        cx_openxml_format.

    methods: set_password_for_word
      importing
        iv_psword type string.

    methods conv_word_to_pdf
      importing
                iv_fullpath    type string optional
      returning value(ev_file) type xstring.

    methods select_and_save
      importing
        iv_filename type string
        iv_pdf      type c default ' '.

  protected section.
  private section.

    class-data:
      gt_object type hashed table of ty_object with unique key uuid .

    data: gv_file      type xstring,
          gv_file_pdf  type xstring,
          gv_main_xml  type xstring,
          go_document  type ref to cl_docx_document,
          go_main_part type ref to  cl_docx_maindocumentpart.

    data: gv_password type string.

    methods: constructor
      importing
        iv_objid type wwwdatatab-objid
      raising
        cx_openxml_format
        cx_openxml_not_found.

    methods: save_file
      importing
        !iv_fullpath type string
        !iv_file     type xstring.

    methods: delete_file
      importing
        iv_fullpath type string.

    methods: read_file
      importing
                iv_fullpath    type string
      exporting
                ev_filesize    type i
      returning value(ev_file) type xstring.

endclass.



class zcl_fill_stencil_word implementation.

  method factory.


    data: lv_notfound type c.

    if iv_uuid is not initial.
      read table gt_object into data(ls_object) with key uuid = iv_uuid.
      if sy-subrc ne 0.
        lv_notfound = 'X'.
      endif.
    endif.

    if iv_uuid is initial or lv_notfound is not initial.
      clear ls_object.

      if lv_notfound is not initial.
        ls_object-uuid = iv_uuid.
      else.
        ls_object-uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
      endif.

      ls_object-stencil = new zcl_fill_stencil_word( iv_objid ).
      insert ls_object into table gt_object.
    endif.

    ev_uuid = ls_object-uuid.
    eo_result = ls_object-stencil.

  endmethod.

  method constructor.

    data: ls_wwwdatatab type wwwdatatab,
          lt_mime       type solix_tab.

    select single * into @data(ls_wwwdata) from wwwdata where objid = @iv_objid.
    check ls_wwwdata is not initial.

    move-corresponding ls_wwwdata to ls_wwwdatatab.
    call function 'WWWDATA_IMPORT'
      exporting
        key  = ls_wwwdatatab
      tables
        mime = lt_mime.

    gv_file = cl_bcs_convert=>solix_to_xstring( it_solix = lt_mime ).

    go_document = cl_docx_document=>load_document( gv_file ).

    check go_document is not initial.

    go_main_part = go_document->get_maindocumentpart( ).

    gv_main_xml = go_main_part->get_data( ).

  endmethod.

  method get_main_string.
    ev_result = cl_abap_codepage=>convert_from( gv_main_xml ).
  endmethod.

  method get_file_xstring.
    if iv_pdf is initial.
      ev_result = gv_file.
    else.
      ev_result = gv_file_pdf.
    endif.
  endmethod.

  method set_main_string.

    gv_main_xml = cl_abap_codepage=>convert_to( iv_value ).
    go_main_part->feed_data( gv_main_xml ).
    gv_file = go_document->get_package_data( ).

  endmethod.

  method set_password_for_word.
    gv_password = iv_psword.

    data: lv_fullpath type string.

    if lv_fullpath is initial.
      cl_gui_frontend_services=>get_sapgui_workdir(
        changing
          sapworkdir            = lv_fullpath
        exceptions
          get_sapworkdir_failed = 1
          cntl_error            = 2
          error_no_gui          = 3
          not_supported_by_gui  = 4
          others                = 5 ).
    endif.

    if lv_fullpath is initial.
      cl_gui_frontend_services=>directory_browse(
        exporting
          window_title         = '选择转换文件夹'
          initial_folder       = 'C:\'
        changing
          selected_folder      = lv_fullpath
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          others               = 4 ).
    endif.

    data: lv_full_path_word    type string,
          lv_full_path_word_pd type string.

    try.
        lv_full_path_word    = |{ lv_fullpath }\\{ cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ) }.docx|.
      catch cx_uuid_error.
    endtry.

    save_file(
      iv_fullpath = lv_full_path_word
      iv_file     = gv_file ).


    data: lo_word type ole2_object,
          lo_docu type ole2_object.

    create object lo_word 'WORD.APPLICATION'.

    call method of lo_word 'Documents' = lo_docu.

    call method of lo_docu 'Open' = lo_docu
      exporting
        #1 = lv_full_path_word.

    set property of lo_docu 'Password' = gv_password.

    call method of lo_docu 'Save'.

    call method of lo_word 'QUIT'.

    gv_file = read_file(
      iv_fullpath = lv_full_path_word
    ).

    delete_file( lv_full_path_word ).

  endmethod.

  method conv_word_to_pdf.

    data: lv_fullpath type string.

    lv_fullpath = iv_fullpath.

    if lv_fullpath is initial.
      cl_gui_frontend_services=>get_sapgui_workdir(
        changing
          sapworkdir            = lv_fullpath
        exceptions
          get_sapworkdir_failed = 1
          cntl_error            = 2
          error_no_gui          = 3
          not_supported_by_gui  = 4
          others                = 5 ).
    endif.

    if lv_fullpath is initial.
      cl_gui_frontend_services=>directory_browse(
        exporting
          window_title         = '选择转换文件夹'
          initial_folder       = 'C:\'
        changing
          selected_folder      = lv_fullpath
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          others               = 4 ).
    endif.

    data: lv_full_path_word type string,
          lv_full_path_pdf  type string.

    try.
        lv_full_path_word = |{ lv_fullpath }\\{ cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ) }.docx|.
        lv_full_path_pdf  = |{ lv_fullpath }\\{  cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ) }.pdf|.
      catch cx_uuid_error.
    endtry.

    save_file(
      iv_fullpath = lv_full_path_word
      iv_file     = gv_file ).


    data: lo_word type ole2_object,
          lo_docu type ole2_object.

    create object lo_word 'WORD.APPLICATION'.

    call method of lo_word 'Documents' = lo_docu.

    call method of lo_docu 'Open' = lo_docu
      exporting
        #1 = lv_full_path_word
        #5 = gv_password.

    call method of lo_docu 'ExportAsFixedFormat' = lo_docu
      exporting
         #1 = lv_full_path_pdf
         #2 = '17'.

    call method of lo_word 'QUIT'.

    gv_file_pdf = read_file(
      iv_fullpath = lv_full_path_pdf
    ).

    if gv_file_pdf is initial.
      gv_file_pdf = gv_file.
    endif.

    delete_file( lv_full_path_word ).
    delete_file( lv_full_path_pdf ).

    ev_file = gv_file_pdf.

  endmethod.

  method select_and_save.

    data: lv_filename    type string,
          lv_path        type string,
          lv_fullpath    type string,
          lv_file_filter type string,
          lv_file        type xstring.

    if iv_pdf is not initial.
      lv_file_filter = 'PDF'.
      lv_file = gv_file_pdf.
    else.
      lv_file_filter = cl_gui_frontend_services=>filetype_word.
      lv_file = gv_file.
    endif.

    lv_filename = iv_filename.

    call method cl_gui_frontend_services=>file_save_dialog
      exporting
        default_file_name         = lv_filename
        file_filter               = lv_file_filter
        prompt_on_overwrite       = 'X'
      changing
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
      exceptions
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        others                    = 5.

    if lv_fullpath is not initial.
      save_file(
        iv_fullpath = lv_fullpath
        iv_file     = lv_file ).
    endif.

  endmethod.

  method save_file.

    data: lt_data_tab type solix_tab,
          lv_filesize type i.

    lt_data_tab = cl_bcs_convert=>xstring_to_solix( iv_file ).
    lv_filesize = xstrlen( iv_file ).

    cl_gui_frontend_services=>gui_download(
      exporting
        bin_filesize            = lv_filesize
        filename                = iv_fullpath
        filetype                = 'BIN'
        confirm_overwrite       = 'X'
      changing
        data_tab                = lt_data_tab
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
        others                  = 24 ).

    if sy-subrc ne 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.

  method delete_file.

    data: lv_rc type i.

    cl_gui_frontend_services=>file_delete(
      exporting
        filename             = iv_fullpath
      changing
        rc                   = lv_rc
      exceptions
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        others               = 9 ).

  endmethod.

  method read_file.

    data: lv_filesize type i,
          lt_data_tab type solix_tab.

    cl_gui_frontend_services=>gui_upload(
      exporting
        filename                = iv_fullpath
        filetype                = 'BIN'
      importing
        filelength              = lv_filesize
      changing
        data_tab                = lt_data_tab
      exceptions
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        others                  = 19 ).

    ev_filesize = lv_filesize.
    ev_file = cl_bcs_convert=>solix_to_xstring(
      it_solix = lt_data_tab
      iv_size  = lv_filesize
    ).

  endmethod.

endclass.
```

