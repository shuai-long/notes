- SAP 标准示例程序：`SAPHTML_EVENTS_DEMO`

- 使用 URL 展示 HTML 页面

  ```abap
  *&---------------------------------------------------------------------*
  *& Report zty_show_html
  *&---------------------------------------------------------------------*
  *& 使用 url 地址展示 html 页面
  *&---------------------------------------------------------------------*
  report zty_show_html.
  
  define append_events.
    clear ls_event.
    ls_event-eventid = &1.
    ls_event-appl_event = 'X'.
    append ls_event to lt_events.
  end-of-definition.
  
  class cl_report definition deferred.
  
  data: go_report type ref to cl_report.
  
  class cl_report definition.
    public section.
  
      class-methods: class_constructor.
  
      class-methods: factory
        importing
                  iv_url          type string optional
        returning value(eo_value) type ref to cl_report.
  
      methods constructor
        importing
          iv_url type string optional.
  
      methods: html_init.
  
      methods: set_events.
  
      methods: set_handle.
  
      methods: call_screen.
  
      methods:
        handle_sapevent for event sapevent of cl_gui_html_viewer
          importing
            action
            frame
            getdata
            postdata
            query_table.
  
      methods:
        get_url
          returning value(ev_result) type string,
        set_url
          importing
            iv_url type string.
  
  
  
    private section.
      data: gv_url         type string value 'http://143.64.176.239:18080/#/JobCardTemplate/EngineRepairCardEdit?type=1',
            go_html_viewer type ref to cl_gui_html_viewer.
  
  endclass.
  
  
  start-of-selection.
    go_report = cl_report=>factory(  ).
    go_report->call_screen( ).
  
  module status_9000 output.
    go_report->html_init( ).
  endmodule.
  
  module user_command_9000 input.
  
    case sy-ucomm.
      when 'BACK'.
        leave program.
    endcase.
  
  endmodule.
  
  class cl_report implementation.
  
    method class_constructor.
  
    endmethod.
  
    method constructor.
      if iv_url is not initial.
          gv_url = iv_url.
      endif.
    endmethod.
  
    method factory.
      eo_value = new cl_report( ).
    endmethod.
  
    method call_screen.
      call screen 9000.
    endmethod.
  
    method html_init.
  
      data: lv_url type c length 1000.
  
      lv_url = gv_url.
  
      if go_html_viewer is initial.
  
        go_html_viewer = new cl_gui_html_viewer( parent = cl_gui_container=>default_screen ).
  
        set_events( ).
  
        set_handle( ).
  
        go_html_viewer->show_url( lv_url ).
  
      endif.
  
    endmethod.
  
    method set_events.
  
      data:lt_events type cntl_simple_events,
           ls_event  like line of lt_events.
  
      append_events: cl_gui_html_viewer=>m_id_sapevent.
  
      go_html_viewer->set_registered_events( events = lt_events ).
  
    endmethod.
  
    method set_handle.
      set handler: handle_sapevent for go_html_viewer.
    endmethod.
  
    method handle_sapevent.
  
  *    data:lv_str type string.
  *    data:lt_postdata type cnht_post_data_tab.
  *    data:ls_postdata like line of lt_postdata.
  *    data:lt_edquery type cnht_query_table.
  *    data:ls_edquery like line of lt_edquery.
  *    lv_str = 'action:' && action
  *      && ';frame' && frame
  *      && ';getdata' && getdata.
  *    lt_postdata = postdata.
  *    lt_edquery = query_table.
  *
  *    if lt_postdata is not initial.
  *      read table lt_postdata into ls_postdata index 1.
  *    endif.
  *
  *    lv_str = lv_str && ';' && ls_postdata.
  *
  *    loop at lt_edquery into ls_edquery.
  *      lv_str = lv_str && ';name='
  *        && ls_edquery-name
  *        && '-'
  *        && ls_edquery-value.
  *    endloop.
  *
  *    message lv_str type 'I'.
  
    endmethod.
  
    method get_url.
      ev_result = me->gv_url.
    endmethod.
  
    method set_url.
      me->gv_url = iv_url.
    endmethod.
  
  endclass.
  ```

  

