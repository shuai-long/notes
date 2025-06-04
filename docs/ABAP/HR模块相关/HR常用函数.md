## HR常用BAPI

后续可以使用相关类 `CL_HRBAS*`,待研究

## 结构化授权读取 ##

```abap
"获取结构化的组织架构
DATA: LD_SELECTED_PLVAR	     TYPE OBJEC-PLVAR,
      LD_ACT_SEARCH_OTYPE	   TYPE OBJEC-OTYPE,
      IT_SELECTED_OBJECTS	   TYPE STANDARD TABLE OF HRSOBID,
      WA_SELECTED_OBJECTS	   LIKE LINE OF IT_SELECTED_OBJECTS,
      LD_SELECTED_OTYPE	     TYPE OBJEC-OTYPE,
      LD_ACT_SEARCH_WEGID	   TYPE GDSTR-WEGID,
      LD_SELECTED_OBJID	     TYPE OBJEC-REALO,
      LD_ACT_SEARCH_SVECT	   TYPE GDSTR-SVECT,
      LD_CHANGED_FLAG	       TYPE HRPP0C-TEST,
      LD_SET_MODE	           TYPE OBJEC-HISTO,
      LD_LAST_OK_CODE	       TYPE T77FC-FCODE,
      LD_ACT_ROOT_OT         TYPE OBJEC-OTYPE,
      LD_ACT_ROOT_ID         TYPE STRING,
      LD_ACT_PLVAR           TYPE OBJEC-PLVAR,
      LD_ACT_SEARCH_BEGDA	   TYPE OBJEC-BEGDA,
      LD_ACT_SEARCH_ENDDA	   TYPE OBJEC-ENDDA,
      LD_NO_SEARK	           TYPE OBJEC-HISTO,
      LD_ACT_LIST_TYPE       TYPE STRING,
      LD_ACT_INT_WEGID       TYPE HRRHAS-77AW_INT,
      LD_SELECTED_OBJ_APPEND TYPE HRPP0C-TEST,
      LD_CHANGE_SEARCH_TYPE	 TYPE HRPP0C-TEST,
      LD_RESTRICT_CALLBACK   TYPE TFDIR-FUNCNAME.

CALL FUNCTION 'RH_TYPE_STRUC_HELP'
  EXPORTING
    ACT_SEARCH_OTYPE         = LD_ACT_SEARCH_OTYPE
*   act_search_wegid         = ld_act_search_wegid
*   act_search_svect         = ld_act_search_svect
*   set_mode                 = ld_set_mode
*   act_root_ot              = ld_act_root_ot
*   act_root_id              = ld_act_root_id
*   act_plvar                = ld_act_plvar
*   act_search_begda         = ld_act_search_begda
*   act_search_endda         = ld_act_search_endda
*   no_seark                 = ld_no_seark
*   act_list_type            = ld_act_list_type
*   act_int_wegid            = ld_act_int_wegid
*   selected_obj_append      = ld_selected_obj_append
*   change_search_type       = ld_change_search_type
*   restrict_callback        = ld_restrict_callback
  IMPORTING
    SELECTED_PLVAR           = LD_SELECTED_PLVAR
    SELECTED_OTYPE           = LD_SELECTED_OTYPE
    SELECTED_OBJID           = LD_SELECTED_OBJID
    CHANGED_FLAG             = LD_CHANGED_FLAG
    LAST_OK_CODE             = LD_LAST_OK_CODE
* TABLES
*   selected_objects         = it_selected_objects
  EXCEPTIONS
    NO_ACTIVE_PLVAR          = 1
    NO_OBJECT_SELECTED       = 2
    NO_STRUC_SEARCH_POSSIBLE = 3.
```

## 读取0041日期

```abap
methods get_date_for_it0041
  importing
            iv_datar         type any
            is_data          type any
  returning value(rv_result) type dats.
```

```abap
method get_date_for_it0041.

  data: lo_message_handler type ref to if_hrpa_message_handler,
        ls_p0041           type p0041,
        lv_datar           type datar,
        lv_date            type dats.

  move-corresponding is_data to ls_p0041.
  lv_datar = iv_datar.

  call function 'HR_ECM_READ_IT0041_DATE_TYPE'
    exporting
      datar           = lv_datar
      p0041           = ls_p0041
      message_handler = lo_message_handler
    importing
      date            = lv_date.

  rv_result = lv_date.

endmethod.
```





