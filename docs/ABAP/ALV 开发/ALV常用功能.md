## ALV复选框

- 获取对象定义

  <!-- tabs:start -->

  <!-- tab:OO ALV -->

  ```abap
  data lo_alv_grid type ref to cl_gui_alv_grid.
  ```

  <!-- tab:Function ALV -->

  ```abap
  data lo_alv_grid type ref to cl_gui_alv_grid.
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lo_alv_grid.
  ```

  <!-- tabs:end -->

- 实现

  <!-- tabs:start -->

  #### **ALV自带复选框**

  - 获取选择行

    ```abap
    data lt_rows type lvc_t_row.
    lo_alv_grid->get_selected_rows(
      importing
        et_index_rows = lt_rows ).
    ```

  #### **内表定义字段**

  - 检查数据更改

    ```abap
    lo_alv_grid->check_changed_data( ).
    ```

  - 获取过滤掉的行

    <!-- tabs:start -->

    #### **方法一**

    ```abap
    lo_alv_grid->get_filtered_entries(
      importing
        et_filtered_entries = data(lt_filtered_entries)
    ).
    ```

    #### **方法二**

    ```abap
    data: lt_filtered_entries type slis_t_filtered_entries. 
    call function 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
      importing
        et_filtered_entries = lt_filtered_entries
      exceptions
        no_infos            = 1
        program_error       = 2
        others              = 3.
    ```

    <!-- tabs:end -->

  - 全选与取消全选

    <!-- tabs:start -->

    #### **全选**

    ```abap
    "--------------------> 全选
    sort lt_rows.
    loop at gt_alv assigning field-symbol(<fs_alv>).
      read table lt_filtered_entries transporting no fields with key table_line = sy-tabix binary search.
      if sy-subrc ne 0.
        <fs_alv>-checkbox = 'X'.
       else.
        <fs_alv>-checkbox = ' '.
      endif.
    endloop.
    ```

    #### **取消全选**

    ```abap
    "--------------------> 取消全选
    sort lt_rows.
    loop at gt_alv assigning field-symbol(<fs_alv>).
      read table lt_filtered_entries transporting no fields with key table_line = sy-tabix binary search.
      if sy-subrc ne 0.
        <fs_alv>-checkbox = ' '.
       else.
        <fs_alv>-checkbox = 'X'.
      endif.
    endloop.
    ```

    <!-- tabs:end -->

  - 刷新alv数据

    ```abap
    data ls_stable type lvc_s_stbl value 'XX'.
    lo_alv_grid->refresh_table_display( is_stable = ls_stable ).
    ```

  <!-- tabs:end -->

## ALV按钮

<!-- tabs:start -->

<!-- tab:ALV按钮文本切换 -->

1. 程序中定义一个全局变量，并初始化全局变量，类型是`smp_dyntxt`。

   ```abap
   data: gs_btn_change type smp_dyntxt.
   
   gs_btn_change = value #(
   	icon_id   = icon_display
   	text      = 'Display'
   	icon_text = 'Display'
   ).
   ```

1. 在GUI状态中定义动态文本的按钮![创建按钮](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250507200645006.png)

   输入字段名称（此处可以是表字段，也可以是程序变量）![输入字段名称](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250507201049354.png)

<!-- tab:ALV下拉按钮 -->

- 定义方法

  ```abap
  "--------------------> 初始化ALV工具栏对象事件，如增加按钮并设定属性
  methods handle_toolbar for event toolbar of cl_gui_alv_grid
    importing
      e_object
      e_interactive.
      
  "--------------------> 该事件用于在下ALV工具栏的下拉菜单按钮中增加选项
  methods handle_menu_button for event menu_button of cl_gui_alv_grid
    importing
      e_object
      e_ucomm.
  ```

- 实现方法

  ```abap
  method handle_toolbar.
  
    e_object->mt_toolbar = value #( base mt_toolbar
                                    ( function = 'BTN_LIST'  quickinfo = '下拉按钮,按钮类型为1' butn_type = '1' )
                                    ( function = 'BTN_STAND' quickinfo = '标准按钮,按钮类型为0' butn_type = '0' ) ).
  endmethod.
  
  method handle_menu_button.
  
    case e_ucomm.
      when 'BTN_LIST'.
      "--------------------> 为下拉按钮添加子按钮
      e_object->add_function( fcode = 'BTN_SON1' text = '子按钮1' ).
  
    endcase.
    
  endmethod.
  ```

<!-- tabs:end -->

## ALV颜色设置

![alv常用颜色](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20220901165722040.png)

<!-- tabs:start -->

<!-- tab:行颜色 -->

1. 在构成ALV数据的内表中，添加一个CHAR4类型的字段

   ```abap
   types: begin of ty_alv,
            row_color type char4, 
          end of ty_alv.
   data: gt_alv type table of ty_alv.
   ```

2. 在layout中指定颜色字段: 

   `slis_layout_alv`结构字段为`info_fieldname`

   `lvc_s_layo`结构字段为`info_fname`

   ```abap
   gs_layo-info_fname = 'ROW_COLOR'.
   ```

3. 在内表中为颜色赋值即可

   ```abap
   loop at gt_alv assigning field-symbol(<fs_alv>).
   	<fs_alv>-row_color = 'C610'
   endloop.
   ```

<!-- tab:列颜色 -->

在构成ALV字段的FIELDCAT内表字段`emphasize`赋值即可.

```abap
ls_fcat-emphasize = 'C610'.
```

<!-- tab:单元格颜色 -->

1. 在构成ALV数据的内表中，添加一个`lvc_t_scol`类型的字段，结构如下：

   - `fname`: 内表字段名称

   - `color`: 颜色代码，包含以下字段
     - `col`: 颜色
     - `int`: 强化 
     - `inv`: 相反，值范围：1/0 设置前景或者背景

   - `nokeycol`: 覆盖码颜色

   ```abap
   types: begin of ty_alv,
            cell_color type lvc_t_scol, 
          end of ty_alv.
   data: gt_alv type table of ty_alv.
   ```

2. 在layout中指定颜色字段: 

   `slis_layout_alv`结构字段为`coltab_fieldname`

   `lvc_s_layo`结构字段为`ctab_fname`

   ```abap
   gs_layo-ctab_fname = 'CELL_COLOR'.
   ```

3. 在内表中设置字段颜色。

   ```abap
   loop at gt_alv assigning field-symbol(<fs_alv>).
   	<fs_alv>-cell_color = value #( ( fname = 'BUKRS' color = value #( col = '6' int = '1' inv = '0' ) ) ).
   endloop.
   ```

<!-- tabs:end -->

## 禁止删除新增行

```abap
gs_layo-no_rowins = 'X'. "禁止新增行
gs_layo-no_rowmove = 'X' "禁止删除行
```

## EDIT_MASK

<!-- tabs:start -->

<!-- tab:convexit -->

使用时直接给fieldcat的`convexit`赋值即可：

- 物料：`MATN1`
- 前导零：`ALPHA`
- WBS元素：`ABPSP`
- 项目：`ABPRJ`
- 单位：`CUNIT`

<!-- tab:edit_mask -->

使用时给fieldcat的`edit_mask`赋值即可，例如：`edit_mask='==ZSIGN'`，常用列表如下：

<!-- tabs:start -->

<!-- tab:负号前置 -->

创建`conversion_exit_zsign_output`函数

```abap
function conversion_exit_zsign_output.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  data: lv_output1 type char20,
        lv_output2 type char20,
        lv_outnum  type p length 16 decimals 2.

  if input is not initial.
    lv_outnum = input.
    if input >= 0.
      write lv_outnum to lv_output1.
    else.
      lv_outnum = lv_outnum * ( -1 ).
      write lv_outnum to lv_output1.
      concatenate '-' lv_output1 into lv_output1.
    endif.
  elseif input = '0.00'.
    write lv_outnum to lv_output1.
  endif.

  condense lv_output1 no-gaps.
  write lv_output1 to lv_output2 right-justified.
  output = lv_output2.

endfunction.
```

<!-- tabs:end -->

<!-- tabs:end -->

## ALV下拉框

1. 设置下拉列表并进行注册。

   <!-- tabs:start -->

   <!-- tab:无值转换 -->

   ```abap
   data gt_drop type lvc_t_drop.
   
   gt_drop = value #( handle = '1' ( value = '1-企业' )
                                   ( value = '2-事业' )
                      handle = '2' ( value = '1-是' )
                                   ( value = '2-否' )
   ).
   
   go_alv_grid->set_drop_down_table( it_drop_down = gt_drop ).
   ```

   <!-- tab:值转换 -->

   ```abap
   data gt_dral type lvc_t_dral.
   
   gt_dral = value #( handle = '1' ( value = '1-企业' int_value = '1' )
                                   ( value = '2-事业' int_value = '2' )
                      handle = '2' ( value = '1-是'  int_value = '1' )
                                   ( value = '2-否'  int_value = '2' )
   ).
   
   go_alv_grid->set_drop_down_table( it_drop_down_alias = gt_dral ).
   ```

   <!-- tabs:end -->

2. 关联下拉字段和下拉列表

   <!-- tabs:start -->

   <!-- tab:单元格下拉框 -->

   1. 在构成alv的内表中添加一个`int4`类型的字段，用以存储下拉框的分组（若一行存在多个下拉框，则定义多个字段即可）：

      ```abap
      types: begin of ty_alv,
               drop_handle type int4,
             end of ty_alv.
      ```

   2. 通过fieldcat的`drdn_field`设置内表的下拉组字段

      <!-- tabs:start -->

      <!-- tab:无值转换 -->

      ```abap
      gs_fcat-drdn_field = 'DROP_HANDLE'.
      ```

      <!-- tab:值转换 -->

      ```abap
      gs_fcat-drdn_field = 'DROP_HANDLE'.
      gs_fcat-drdn_alias = 'X'.
      ```

      <!-- tabs:end -->

   <!-- tab:列下拉框 -->

   1. 通过fieldcat的`drdn_hndl`设置该列取下拉列表的第几组

      <!-- tabs:start -->

      <!-- tab:无值转换 -->

      ```abap
      gs_fcat-drdn_hndl = '1'.
      ```

      <!-- tab:值转换 -->

      ```abap
      gs_fcat-drdn_hndl = '1'.
      gs_fcat-drdn_alias = 'X'.
      ```

      <!-- tabs:end -->

   <!-- tabs:end -->

## ALV搜索帮助

1. fieldcat为字段设置搜索帮助

   ```abap
   gs_fcat-f4availabl = 'X'.
   ```

2. 添加搜索帮助字段并注册

   ```abap
   data: gt_f4 type lvc_t_f4.
   
   gt_f4 = value #(register = 'X' chngeafter = 'X' ( fieldname = to_upper( fieldname1 ) )
   
   go_alv_grid->register_f4_for_fields( gt_f4 ).
   ```

3. 定义方法并注册

   - 定义

     ```abap
     methods handle_onf4 for event onf4 of cl_gui_alv_grid
       importing
         e_fieldname
         es_row_no
         er_event_data
         et_bad_cells
         e_display.
     ```

   - 实现

     ```abap
     method handle_onf4.
     	data: lt_return type table of ddshretval.
     
       field-symbols: <fs_value_list> type table,
                      <fs_modi>       type lvc_t_modi.
     
       read table gt_alv into data(ls_alv) index es_row_no-row_id.
     	
     	"--------------------> 搜索帮助取数逻辑
       case e_fieldname.
         when ''.
           select v~matnr as value, maktx, maktg from mara as v left outer join makt as t
             on v~matnr eq t~matnr and spras eq @sy-langu
             into table @data(lt_makt) order by v~matnr.
           assign lt_makt to <fs_value_list>.
       endcase.
       
       check <fs_value_list> is assigned.
       call function 'F4IF_INT_TABLE_VALUE_REQUEST'
         exporting
           retfield        = 'VALUE'
           value_org       = 'S'
         tables
           value_tab       = <fs_value_list> "lt_vrm_values
           return_tab      = lt_return
         exceptions
           parameter_error = 1
           no_values_found = 2
           others          = 3.
     
       if sy-subrc eq 0 and e_display is initial.
         read table lt_return into data(ls_return) index 1.
         if sy-subrc eq 0.
     
           data(ls_value) = value lvc_s_modi(
               row_id    = es_row_no-row_id
               fieldname = e_fieldname
               value     = ls_return-fieldval
           ).
     
           assign er_event_data->m_data->* to <fs_modi>.
           append ls_value to <fs_modi>.
         endif.
       endif.
     
       er_event_data->m_event_handled = 'X'.
     endmethod.
     ```

   - 注册

     ```abap
     set handler handle_onf4 for go_alv_grid.
     ```

     

   
