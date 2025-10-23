## 薪酬Schema增强

1. 查找国家对应的程序(`pcburz*`)，例如中国增强程序`pcburzcn0`，创建隐式增强，新增 Include 程序

   ![image-20250825153449835](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20250825153449835.png)

2. 在 Include 中新建一个前缀为`fu`且后缀由`z_`或`y_`开始的子例程，

   ```abap
   form fuz_pay.
   endform.
   ```

3. 使用TCODE `PE04`进行配置，名称为Include程序中子例程的后缀![img](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_ZmFuZ3poZW5naGVpdGk%2Cshadow_10%2Ctext_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2h1YW5nbGluNg%3D%3D%2Csize_16%2Ccolor_FFFFFF%2Ct_70.png )

4. 选择对应的国家![img](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_ZmFuZ3poZW5naGVpdGk%2Cshadow_10%2Ctext_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2h1YW5nbGluNg%3D%3D%2Csize_16%2Ccolor_FFFFFF%2Ct_70-20250825164800518.png)

5. 设置输入/输出参数![img](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_ZmFuZ3poZW5naGVpdGk%2Cshadow_10%2Ctext_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2h1YW5nbGluNg%3D%3D%2Csize_16%2Ccolor_FFFFFF%2Ct_70-20250825165206314.png)![img](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_ZmFuZ3poZW5naGVpdGk%2Cshadow_10%2Ctext_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2h1YW5nbGluNg%3D%3D%2Csize_16%2Ccolor_FFFFFF%2Ct_70-20250825165217136.png)

6. 使用TCODE `PE01` 将 `PE04` 中的名称配置即可![img](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_ZmFuZ3poZW5naGVpdGk%2Cshadow_10%2Ctext_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2h1YW5nbGluNg%3D%3D%2Csize_16%2Ccolor_FFFFFF%2Ct_70-20250825165355559.png)

## 薪酬Schema报错增强

- 新增函数

  ```abap
  form fu_fu03.
  
    break itl013.
  
    data(lt_error_text) = error_ptext[].
    refresh lt_error_text.
    read table it into data(ls_it) with key lgart = '5410'.
    if sy-subrc = 0 .
      "--------------------> 添加异常消息
      append initial line to lt_error_text assigning field-symbol(<fs_error_text>).
      <fs_error_text>-tlevel      = '1'.
      <fs_error_text>-text1       = '存在考勤异常信息'.
      <fs_error_text>-tlength1    = strlen( <fs_error_text>-text1 ).
      <fs_error_text>-text2       = ''.
      <fs_error_text>-tlength2    = strlen( <fs_error_text>-text2 ).
      <fs_error_text>-tintensiv1  = '0'.
  
      "--------------------> 处理异常消息长度
      perform alter_tlength tables lt_error_text.
  
      "--------------------> 将异常消息添加到异常表中
      append lines of lt_error_text to error_ptext[].
  
      "--------------------> 报错
      perform errors tables lt_error_text.
    endif.
  
  
  endform.
  ```

- 处理异常消息长度函数

  ```abap
  form alter_tlength tables it_ptext structure ptext.
  
    data: ls_ptext type thrpl_ptext with header line,
          lv_max_length1 type i value 0,
          lv_max_length2 type i value 0,
          lv_length      type i,
          lv_length_vis  type i,
          lv_length_tech type i.
  
    loop at it_ptext into ls_ptext.
      if ls_ptext-text2 is initial.
        call method cl_scp_linebreak_util=>string_split_at_position
          exporting
            im_string                 = ls_ptext-text1
            im_pos_vis                = 132
          importing
            ex_pos_vis                = lv_length_vis
            ex_pos_tech               = lv_length_tech
          exceptions
            pos_not_valid             = 1
            unsupported_boundary_kind = 2
            invalid_text_enviroment   = 3
            others                    = 4.
        if lv_length_tech > lv_length_vis. "calculate max length of text
          lv_length = lv_length_tech.
        else.
          lv_length = lv_length_vis.
        endif.
        if lv_max_length1 < lv_length.
          lv_max_length1 = lv_length.
        endif.
      else.
        call method cl_scp_linebreak_util=>string_split_at_position
          exporting
            im_string                 = ls_ptext-text1
            im_pos_vis                = 132
          importing
            ex_pos_vis                = lv_length_vis
            ex_pos_tech               = lv_length_tech
          exceptions
            pos_not_valid             = 1
            unsupported_boundary_kind = 2
            invalid_text_enviroment   = 3
            others                    = 4.
        if lv_length_tech > lv_length_vis. "calculate max length of text
          lv_length = lv_length_tech.
        else.
          lv_length = lv_length_vis.
        endif.
        if lv_max_length2 < lv_length.
          lv_max_length2 = lv_length.
        endif.
      endif.
  
    endloop.
    loop at it_ptext into ls_ptext.
      if ls_ptext-text2 is initial.
        ls_ptext-tlength1 = lv_max_length1.
      else.
        ls_ptext-tlength1 = lv_max_length2.
      endif.
      modify it_ptext from ls_ptext index sy-tabix.
    endloop.
  endform.
  ```

## 考勤Schema增强

- 考勤管理的国家分配需要勾选所有的国家代码

  ![国家分配](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011090320384.png)

- 对于薪酬部分，不能直接使用INCLUDE `RPCFDCZ0`，要使用`PCFDCZ**0`，`**`为国家代码，如中国就使用CN；对于考勤部分预留自定义程序写在`RPTMOZ00`这个程序里。

  ![image-20251011091009958](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091009958.png)![image-20251011091028303](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091028303.png)

- 代码完成后如果不能找到程序被调用，则需要通过运行程序`RPUCT300`来更新配置表

  ![image-20251011091505616](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091505616.png)

  