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


## 考勤Schema增强

- 考勤管理的国家分配需要勾选所有的国家代码

  ![国家分配](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011090320384.png)

- 对于薪酬部分，不能直接使用INCLUDE `RPCFDCZ0`，要使用`PCFDCZ**0`，`**`为国家代码，如中国就使用CN；对于考勤部分预留自定义程序写在`RPTMOZ00`这个程序里。

  ![image-20251011091009958](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091009958.png)![image-20251011091028303](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091028303.png)

- 代码完成后如果不能找到程序被调用，则需要通过运行程序`RPUCT300`来更新配置表

  ![image-20251011091505616](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20251011091505616.png)

  