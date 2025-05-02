1. 在类方法 CL_COS_UTILITIES->IS_S4H 下图位置中添加代码，代码如下：![image-20240614114734621](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614114734621.png)

   ```abap
   if sy-tcode = 'SMARTFORMS'.
     rv_is_s4h = ''.
     return.
   endif.
   ```

2. 运行程序：RSCPSETEDITOR，取消 SAPscript 和 智能表的勾选![image-20240614115039360](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614115039360.png)