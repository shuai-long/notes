更多内容请 [点击](https://blog.csdn.net/HAND_YS/article/details/122874066) 

## `OXK3` 增加字段

T-code：`OXK3`

> 最多可激活18个字段，每个字段的长度不得长于22，自定义字段必须以"ZZ"或 “YY” 开头

- 选择添加字段

  ![添加字段-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_d3F5LXplbmhlaQ%2Cshadow_50%2Ctext_Q1NETiBAWWlzb29u%2Csize_20%2Ccolor_FFFFFF%2Ct_70%2Cg_se%2Cx_16.png)

- 输入字段的技术属性；名称、描述、数据类型、字段长度

  ![添加字段-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_d3F5LXplbmhlaQ%2Cshadow_50%2Ctext_Q1NETiBAWWlzb29u%2Csize_20%2Ccolor_FFFFFF%2Ct_70%2Cg_se%2Cx_16-20240520193745525.png)

- 点击执行，可以勾选测试运行，以检查是否满足执行条件

  ![添加字段-3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1d51b6ad98b94390a1b8efd3238a96d7.png)

- 测试运行结果，警告消息中会有激活步骤完成后需要对MSEG视图需要特殊处理的 Note

  ![添加字段-4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_d3F5LXplbmhlaQ%2Cshadow_50%2Ctext_Q1NETiBAWWlzb29u%2Csize_20%2Ccolor_FFFFFF%2Ct_70%2Cg_se%2Cx_16-20240520194212960.png)

- 测试执行如果无错误，则正式执行，激活过程由于修改了大量标准表和结构，同时相应的程序也需要同步调整，此步骤会将激活包入请求中，并且持续半个小时到1个小时，前台业务操作将会ABAP DUMP而受到影响，会出现耐心等待别让你的系统掉线，如果掉线，则根据ABAP错误使用SE11直接去激活ABAP对象，

- 最后系统会自动生成9999屏幕，用于凭证输入时输入自定义字段的内容。9999 屏幕为编码块屏幕。

  ![添加字段-5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/watermark%2Ctype_d3F5LXplbmhlaQ%2Cshadow_50%2Ctext_Q1NETiBAWWlzb29u%2Csize_20%2Ccolor_FFFFFF%2Ct_70%2Cg_se%2Cx_16-20240520194335983.png)

- 切换专家模式

  ![添加字段-6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/a7714606c76433f604640e7c067064f7.png)

- 添加字段到结构 `CI_COBL` 

  ![添加字段-7](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/e4afbd386313cd1db396c9740cc08527.png)

- 需要添加的字段，字段长度可以超过22位，最后激活该结构

  ![添加字段-8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/2053108eb1a2957f124aea2c2849bcce.png)

- 添加字段到结构  `CI_COBL_BI`

  ![添加字段-9](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/a72366b415f0f935e738f8bb25582551.png)

- 保持 `CI_COBL_BI` 跟结构 `CI_COBL` 一致，最后激活

  ![添加字段-10](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/763f95f2bbcc714f59eb9064799f42cd.png)

## **新建 `MSEG` 的 CDS 视图**

无论是使用普通模式还是专家模式，都需要调整MSEG的CDS视图，在Eclipse里操作步骤如下：

1. 选择新建类型

   ![新建 CDS 视图-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/7360841139b9909fa4106996e0ea330a.png)

2. 选择  `Data Definition`

   ![新建 CDS 视图-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/823f738c0e0d6ee7afb5a687b9626063.png)

3. 选择 包，输入 名称 和 描述，

   ![新建 CDS 视图-3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/309b9951a1dfc8b0571ab457b3198224.png)

4. 完成代码，中括号中的字段为实际新增的字段

   ```abap
   @AbapCatalog.sqlViewAppendName: ‘Z_CL_COBL’
   @EndUserText.label: ‘Extension view for Append CI_COBL’
   extend view nsdm_e_mseg with Z_CL_COBL_TEMP {
   	cast(’’ as zz001 ) as ZZ001,
       cast(’’ as zz002 ) as ZZ002,
       cast(’’ as zz003 ) as ZZ003,
       cast(’’ as zz004 ) as ZZ004,
       cast(’’ as zz005 ) as ZZ005,
       cast(’’ as zz006 ) as ZZ006,
       cast(’’ as zz007 ) as ZZ007,
       cast(’’ as zz008 ) as ZZ008,
       cast(’’ as zz009 ) as ZZ009,
       cast(’’ as zz010 ) as ZZ010,
   }
   ```

## 添加字段到屏幕

​	需要注意的是：这种 Coding Block 实现的方式只能应用到总账凭证上，应收应付不适用。因为 SAP 的 Coding Block 功能并没有覆盖 FI 的全部模块，在应收应付帐和预制凭证中需要拷贝标准屏幕并手工添加 Coding Block 字段以满足业务需求。

- `SAPMF05A` ：用户总帐屏幕一般过帐创建及修改，客户屏幕 301 -> 9901； 供应商屏幕 302 -> 9902 ；特殊分类帐屏幕 303 -> 9903
- `SAPMF05L`：用户总帐屏幕一般过帐显示，客户屏幕 301 -> 9901； 供应商屏幕 302 -> 9902 ；特殊分类帐屏幕 303 -> 9903
- `SAPLF040`：预制凭证创建/修改/显示，客户屏幕 301 -> 9901； 供应商屏幕 302 -> 9902 ；特殊分类帐屏幕 303 -> 9903

例如： 复制 303 -> 9903

1. 复制屏幕

   ![复制屏幕-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/b7005509f6c26402521a9c2764230439.png)

2. 点击布局，添加自定义字段到复制的屏幕中

   ![复制屏幕-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/d699f483de2d1d438168710ee9612bbc.png)

3. 选择添加自定义字段

   ![复制屏幕-3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/d4232054288ce3b10084fce937b4c91a.png)

4. 如果需要控制修改凭证时，前台该自定义字段可以编辑，则修改屏幕字段属性

   ![复制屏幕-4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/a6b8bafb7799ff8c33843047e732f5c4.png)

5. 包含文件 `ZMF05A_FRM_MODIFY_SCREEN001`， 代码如下：

   ```abap
   module frm_modify_screen output.
   	if sy-tcode eq 'FB02'.
   		loop at screen.
   			if screen-name+0(7) eq 'BSEG-ZZ' or screen-name+0(7) eq 'BSEG-YY'.
   				screen-input = '1'.
   				modify screen.
   			endif.
   		endloop.
   	endif.
   endmodule.
   ```

   ![复制屏幕-5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/d86926e2d14b88d6dd94fbf8327cc373.png)

6. 将拷贝出来的屏幕替换标准屏幕，标准表 `T019`，将标准屏幕 301 或者 302 或者 303 对应的条目里，将屏幕号改成自定义屏幕，或新增配置

   - 客户配置如下：

     ![替换屏幕-6.1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/9ca976829f6be03127760eb0eecf0166.png)

   - 供应商配置如下：

     ![复制屏幕-6.2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/43f5bbdff9b6ea899afc32e47c73d7ec.png)

   - 特殊分类账配置如下：

     ![复制屏幕-6.3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/2f49b8b9d9c037caefad63a4080b3a38.png)

## 维护字段状态

该配置决定了自定义字段是否在编码块中隐藏、必输属性。

T-Code：`OBC4`

1. 维护字段状态变式：

   ![维护字段状态-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/a5ad14ce86a6ae7b44e802a0cb97ff5d.png)

2. 选择变式维护字段是否隐藏或可选。

   ![维护字段状态-2.1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/6715c767fd184b3cb3854e55a846d597.png)

   ![维护字段状态-2.2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/90837397080a79c0ab75d7b5a12f3f40.png)

   ![维护字段状态-2.3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/82ba984382c45355247e47ef164745f1.png)

## 添加搜索帮助

由于是自定义字段，不能使用标准搜索帮助，故需要自建搜索帮助，步骤如下：

1. 代码 `SE11`，输入搜索帮助名称，ZH+字段名

   ![添加搜索帮助-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/3eee04372e2e013a7a5a258423d59ecc.png)

2. 事务代码SE37，复制标准函数 `F4IF_SHLP_EXIT_EXAMPLE` 到 `ZFMFI_ZZ004_SHLP_EXIT`，

   ![添加搜索帮助-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/ce13e426f5b819db68cc212357a9fff3.png)

3. 代码如下：

   ```ABAP
   RANGES:lr_zz004 FOR cobl-zz004.
   RANGES:lr_zz004_txt FOR kna1-name1.
   
   LOOP AT shlp-selopt INTO DATA(ls_selopt).
       IF ls_selopt-shlpfield = ‘ZZ004’.
       	APPEND VALUE #( sign = ls_selopt-sign 
       					option = ls_selopt-option 
       					low = ls_selopt-low 
       					high = ls_selopt-high ) TO lr_zz004.
       ELSEIF ls_selopt-shlpfield = ‘ZZ004_TXT’.
       	APPEND VALUE #( sign = ls_selopt-sign 
       					option = ls_selopt-option 
       					low = ls_selopt-low 
       					high = ls_selopt-high ) TO lr_zz004_txt.
       ENDIF.
   ENDLOOP.
   SELECT
     kunnr AS zz004,name1 AS zz004_txt
     FROM kna1
     WHERE kunnr IN @lr_zz004
      AND  name1 IN @lr_zz004_txt
     INTO TABLE @DATA(lt_kna1).
   
   "也可以使用系统标准函数F4UT_RESULTS_MAP将源数据绑定到recordtab
   CALL FUNCTION 'F4UT_RESULTS_MAP'
       TABLES
         shlp_tab    = shlp_tab
         record_tab  = record_tab
         source_tab  = lt_kna1
       CHANGING
         shlp        = shlp
         callcontrol = callcontrol.
   ```

   ![添加搜索帮助-3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/8cb2476483042e97959f5e39dbbff366.png)

4. 添加搜索帮助到 `COBL` 结构。

   ![添加搜索帮助-4.1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/e2fb0de9b76f5aff327d48f417e18b98.png)

   ![添加搜索帮助-4.2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/2c87663e5cc8c1d81e6379bf77314b07.png)

5. 激活该结构，用同样的方法在结构 `CI_COBL_BI` 里对自定义字段加搜索帮助。时间较长，耐心等待，添加完之后，编码块屏幕的自定义字段会有搜索帮助。

   ![添加搜索帮助-5.1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/8dd578685736d97ad2338f714cda7ec6.png)

   > **注：**应收应付是复制标准屏幕到自定义屏幕，例如复制标准屏幕 303 到 9303 或者 9903，打开该屏幕，添加搜索帮助
   >
   > ![添加搜索帮助-5.2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/418836c78be5ac1048cc5f4c8294f2cf.png)