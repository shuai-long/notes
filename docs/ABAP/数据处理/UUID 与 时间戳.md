[TOC]

# UUID 与 时间戳 #

## UUID ##

UUID含义是通用唯一识别码 (Universally Unique Identifier)，通俗一点就是一个不重复的字符串，有时会把UUID作为数据库的一个主键，这样就不用担心主键重复的问题。

SAP中有个专门生成 UUID 的类`CL_SYSTEM_UUID`，提供了4个静态方法创建生成不同类型的UUID：2进制，16进制，64位以及32位uuid。也可以进行不同类型UUID之间的相互转换。

- UUID 获取

  ```abap
  DATA:l_uuid22 TYPE sysuuid_c22,
       l_uuid32 TYPE sysuuid_c32,
       l_uuid26 TYPE sysuuid_c26,
       l_uuid16 TYPE sysuuid_x16.
   
  TRY.
      CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_x16
        RECEIVING
          uuid = l_uuid16.
    CATCH cx_uuid_error .
  ENDTRY.
  TRY.
      CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c22
        RECEIVING
          uuid = l_uuid22.
    CATCH cx_uuid_error .
  ENDTRY.
  TRY.
      CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
        RECEIVING
          uuid = l_uuid32.
    CATCH cx_uuid_error .
  ENDTRY.
  TRY.
      CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c26
        RECEIVING
          uuid = l_uuid26.
    CATCH cx_uuid_error .
  ENDTRY.
   
  WRITE:  `Binary Format UUID:`,l_uuid16.
  WRITE:/ `Base64 UUID:       `,l_uuid22.
  WRITE:/ `Base32 UUID:       `,l_uuid26.
  WRITE:/ `Hex Format UUID:   `,l_uuid32.
  
  ```

## 时间戳 ##

UTC（UTC, Universal Time Coordinated，通用协调时）时间戳，分为长时间戳和段时间戳，其中长时间戳参考的系统的数据元素`TIMESTAMPL`，类型为DEC（21，7）；而短时间戳参考的系统数据元素为`TIMESTAMP`，类型为DEC（15，0）。下文中如果没有特指，一般都指的短时间戳。

1. **获取当前时间戳**

   ```abap
   "GET TIME STAMP FIELD timestamp. 获取当前时间戳(这里的时间戳可以是长类型,也可以是短类型)
   ```

   调用示范:

   ```abap
    GET TIIME STAMP FIELD lv_timestamp.
   ```

2. **使用`CONVERT DATE`生成指定时间的时间戳**

   ```abap
   "CONVERT DATE date [TIME time [DAYLIGHT SAVING TIME dst] ] INTO TIME STAMP time_stamp TIME ZONE tz.
   "根据指定的日期、时间（可选是否为夏令时）和时区生成时间戳，其中[]圈起来的参数为可选参数，参数解释如下：
       "date:        指定的日期（DATS类型）
       "time:        指定的时间（TIMS类型）
       "dst:         夏令时标志(CHAR1类型)
       "time_stamp:  时间戳 DEC（15，0)类型
       "tz:          时区（CHAR06类型）
   "系统预留（系统结构为SYST，程序默认结构为SY）的时间变量有：
   	"SY-DATUM：应用服务器日期；
   	"SY-UZEIT：应用服务器时间；
   	"SY-TZONE：应用服务器时区(INT4)；
   	"SY-DAYST：夏令时；
   	"SY-DATLO：用户本地日期；
   	"SY-TIMLO：用户本地时间；
   	"SY-ZONLO：用户本地时区(CHAR6)。
   ```

   在实际测试过程中，即使手动调整本地电脑的时间为费正确的时间，发现系统用户本地时间和服务器时间对应的系统变量的值是一样的（正确的时间值），并不是真正的本地时间，可能是因为时区相同，没有测出差别。调用示范：

   ```abap
   CONVERT DATE SY-DATLO TIME SY-TIMLO INTO TIME STAMP DATA(LV_TIMESTAMP) TIME ZONE SY-ZONLO.
   ```

3. **使用系统函数来进行时间戳来与日期和时间进行转换**

   系统中有大量的根据日期、时间、时区与时间戳进行互转的函数，只需要在SE37下模糊匹配“*TIMESTAMP*”就可以查到大量的有用函数，实际上，通过查看这些函数的代码可以发现，其实本质最核心的代码还是调用`CONVERT` 语句,因此只需要掌握`CONVRT`语句即可快速互转时间戳与日期时间信息。以下为常见的几个函数：

   - 根据制定的日期、时间、时区生成时间戳：`IB_CONVERT_INTO_TIMESTAMP`
   - 根据制定的时间戳和时区转化成日期、时间：`IB_CONVERT_FROM_TIMESTAMP`
   - 把指定区域的时间戳转化成日期和时间：`LTRM_TIMESTAMP_CONVERT_FROM`
   - 把指定区域的日期和时间转化成时间戳：`LTRM_TIMESTAMP_CONVERT_INTO`
   - 把指定的日期和时间转化成时间戳（默认本地时区）：`ABI_TIMESTAMP_CONVERT_FROM`
   - 把指定的时间戳转化成日期和时间（默认本地时区）：`ABI_TIMESTAMP_CONVERT_INTO`
   - 获取指定的远程系统的时间戳：`RSWR_TIMESTAMP_GET`

   ```abap
   DATA: LV_TIMESTAMP TYPE TIMESTAMP.
   CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
     　EXPORTING
        I_DATLO     = SY-DATLO
        I_TIMLO     = SY-TIMLO
        I_TZONE     = SY-ZONLO
      IMPORTING
        E_TIMESTAMP = LV_TIMESTAMP.
   ```

4. **两个时间戳之间的秒数**

   JAVA 的时间戳是 UNIX 时间戳,是以 1970年1月1日 开始计算的秒数,而不是以零时区开始计算的,所以北京时间 1970年1月1日8点,对应SAP时间戳为==19700101000000==(转换了时区后的),对应JAVA的是==0==…

   ```abap
   DATA: time           TYPE tzntstmpl,
         secends_s      TYPE char20,
         timestamp_s    TYPE string,
         curr_timestamp TYPE p,
         timestamp1970  TYPE p,
         unix_timestamp TYPE char25,
         temp           TYPE char3.
   
   GET TIME STAMP FIELD time.
   curr_timestamp = time.
   timestamp1970 = '19700101000000'.
   
   TRY.
       secends = cl_abap_tstmp=>subtract( tstmp1 = curr_timestamp tstmp2 = timestamp1970 ).
     CATCH cx_parameter_invalid_range.
     CATCH cx_parameter_invalid_type.
   ENDTRY.
   
   secends_s = secends.
   SHIFT secends_s BY 9 PLACES RIGHT.
   CONDENSE secends_s.
   ```

   