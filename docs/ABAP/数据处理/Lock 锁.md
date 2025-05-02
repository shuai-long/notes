[TOC]

# 锁 #

## Database 锁 ##

与 DB LUW 机制类似，数据库本身一般也提供数据锁定机制。数据库将当前正在执行修改操作的所有数据进行锁定，该锁定将随着数据库的 LUW 的结束而被重置，因而每数据库提交或者回滚之后该锁定就被释放。因为 SAP 的 LUW 概念独立于数据库 LUW 和对话步骤，出于同样的原因，SAP 还需要定义自己的数据锁定机制。

## 逻辑锁 ##

SAP LUW 要求数据库对象的锁定在 SAP LUW 结束释放，并且该数据库锁要求对所有 SAP 程序都可见。 SAP 提供了一个逻辑数据锁定机制，该机制基于系统特定的锁定服务应用服务器中的中心**锁定表（**即将加锁的信息记入数据库表**）**。一个 ABAP 程序在访问数据之前，将希望锁定的数据表关键字发送给该表，因而所有的程序在访问一个数据库表之前必须首先判断该表是否已经被锁定了。

SAP 锁定与数据库物理锁定是不同的，它是一种业务逻辑上的锁定。它不会在物理表上进行加锁，而是将关键字传递加锁函数，加锁函数会在特定表中进加锁信息登记。

SAP LUW 在结束时（提交或回滚），SAP 锁定将会隐式解除。

### 锁对象 ###

在 SE11 里创建锁对象，自定义的锁对象都必须以 EZ 或者 EY 开头来命名。一个锁对象里只包含一个 PRIMARY TABLE，可以包含若干个 SECONDARY TABLE。

在设定一个 SAP 锁定，必须在数据字典中创建一个锁定对象。一或多个数据库表或关键字段可以被指定为锁定对象中的一或多行。激活该对象后，系统自动生成两个功能函数，名称为 ENQUEUE\_<LOCK OBJECT> 和 DENQUEUE_<LOCK OBJECT>，其中\_<LOCK OBJECT> 是锁对象的名称。

具体步骤如下: 

1. SE11 进入, 选择锁对象

   ![创建锁对象-步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/152217403864379.png)

2. 选择锁模式
   **X:** ==扩展排他锁==在同一时间内只能加一次（即使是在同一个事务内），其他所有的加锁请求都会被拒绝。
   **S:** ==共享锁==同一时间内允许多个用户读取同一数据，但是一旦某个用户修改数据后，其它用户将不能再访问该数据。只要是加的共享锁，即使是来自不同用户请求，都是可以加上的（即同一时间内可以允许多个不同的用户加共享锁），但只要是已加了共享锁，其他排它类型的锁不能再加了。
   **E:** ==排他锁==可重入（可重入即可针对同一数据进行多次加锁）类型的排它锁（独占锁）。该类型的锁住的数据在同一时间内，只能被一个用户读取和修改，但同一时间内其他非同一事务内的排他锁或共享锁的加锁请求都会被拒绝，但在同一事务内的 E、S 锁还是可以加的。

   ![创建锁对象-步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/152217412452249.png)

   > **三种锁模式:**
   >
   > * ==**模式 E**==：当更改数据的时候设置为此模式。WriteLock ( exclusive lock )
   >
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 E，其他用户不能再对这个锁对象加 E、X、S 模式的任意一种锁；
   >
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 E，在这个程序，你还可以再对这个锁对象加 E、S 模式的锁，X 模式的不可以。
   >
   > * ==**模式 S**==：本身不需要更改数据，但是希望显示的数据不被别人更改。Read Lock ( Shared Locked )
   >
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 S，其他用户不能再对这个锁对象加 E、X 模式的锁，但是可以加 S 模式的锁；
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 S，在这个程序，你还可以再对这个锁对象加 S 模式的锁，如果没有别的用户对其加 S 模式的锁，那么你还可以对其加 E 模式的锁。X 模式的不可以。
   >
   > * **==模式 X==**：和 E 类似，但是不允许累加，完全独占。Enhancedwrite lock ( exclusive lock without cumulating )
   >
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 X，其他用户不能再对这个锁对象加 E、X、S 模式的任意一种锁；
   >   * 如果你在一个程序里成功对一个锁对象加锁之后，如果模式为 X，在这个程序，你不可以再对这个锁对象加 E、X、S 模式的锁。

3. 选择锁参数: 会自动将表主键加进来

   ![创建锁对象-步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/152217437452819.png)

4. 程序调用

   加锁:

   ```abap
   "当调用设置锁函数时，LOCK PARAMETERS 如果没有指明，系统会锁定整个表。
   TABLES: zspfli.
   DATA it_zspfli LIKE TABLE OF zspfli WITH HEADER LINE.
   
   CALL FUNCTION 'ENQUEUE_EZ_ZSPFLI'" 加锁
    EXPORTING
      mode_zspfli          = 'E'
      mandt                = sy-mandt
      carrid               = 'AA'
      connid               = '0011'
   *   X_CARRID             = ' '" 是否使用初始值填充参数 CARRID
   *   X_CONNID             = ' '" 是否使用初始值填充参数 CONNID
   *   _SCOPE               = '2'
   *   _WAIT                = ' '
   *   _COLLECT             = ' '
    EXCEPTIONS
      foreign_lock         = 1
      system_failure       = 2 .
      
   " 判断锁定是否成功   
   IF sy-subrc <> 0.
     MESSAGE '2、本行数据不能修改！' TYPE 'I'.
   ELSE.
     SELECT * INTO TABLE it_zspfli FROM zspfli.
     READ TABLE it_zspfli WITH KEY carrid = 'AA' connid = '0011'.
     it_zspfli-distance = 2000.
     MODIFY zspfli FROM it_zspfli." 修改数据
   ENDIF.
   ```

   > **扩展:**
   >
   > ​    SAP 锁定与数据库物理锁定是不同的，它是一种业务逻辑上的锁定。它不会在物理表上进行加锁，而是将关键字传递加锁函数，加锁函数会在特定表中进加锁信息登记。
   >
   > **参数解释:**
   >
   > * `_SCOPE`:  
   >
   >   1: 表示程序内有效    
   >
   >   2: 表示 update module 内有效  
   >
   >   3: 全部
   >
   > * `_WAIT`:   
   >
   >   表示如果对象已经被锁定 , 是否等待后再尝试加锁 , 最大的等待时间有系统参数 ENQUE/DELAY_MAX 控制
   >
   > * `_COLLECT`: 
   >
   >   参数表示是否收集后进行统一提交 ,COLLECT 是一种缓存与批处理方法 , 即如果指定了 Collect, 加锁信息会放到 Lock Container 中 ,Lock Container 实际上是一个 funciton Group 控制的内存区域 , 如果程序中加了很多锁 , 锁信息会先放到内存中 , 这样可以减少对 SAP 锁管理系统访问 , 若使 Lock Container 中的锁生效 , 需执行 `FLUSH_ENQUEUE` 这个 Funciton, 将锁信息更新到锁管理系统中 , 此时加锁操作生效 , 使用函数 `RESET_ENQUEUE` 可以清除 Lock Container 中的锁信息

   解锁: 

   ```abap
   CALL FUNCTION 'DEQUEUE_EZ_ZSPFLI'" 解锁
    EXPORTING
      mode_zspfli       = 'E'
      mandt             = sy-mandt
      carrid            = 'AA'
      connid            = '0011'
   *   X_CARRID          = ' '
   *   X_CONNID          = ' '
   *   _SCOPE            = '3'
   *   _SYNCHRON         = ' '
   *   _COLLECT          = ' '.
   ```

   > ​    有些情况下，程序中设置的逻辑锁会隐式的自己解锁。比如说程序结束发生的时候（MESSAGE TYPE 为 A 或者 X 的时候），使用语句 `LEAVE PROGRAM`，`LEAVE TO TRANSACTION`，或者在命令行输入 /n 回车以后。
   >
   >  
   >
   > 在程序的结束可以用 `DEQUEUE FUNCTION MODULE` 来解锁（当然如果你不写这个，程序结束的时候也会自动的解锁），这个时候，系统会自动从 LOCK TABLE 把相应的记录删除。使用 `DEQUEUE FUNCTION MODULE` 来解锁的时候，不会产生 EXCEPTION（不需要对系统返回码 sy-subrc 进行判断）。如是要解开你在程序中创建的所有的逻辑锁，可以用函数：`DEQUEUE_ALL`.

### 锁查看和维护 ###

当使用程序加锁时，在可加锁的情况下，会即时的产生一条锁信息数据，可以通过 SM12 查看所产生的锁信息数据。通过 SM12，还可以删除锁记录，让数据不再锁定

![锁定条目选择](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/152217488394930.png)

![查看锁定条目](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/152217505422143.png)

### 加锁和解锁函数 ###

