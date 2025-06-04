## LDB简介 ##

LDB —– 逻辑数据库(SE36),在HR报表开发中,用于替代传统的SQL语句,实质上就是一个程序,LDB自带屏幕,可根据配置实现不同的屏幕展现.

开发过程中介意忽略具体的数据表之间通过外部关键字设定的关系,而是使用  ==GET== 事件按照节点层级逐级获取数据,自带权限检查.

HR中常用的LDB有: 

- PNP —– 老版本的PA相关逻辑数据库,自带 payrall 的节点
- PNPCE —– 新版本的PA相关逻辑数据库(推荐使用)
- PCH —– OM相关的逻辑数据库

薪酬逻辑数据库DEMO: `EXAMPLE_PNP_GET_PAYROLL`

## 常用宏 ##

### HR PNP ###

常用的包含的文件(LDB报表中不需要声明): **DBPNPSEL**(LDB选择屏幕)   **DBPNPCOM**(常用宏包含文件),保存宏的表 **TRMAC**

| 宏/参数                        | 参数                                                         | 描述                                                         |
| ------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **rp-set-data-interval**       | &1  信息类型,例如 pa0000<br />&2  取数开始日期<br />&3  取数结束日期 | 设置某个信息类型的取数范围(get 事件之前),否则所有的取数范围将根据`pnpbegda`和`pnpendda`决定 |
| **rp_provide_from_last**       | &1  信息类型表<br />&2  子类型 subtype<br />&3  开始日期<br />&4  结束日期 | 从内表PXXXX中读取指定时间内最新的一条记录,使用 `PNP-SW-FOUND`判断是否存在数据 |
| **rp_provide_from_first**      | &1  信息类型表<br />&2  子类型 subtype<br />&3  开始日期<br />&4  结束日期 | 从内表PXXXX中读取指定时间内最早的一条记录,使用 `PNP-SW-FOUND`判断是否存在数据 |
| **rp_read_infotype**           | &1  员工编号<br />&2  信息类型 例如 0000<br />&3  表, P类型表<br />&4  开始日期<br />&5  结束日期 | 读取指定事件指定信息类型的数据                               |
| **pnp-sw-found**               |                                                              | 1: 找到    2:没找到                                          |
| **pnp-sw-auth-skipped-record** |                                                              | 部分数据读取宏,是否有数据被过滤(无权限)  <br />1: 有<br />2: 没有 |
| **pnp-sw-skip-pernr**          |                                                              | 全局控制参数,设定如果当前用户对当前循环的员工的某个信息类型中的某条数据没有权限时,是否跳过这个员工<br />1:  跳过<br />2:  不跳过 |
| **PNPBEGDA/PNPENDDA**          |                                                              | 全局数据取数范围(在start of selection 后设置无效,如果要修改,可用 `rp-set-data-interval` |
| **PNPBEGPS/PNPENDPS**          |                                                              | 全局员工取值范围(在start of selection 后设置无效)            |
| **pn-begda/pn-endda**          |                                                              | 全局数据取值范围                                             |
| **pn-begps/pn-endps**          |                                                              | 全局员工取值范围                                             |
|                                |                                                              |                                                              |

### HR PNPCE ###

常用的包含的文件(LDB报表中不需要声明): **DBPNPCESEL**(LDB选择屏幕)   **DBPNPCECOM**(常用宏包含文件),保存宏的表 **TRMAC**,类似于PNP,目前常用宏中有两个名称可能不一致

| PNP                  | PNPCE                |
| -------------------- | -------------------- |
| RP-SET-DATA-INTERVAL | RP_SET_DATA_INTERVAL |
| PNP-SW-SKIP-PERNR    | PNP_SW_SKIP_PERNR    |

```ABAP
TABLES: PERNR.
NODES: PERAS.
INFOTYPES: 0000, 1111, ..., .
GET PERAS.
```

### HR PCH ###

PCH逻辑数据库主要是对应于组织管理(OM).相对于PNP逻辑数据库,PCH每次返回的节点是`OBJEC`类型,代表一个对象.这些对象包括在PP01可以维护的对象(例如:O,S… 等),随其一同返回的也是OM的信息类型(例如:1000,1001… 等).PCH查找节点,除了标准的选择条件之外,还可以应用评估路径,这样大大的增强了PCH的可用性.

同PNP逻辑数据库一样,PCH也要在程序里声明`TABLES:OBJEC.`才可以使用,返回的也是OBJEC结构,使用方法基本同PNP逻辑数据库类似.

```abap
*必须有的三行,若指定评估路径,系统会循环每一个由评估路径获取到的对象
TABLES: ojbec, gdstr."r若要带出评估路径选项,需要声明 gdstr
INFOTYPES: 1000.
GET OBJEC.
```

## 常用指令 ##

| 指令/语句             | 用途                                                         |
| --------------------- | ------------------------------------------------------------ |
| NODES                 | 声明LDB逻辑数据库节点,例: `NODES: BERAS.`                    |
| INFOTYPES             | 声明信息类型,声明后会自动创建一个带表头的PXXXX内表,并且自动填充满足条件的数据 |
| INFOTYPES XXXX MODE N | 声明信息类型 XXXX,但不填充数据(此处用于其中某些信息类型没有权限) |
| GET                   | 循环LDB节点,例: `GET PERNR.`                                 |
| GET…LATE              | 当节点的一条数据循环结束后触发.例如:<br />在PNP逻辑数据库中,一个员工可能会有很多薪资发放揭露,此时会有嵌套调用:<br />`start-of-selection`<br />`get pernr`.<br />`get payroll`.<br />`get pernr late`. |
| REJECT                | 终止本次循环的后续代码执行,跳转到下一条记录.类似于`LOOP`中的`continue`语句. |
| STOP                  | 结束LDB执行,直接跳转至`end-of-selection`                     |

