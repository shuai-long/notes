[TOC]

# WebService #

## 简介 ##

SAP 的 web service 是在 Netweaver 组件通过 UDDI 工具，采用 **SOAP** 和 **WSDL** 这两种 web 服务技术，将 SAP 已有功能封装成 webservice 对象，供其他系统调用或者调用其他系统的 webservice，从而实现 SAP 与非 SAP 系统（如 OA、PLM 等）系统的集成。

名词解释:

* **UUDI:** 在用户能够调用 Web 服务之前，必须确定这个服务内包含哪些属性与方法，找到被调用的接口定义，而这些都需要服务提供者（Service Provider）通过标准的 web 服务协议来进行编制。 UDDI 正是这样一个工具，用户通过 web 在 UDDI 中查找并定位那些他们需要的服务。UDDI 利用 SOAP 消息机制（标准的 XML/HTTP）来发布、编辑、浏览以及查找注册信息。它采用 XML 格式来封装各种不同类型的数据，并且发送到服务请求者或者由服务提供者返回需要的数据。

* **WSDL:**对于服务消费者（Service Consumer）来说，要找到一个自己需要使用的服务，他必须知道如何以及从哪调用。 WSDL 规范是一个描述接口、语义以及 Web 服务的 XML 文档。给这个 XML 文档配置上网络地址后，就可以简单而又快捷地被查找和定位。

* **SOAP:**当商业用户通过 UDDI 找到你的 WSDL 描述文档后，它通过 SOAP 协议调用你建立的 Web 服务中的一个或多个对象。从技术角度来看，SOAP 详细指明了如何响应不同的请求以及如何对参数编码。一个 SOAP 封装了可选的头信息和正文，并且通常使用 HTTP POST 方法来传送到一个 HTTP 服务器。SOAP 同时支持消息传送和远程过程调用。

## 发布 WebService ##

### 配置工作 ###

Webservice 最终是以 WSDL 形式发布，即一个带有 URL 的 XML 文件。既然是 URL，就必然涉及到发布服务器的域名、端口、目录等信息，因此 SAP 提供的 webservice 在能够被正常调用之前，需要对服务器的域名进行 DNS 解析，并且 WS 要发布必须要通过 SAP 的 SOA Manager 进行服务的绑定，所以还需要对 SOA Manager 进行的一定的设置，让其正常工作，才能进行下一步的服务绑定。具体步骤见下面章节。

### 配置服务器名称 ###

服务器的参数一般是由 basis 来配置完成的。这里主要用来查看服务器的 HostName，以帮助下一步的 DNS 解析操作。

如果想修改系统的参数，建议找 Basis 修改，这里与 webservice 相关的几个重要参数为：

* **login/system_client** 登录时默认的 Client 号
* **login/fails_to_user_lock** 密码输错多少次后锁定
* **rdisp/mshost** 状态栏中显示的系统名称
* **Sapglobalhost** SAP 全局主机名，这个参数就是后面需要设置的被解析的对象

1. 运行 T-CODE:**`RZ10`**, 即可进入服务器参数文件管理界面，如下图：

![服务器参数文件管理](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1681095747195.jpg)

点击 “参数文件” 旁边的搜索帮助按钮，即可弹出服务器上已有的实例和默认的参数设置，这里我们只需选择默认参数文件查看即可。

![服务器参数文件](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806210708162-1950998723.png)

选择下面的 “基本维护” 选项，就可以快速查看服务器默认的 Hostname 以及默认集团号，如下图所示:

![参数文件值](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806210808209-1400617125.png)

如果想查看更多服务器信息，可以选择参数维护概览界面的 “扩展维护” 选项，就可以看到服务器的详细信息，如下图所示：

![查看扩展维护](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806210821365-338672353.png)

![参数文件其他信息](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806210831522-1348886655.png)

### 配置服务器被 DNS 解析 ###

如果 SAP 服务器本身没有加入到所在工作环境的域控中，那么服务器的 hostname 默认是无法被解析到的，则需要进行服务器的 host 解析。常见的 host 解析办法有两种：集中解析和单机解析。各自的优缺点和办法如下：

* **集中解析:**集中解析就是将 host 解析条目添加到服务器所在网络的 DNS 服务中，一般需要联系网络管理员来添加这个解析条目。集中解析的优点就是处于该网络环境中的所有客户端主机，只要使用的默认 DNS 服务器，就可以直接访问发布后的 webservice，而不需要在本机上做出任何操作。

* **单机解析:**单机解析就是在客户端电脑上，直接修改本地的 host 文件，具体操作如下：

  依次浏览到这个目录下：`C:\Windows\System32\drivers\etc`，找到下面的一个名为 “hosts” 不带文件类型后缀的文件，右击这个文件，选择以记事本方式打开该文件，然后在下面的空白区域添加这样一条解析：

  `<SAP 服务器的 IP 地址 >   <SAP 的 hostname>`

### 启动 SOAMANAGER 服务 ###

前面已经介绍过 SAP 是通过 SOAMANAGER 来对外发布指定服务器的，SOAMANAGER 本身其实也是一个基于 Netweaver 的 Web Dynpro 程序，当你在 SAP GUI 客户端执行 T-Code：SOAMANAGER 的时候，就会默认调用 IE 来打开这个 WDA 程序，从而实现对 webservice 的发布的管理。但是根据 SAP 官方 Note1124553 的说法，出于安全方面的考虑，SAP Netweaver 组件安装后，默认是不激活 WDA 程序显示服务以及 SOAMANAGER 服务的，因此，需要手工通过 T-Code：==`SICF`== 来先激活 WAD 程序显示相关服务以及 SOAMANAGER 服务，具体需要激活的服务如下：

* **与显示 WDA 程序相关的服务**

  * /default_host/sap/bc/webdynpro
  * /default_host/sap/public/myssocntl
  * /default_host/sap/public/bc
    * /default_host/sap/public/bc/ur
    * /default_host/sap/public/bc/icons
    * /default_host/sap/public/bc/icons_rtl
    * /default_host/sap/public/bc/webicons
    * /default_host/sap/public/bc/pictograms
    * /default_host/sap/public/bc/webdynpro/*   (ssr, mimes 等所有子节点服务)

* **与 SOAMANAGER 相关的服务**

  - /default_host/sap/bc/soap 

  - /default_host/sap/bc/srt

  - /default_host/sap/bc/srt/rfc/sap

  - /default_host/sap/bc/webdynpro/sap 

  - /default_host/sap/bc/webdynpro/sap/appl_soap_management.

### 发布 WebService 接口 ###

要让外部应用能够访问 SAP 的 Web Service，那么必须配置 SAP 的 webservice 作为服务提供者（Service Provider），则需要先后按照以下步骤进行操作：

1. 创建 RFC 函数

   使用 T-code：==SE37== 或者 ==SE80==，创建函数，函数组设置与编程过程这里不作详细描述。将函数属性页签中的处理类型设置为 “远程启用的模块”，然后激活程序即可。

   ![创建 RFC 函数](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211554178-395153451.png)

2. 给 RFC 函数配置 WebService

   通过以下三种方法中的任意一种，均可进入 WebService 向导创建界面，这里以方法 3 触发向导为例，因为这样可以看到更多的配置界面。按照以下图示进行配置即可。

   * 通过 SE37 菜单中的使用程序创建

     实用程序 => 更多实用程序 => 创建 WEB 服务 => 来自函数模块

     ![通过 SE37 创建](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211616819-1528857689.png)

   * 使用 SE80 的函数组下的函数模块直接创建

     函数名右击 => 创建 =>企业服务

     ![通过 SE80 创建](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211627100-801950245.png)

   * 使用 SE80 的包进行创建

     在包名上右击 => 创建 => 企业服务
     
     ![SE80 选择包创建企业服务](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211637725-684684418.png)

   **创建企业服务详细步骤如下:**  

   ![创建企业服务--步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211648897-149640882.png)


![创建企业服务-步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211712412-1157656399.png)

此处生成的 WebService 需要记住, 后续生成 WSDL 地址需要使用

![创建企业服务-步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211722678-377814852.png)

![创建企业服务-步骤4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211729303-2140641794.png)

输入需要创建 WebService 的函数名称, 也可以通过右侧搜索帮助直接点选, 本选项涉及到名称映射, 可以取到隐藏真实的 ABAP 对象名, 这里不建议勾选

![创建企业服务-步骤5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211738381-936959018.png)

**SOAP APPL:** 保持默认, 不要修改

**Profile:** 是用来设置服务的安全策略的，这里建议选择第四个，第四个就是简单地使用 SAP 提供的接口用户名和密码进行访问。

![创建企业服务-步骤6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20190926175215903-1866558433.png)

需要传输的话选择请求号, 不需要传输的话勾选 `Local Object`

![创建企业服务-步骤7](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211748881-1220262486.png)

![创建企业服务-步骤8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211754428-127472864.png)

向导完成后，我们在左侧的服务目录中并不能看到我们刚刚创建的 WS，这时候需要点击菜单栏上的保存按钮，才会显示出来。显示出来的 WS 对象名称为蓝色的，有 ABAP 编程经验的应该知道，这个蓝色的表示没有激活，所以，选中新建的 WS，然后点击工具栏中的激活按钮进行激活，即完成 WS 的创建。

![创建企业服务-步骤9](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806211759444-138677446.png)



### 生成 WSDL ###

使用 SOAMANAGER 生成 WSDL( 可供外部访问的 XML 链接 ),  WebService 创建完成后，并不代表 WebService 配置完成，还需要使用 SOAMANAGER 来进行绑定 WSDL 才能被其他系统通过 WEB 方式进行访问和调用。具体操作步骤如下：

1. 输入 T-code: ==SOAMANAGER==, 回车执行

![生成WSDL地址-步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212025365-489854068.png)

2. 选择 Web 服务配置

![生成WSDL地址-步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212046959-373499805.png)

3. 输入刚刚创建的 WebService 名称进行搜索, 双击搜索到的 WebService 名称, 进入绑定界面:

![生成WSDL地址-步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212107834-579334664.png)

4. 点击创建服务

   ![生成WSDL地址-步骤4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212118850-1915503517.png)

5. **服务名称:** 输入刚刚查询使用的 WebService 名称

   **服务描述文本:** 随便输

   **新绑定名称:** 可以与 WebService 名称一致, 也可以另外起名

   ![生成WSDL地址-步骤5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212123428-1306268509.png)

6. 这里进行 WS 的安全设置，与前面创建 WS 时的 Profile 的选择有关系，将窗口下拉到 “传输通道验证” 板块，选择 “用户标识和密码”，然后回到顶部点击 “下一步”。

   ![生成WSDL地址-步骤6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212142178-1424461134.png)

   ![生成WSDL地址-步骤6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212213334-360448051.png)

7. SOAP 选项保持默认, 直接点击下一个

   ![生成WSDL地址-步骤7](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212219444-2089855820.png)

8. 完成 WSDL 的生成

   ![生成WSDL地址-步骤8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212234006-610148094.png)

9. 点击按钮可以打开 WSDL 的详细信息界面

   ![生成WSDL地址-步骤9](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212240225-162876805.png)

   ![生成WSDL地址-步骤9](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212245709-264263015.png)

10. 点击执行按钮后，可能会弹出用户认证信息，输入用户信息后，即可以打开 XML 文件了。

    ![生成WSDL地址-步骤10](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/1203362-20170806212302740-1839936900.png)

至此，SAP 作为 webservice 的提供者（Service Provider）已经配置完成。外部系统可以通过上面生成 WSDL 的链接与 SAP 系统进行交互了。

## 调用 WebService ##

SAP 能通过设置 WebService 与外部的 webservice 服务连接；

### **创建企业服务** ###

1. 使用 T-Code：==SE80==, 选择包 Package ，指定保存的开发包，按图 1 操作，创建一个 Enterprise Service。

   ![创建企业服务-步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211123235736.png)

2. 会出现如下的创建导向窗口，选择 ‘Service Consumer’，点击‘继续’

   ![创建企业服务-步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211123849145.png)

3. 选择 ==External WSDL/Schema==

   ![创建企业服务-步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124013366.png)

4. 选择 ==URL==, 点击继续

   ![创建企业服务-步骤4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124216607.png)

5. 填写 URL 地址

   ![创建企业服务-步骤5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124455944.png)

6. 分配请求: 在弹出的窗口中，在 Package 栏中输入所需保存的开发类，若保存为本地开发类，则需将‘Local/Object’选上

   ![创建企业服务-步骤6](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124546422.png)

7. 完成创建

   ![创建企业服务-步骤7](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124743285.png)

8. 保存并激活

   ![创建企业服务-步骤8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211124825409.png)

   ![创建企业服务-步骤8](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211125019896.png)

### 创建逻辑端口 ###

1. 执行事务: ==LPCONFIG==

   ![创建逻辑端口-步骤1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211125122662.png)

2. 填写生成的代理类 , 逻辑端口可以自定义, 勾选缺省端口

   ![创建逻辑端口-步骤2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/2021021112534096.png)

3. 点击创建, 填写描述 和对方的 ==URL== 地址, 勾选消息标识和状态管理

   ![创建逻辑端口-步骤3](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211130313472.png)

4. 在一般设置下的操作页签: 在激活代理类时, 系统自动将带出将要操作 WebService 的行为(方法), 在对应生产的代理类中又对应方法的输入及输出参数

   ==SOAP 操作==: 执行 WebService 行为的地址

   ![创建逻辑端口-步骤4](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211130802116.png)

5. 保存并激活逻辑端口

   ![创建逻辑端口-步骤5](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/20210211130909208.png)

   

   

   

   



### 程序实现 ###

```abap
DATA: lo_clientproxy   TYPE REF TO ZCO_SRMWEB_SERVICE_COMMON_SER1,      "参考生成的代理类
        lo_sys_exception TYPE REF TO cx_ai_system_fault.

DATA: ls_srm_input          TYPE zsrm_input1,
        ls_srm_input_response TYPE zsrm_input_response1.

* 2、调用webservice
  CREATE OBJECT lo_clientproxy
    EXPORTING
      logical_port_name = 'ZCO_SRMWEB_SERVICE_COMMON_SER1'.  "调用对应的逻辑端口

  ls_srm_input-parameters-els_account = '900000'."测试账号(固定值)
  ls_srm_input-parameters-access_token = split3."SRM的token值

  ls_srm_input-parameters-remote_request_input_vo-els_account = '900000'."大B账号(固定值)
  ls_srm_input-parameters-remote_request_input_vo-company_name = ''."公司名称(固定值)默认为空
  ls_srm_input-parameters-remote_request_input_vo-special_sign = ''."接口标识(固定值)默认为空
  ls_srm_input-parameters-remote_request_input_vo-special_sign = i_special_sign."接口标识(固定值)
  ls_srm_input-parameters-remote_request_input_vo-business_type = i_business_type."接口标识(固定值)
ls_srm_input-parameters-remote_request_input_vo-other_data = ls_data."JSON 格式

  TRY.
      CALL METHOD lo_clientproxy->srm_input
        EXPORTING
          srm_input          = ls_srm_input
        IMPORTING
          srm_input_response = ls_srm_input_response.

    CATCH cx_ai_system_fault INTO lo_sys_exception."捕获错误
      "CATCH zoa_cx_exception INTO lo_oa_exception.
  ENDTRY.

  o_type    = ls_srm_input_response-parameters-return-status.
  o_message = ls_srm_input_response-parameters-return-message.
  o_data    = ls_srm_input_response-parameters-return-data.
```

