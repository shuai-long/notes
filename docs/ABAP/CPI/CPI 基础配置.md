# CPI 初始化配置

**登陆 [CPIBTP](https://cockpit.cn40.platform.sapcloud.cn/cockpit)**

## 1. 创建子账户

1. **创建-->子账户**![创建子账户-1](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611103249041.png)

2. **填写子账户信息**![创建子账户-2](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611105255399.png)

   

## 2. 启用 Cloud Foundry

- **点击进入子账户**![image-20240611105605591](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611105605591.png)

- **点击启用 Cloud Foundry**![image-20240611105850979](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611105850979.png)
- **保持默认值即可，点击创建**![image-20240611105959108](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611105959108.png)

## 3. 创建空间

1. **点击创建空间**![image-20240611110122945](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611110122945.png)

2. **维护空间信息**

   ![image-20240611110726090](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240611110726090.png)

## 4. 订阅服务

1. **左侧菜单中找到 授权 并点击**![image-20240613143527159](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613143527159.png)
2. **点击编辑按钮**![image-20240613143820295](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613143820295.png)
3. **点击添加服务计划**![image-20240613143922319](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613143922319.png)

4. 搜索并添加服务计划： Integration Suite![image-20240613144524892](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613144524892.png)
5. 搜索并添加服务计划：Process Integration Runtime![image-20240613145051857](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613145051857.png)
6. 搜素并添加服务计划：Credential Store![image-20240613150039922](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613150039922.png)
7. 保存并退出浏览器![image-20240613145401999](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613145401999.png)

## 5. 创建实例和租用

1. 选择服务 -> Service Marketplace，搜索 Integration Suite 并选择创建![image-20240613150942744](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613150942744.png)
2. 选择创建之后默认下一步
3. 选择服务 -> 实例和租用![image-20240613152235304](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613152235304.png)
4. 点击后会弹出一个错误的提示，原因是权限不足。![image-20240613152606406](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613152606406.png)

## 6. 创建角色并添加权限

1. 选择 安全 --> 角色集合![image-20240613152825983](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613152825983.png)
2. 点击创建![image-20240613152916972](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240613152916972.png)
3. 输入角色名称和描述![image-20240614154656965](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614154656965.png)
4. 打开创建的集合配置权限![image-20240614155102826](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614155102826.png)
5. 点击编辑，更改角色集合的权限![image-20240614155250529](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614155250529.png)
6. 查找用户角色![image-20240614155627800](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614155627800.png)
7. 为避免有遗漏或不十分了解建议全选，然后点击添加![image-20240614155909070](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614155909070.png)
8. 在账号中查看用户的标识和电子邮件，在该角色集合中添加用户![image-20240614160538191](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614160538191.png)
9. 完成后点击保存即可。为避免未刷新配置，可关闭 BTP 重新登陆。![image-20240614160757719](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614160757719.png)

## 7. 调配租户

1. 重新进入租用和实例，此时会出现如下界面，创建 Capabilities , 点击 Add![image-20240614162423155](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614162423155.png)
2. **两种界面，按需匹配**
   1. 配置一
      1. 选择实例要添加的功能，必须选择 **设计、开发 和操作集成方案** 以及 **设计、开发和管理 API **功能。其他组件按需添加。![image-20240614163936286](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614163936286.png)
      2. 不用勾选 Message Queues，直接点击下一步![image-20240614164016347](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164016347.png)

      3. 勾选 API Management，点击下一步![image-20240614164139738](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164139738.png)
   2. 配置二
      1. 选择实例要添加的功能，全选所有组件![image-20240614181855534](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614181855534.png)
      2. 下一步![image-20240614181918814](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614181918814.png)
      3. 下一步![image-20240614181933296](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614181933296.png)

3. 等待激活![image-20240614164216173](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164216173.png)
4. 激活完成，重新启动 BTP 平台![image-20240614164242265](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164242265.png)
5. 再次检查用户权限，会发现系统新增了 PI_* 的角色集合![image-20240614164346529](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164346529.png)
6. 重复第六步（创建角色并添加权限），将新增的权限集合添加至自定义的集合中![image-20240614164448001](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240614164448001.png)

## 8. 创建云链接器

1. 进入 云链接器并登陆：[Basel 云链接器地址开发](https://vhbexdclcc01.sap.baesl.com:8443)、[Baesl 云链接器地址生产](https://vhbexpclcc01.sap.baesl.com:8443)

   ![image-20240617094427330](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617094427330.png)

2. 填写云链接器信息，相当于 BTP 中的子账户，Subaccount 需要输入对应子账户的标识![image-20240617094619121](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617094619121.png)![image-20240617094036495](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617094036495.png)

3. 保存后选择 云到本地，点击新建![image-20240617094902290](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617094902290.png)

4. 选择本地系统类型，SAP 系统选择 ABAP System/SAP Gateway，其他系统选择 Non-SAP System（根据具体情况选择），然后选择下一步。![image-20240617095203441](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617095203441.png)

5. 选择通讯协议，按需选择![image-20240617095321094](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617095321094.png)

6. 选择 IP 和端口，点击下一步![image-20240617095532937](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617095532937.png)

7. 填写虚拟 IP 和端口，可以随意填写（但是请按照命名规范，特别是仅有一个云链接器的情况下，请注意生产/

   测试/开发系统的名称，这个虚拟地址名称还关系到集成流配置那部分接口的配置）。![image-20240617095655672](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617095655672.png)

8. 选择系统链接的验证形式（无特殊情况，选择 None 即可）![image-20240617095806856](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617095806856.png)

9. 尽量使用虚拟地址（对系统接口在集成流内进行二次封装）![image-20240617100009024](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100009024.png)
10. 检查配置是否有误，没问题点击 Finish 即可完成配置。![image-20240617100115615](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100115615.png)
11. 选择刚创建的云链接器，点击修改可更改配置的云链接器信息![image-20240617100428541](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100428541.png)![image-20240617100341785](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100341785.png)

11. 为云链接器添加可访问的资源，选中刚刚搭建的 HOST![image-20240617100448894](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100448894.png)
12. 添加资源，URL Path 默认 / 即可，代表可访问所有资源，可参考下图![image-20240617100543730](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240617100543730.png)
