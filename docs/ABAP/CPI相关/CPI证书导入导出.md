## CPI证书导入SAP
<!-- tabs:start -->

<!-- tab:CPI证书导出 -->
<!-- tabs:start -->
<!-- tab:方式一 -->
  1. 进入 实用和租例->点击集成->点击集成和 API->点击全部![image-20240702162218789](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702162218789.png)

  2. 进入该界面，并导出该界面的证书![image-20240702164144100](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164144100.png)

     ![image-20240702164247223](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164247223.png)

     ![image-20240702164355589](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164355589.png)

     ![image-20240702164514164](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702164514164.png)

<!-- tab:方式二 -->

  1. 进入 实用和租例->点击集成->点击集成和 API->点击密匙库![image-20240702165054716](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165054716.png)
  2. 导出 `sap_digicent global ca g2`、`sap_digicent global root ca`、`sap_digicent global root g2` 三个证书。![image-20240702165212793](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165212793.png)
  3. 点击三个点，并选择下载。另外两个同理![image-20240702165404612](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165404612.png)
<!-- tabs:end -->

<!-- tab:SAP证书导入 -->

- 进入 T-code：STRUST![image-20240702165711363](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165711363.png)
- 选择： SSL 客户端 SSL 客户端（匿名）![image-20240702165829831](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165829831.png)
- 点击更改![image-20240702165941496](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702165941496.png)
- 点击导入文件![image-20240702170233532](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170233532.png)
- 选择证书文件（上一步导出的证书）![image-20240702170338686](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170338686.png)
- 点击添加到证书列表![image-20240702170536493](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170536493.png)
- 点击保存即可<img src="https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20240702170616971.png" alt="image-20240702170616971" />
<!-- tabs:end -->