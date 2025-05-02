- eclispe 中显示 sap gui 界面

  在 eclipse.ini 加入以下配置

  ```cmd
  --add-modules=ALL-MODULE-PATH
  --module-path=/Users/zhangshuailong/Library/Java/JavaFX/javafx-sdk-22.0.1/lib
  --add-exports=javafx.web/com.sun.webkit.dom=ALL-UNNAMED
  ```

- 微信双开

  ```bash
  nohup /Applications/WeChat.app/Contents/MacOS/WeChat > /dev/null 2>&1 &
  ```

  可以起别名，后续输入 wechat 即可

  ```bash
  alias wechat='nohup /Applications/WeChat.app/Contents/MacOS/WeChat > /dev/null 2>&1 &'
  ```


- Mac 使用 INode 报错，先停止服务：

  ```bash
  sudo /Library/StartupItems/iNodeAuthService/iNodeAuthService stop
  ```

  启用服务：

  ```bash
  sudo /Library/StartupItems/iNodeAuthService/iNodeAuthService start 
  ```

- Mac 开盖开机

  取消

  ```bash
  sudo nvram AutoBoot=%0
  ```

  开启

  ```bash
  sudo nvram AutoBoot=%03
  ```

  

