# wechat.el
使用Emacs Lisp，基于elnode的微信公众平台开发框架。
提供便捷的方法定义“状态”以及状态之间迁移的条件。

# wechat-5x5.el
一个样例，微信小游戏——开窗。展示了如何基于wechat.el开发自己的公众平台应用。

# launch.el
启动脚本。下载elnode等必要的Emacs插件，载入并启动服务器。

# 启动方法
1. git clone https://github.com/redraiment/wechat.el.git
2. emacs launch.el
3. M-x
4. eval-current-buffer
之后，你就能访问 http://localhost:26870/game/5x5.el?echostr=hello 来测试。
你还可以改变默认的端口（26870）或与Nginx等Web服务器集成（方法参见launch.el的注释）。

## 运行界面
![Screenshot](http://redraiment.com/resources/figure/open-windows/1.png)

## 微信二维码
![2D-Code](http://redraiment.com/resources/figure/open-windows/2.jpg)
