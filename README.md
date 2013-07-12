# wechat.el
wechat.el是一个微信公众平台开发框架，但使用的是Emacs Lisp编程语言！利用Lisp语言强大的可定制性，使得开发一个公众平台的应用犹如编写一段剧本一样简单！

wechat.el使用了状态模式和责任链模式：

1. 把整个应用抽象成一张大的地图
2. 每一种状态都看作一个独立的房间，房间之间有很多扇门连接
3. 进入房间时自动输出提示语，即返回给用户的信息
4. 用户的输入就是钥匙，依次与该房间的门匹配，如果能开启，就通过这一扇门进入下一个房间，即状态迁移

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
