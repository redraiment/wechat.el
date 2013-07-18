#!/bin/sh

curl -d "<xml>
  <ToUserName><![CDATA[wechat.el]]></ToUserName>
  <FromUserName><![CDATA[joe]]></FromUserName>
  <CreateTime>123</CreateTime>
  <MsgType><![CDATA[text]]></MsgType>
  <Content><![CDATA[${1}]]></Content>
  <FuncFlag>0</FuncFlag>
</xml>" "http://localhost:26870/game/5x5.el"
echo
