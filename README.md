﻿# mogeRPG
text game

・readline.lispはhttp://keens.github.io/blog/2016/02/14/readlinenotsukurikata/  
を使わせてもらってます。  

・[win用exe](https://github.com/fusuya/mogeRPG/releases)  
  

・sbcl持ってる人はload.lispをloadするとゲーム始まると思います。  
・Linuxの場合quicklisp必要  
  
・マップ移動方法  
　・Linux  
   w or ↑：上に移動  
   a or ←：←に移動  
   s or ↓：↓に移動  
   d or →：→に移動  
  
  ・windows  
    w：上に移動  
    a：←に移動  
    s：↓に移動  
    d：→に移動  
    ww：上にダッシュ  
    aa：←にダッシュ  
    ss：↓にダッシュ  
    dd：→にダッシュ  
※ダッシュ：壁にぶつかるか敵と遭遇するまで移動する  
  
  
・攻撃の種類  
1:突く  
敵単体に大ダメージ（ランダム）   
2:ダブルアタック  
敵に2回中ダメージ攻撃できる  
3:薙ぎ払う  
ランダムな敵にランダム回数（素早さ依存）、1ダメージ与える。 

・敵指定はアルファベット

・ハンマー  
ハンマーを持ってる状態で壁の方に移動すると壁を壊すかどうか選択肢が出る。   
階段を一階降りるごとに一個もらえる  

・回復薬  
それぞれステータスを回復する  