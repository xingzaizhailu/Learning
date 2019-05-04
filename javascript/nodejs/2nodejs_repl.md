### [Node.js REPL](http://www.runoob.com/nodejs/nodejs-repl.html)
Node.js REPL(Read Eval Print Loop:交互式解释器) 表示一个电脑的环境,我们可以在终端中输入命令，并接收系统的响应。 
Node 自带了交互式解释器，可以执行以下任务：
- 读取 - 读取用户输入，解析输入了Javascript 数据结构并存储在内存中。
- 执行 - 执行输入的数据结构
- 打印 - 输出结果
- 循环 - 循环操作以上步骤直到用户两次按下 ctrl-c 按钮退出。
Node 的交互式解释器可以很好的调试 Javascript 代码。

#### 使用变量
``` bash
    $ node
    > x = 10
    10
    > var y = 10
    undefined
    > console.log("y: " + y)
    10
```

#### 多行表达式
#### 下划线(\_)变量
使用下划线(\_)获取表达式的运算结果
``` bash
    $ node
    > var x = 10
    undefined
    > var y = 20
    undefined
    > x + y
    30
    > var sum = _
    undefined
    > console.log(sum)
    30
    undefined
```
#### REPL命令
- ctrl + c - 退出当前终端。
- ctrl + c 按下两次 - 退出 Node REPL。
- ctrl + d - 退出 Node REPL.
- 向上/向下 键 - 查看输入的历史命令
- tab 键 - 列出当前命令
- .help - 列出使用命令
- .break - 退出多行表达式
- .clear - 退出多行表达式
- .save filename - 保存当前的 Node REPL 会话到指定文件
- .load filename - 载入当前 Node REPL 会话的文件内容。
