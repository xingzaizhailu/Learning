### [Node.js全局对象](http://www.runoob.com/nodejs/nodejs-global-object.html)
JavaScript 中有一个特殊的对象，称为全局对象（Global Object），它及其所有属性都可以在程序的任何地方访问，即全局变量。
在浏览器 JavaScript 中，通常 window 是全局对象， 而 Node.js 中的全局对象是 global，所有全局变量（除了 global 本身以外）都是 global 对象的属性。

#### 全局对象与全局变量
global 最根本的作用是作为全局变量的宿主。按照 ECMAScript 的定义，满足以下条 件的变量是全局变量：
- 在最外层定义的变量；
- 全局对象的属性；
- 隐式定义的变量（未定义直接赋值的变量）。

**注意：** 永远使用 var 定义变量以避免引入全局变量，因为全局变量会污染 命名空间，提高代码的耦合风险。

#### \_filename
表示当前正在执行的脚本的文件名。它将输出文件所在位置的绝对路径，且和命令行参数所指定的文件名不一定相同。 如果在模块中，返回的值是模块文件的路径。

##### 实例
创建文件 main.js ，代码如下所示：

``` javascript
    // 输出全局变量 __filename 的值
    console.log( __filename );
    //执行 main.js 文件，代码如下所示:
    // $ node main.js
    // /web/com/runoob/nodejs/main.js
```
#### \__dirname
表示当前执行脚本所在的目录。
##### 实例
创建文件 main.js ，代码如下所示：

``` javascript
    // 输出全局变量 __dirname 的值
    console.log( __dirname );
    // 执行 main.js 文件，代码如下所示:
    // $ node main.js
    // /web/com/runoob/nodejs
```
#### setTimeout(cb, ms)
`setTimeout(cb, ms)`全局函数在指定的毫秒(ms)数后执行指定函数(cb)。
`：setTimeout()`只执行一次指定函数。
返回一个代表定时器的句柄值。
##### 实例
创建文件 main.js ，代码如下所示：
``` javascript
    function printHello(){
        console.log( "Hello, World!");
    }
    // 两秒后执行以上函数
    setTimeout(printHello, 2000);
    
    // 执行 main.js 文件，代码如下所示:
    // $ node main.js
    // Hello, World!
```
#### clearTimeout(t)
`clearTimeout( t )`全局函数用于停止一个之前通过 setTimeout() 创建的定时器。 参数 t 是通过 setTimeout() 函数创建的定时器。
##### 实例
创建文件 main.js ，代码如下所示：

``` javascript
    function printHello(){
        console.log( "Hello, World!");
    }
    // 两秒后执行以上函数
    var t = setTimeout(printHello, 2000);

    // 清除定时器
    clearTimeout(t);
    // 执行 main.js 文件，代码如下所示:
    // $ node main.js
```
#### setInterval(cb, ms)
`setInterval(cb, ms)`全局函数在指定的毫秒(ms)数后执行指定函数(cb)。
返回一个代表定时器的句柄值。可以使用`clearInterval(t)`函数来清除定时器。
setInterval() 方法会不停地调用函数，直到 clearInterval() 被调用或窗口被关闭。
##### 实例
创建文件 main.js ，代码如下所示：

``` javascript
    function printHello(){
        console.log( "Hello, World!");
    }

    // 两秒后执行以上函数
    setInterval(printHello, 2000);

    // 执行 main.js 文件，代码如下所示:
    // $ node main.js
    // Hello, World! Hello, World! Hello, World! Hello, World! Hello, World! ……
```
以上程序每隔两秒就会输出一次"Hello, World!"，且会永久执行下去，直到你按下 ctrl + c 按钮。

#### console
console 用于提供控制台标准输出，它是由 Internet Explorer 的 JScript 引擎提供的调试工具，后来逐渐成为浏览器的事实标准。
Node.js 沿用了这个标准，提供与习惯行为一致的 console 对象，用于向标准输出流（stdout）或标准错误流（stderr）输出字符。

#### console Functions
- log       如果有多个参数，则 以类似于C 语言 printf() 命令的格式输出。
    console.log('byvoid%diovyb', 1991);
    / => byvoid1991iovyb 
- info
- error     控制台在出现错误时会显示是红色的叉子。
- warn      控制台出现有黄色的惊叹号。
- dir(obj[,options])       用来对一个对象进行检查（inspect），并以易于阅读和打印的格式显示。
- time(time)输出时间，表示计时开始。
- timeEnd   结束时间，表示计时结束。
- trace(message[,..])     当前执行的代码在堆栈中的调用路径，这个测试函数运行很有帮助，只要给想测试的函数里面加入 console.trace 就行了。
- assert(value[,message]\[,..])    用于判断某个表达式或变量是否为真，接手两个参数，第一个参数是表达式，第二个参数是字符串。只有当第一个参数为false，才会输出第二个参数，否则不会有任何结果

##### 实例
``` javascript
    console.info("程序开始执行：");

    var counter = 10;
    console.log("计数: %d", counter);

    console.time("获取数据");
    //
    // 执行一些代码
    // 
    console.timeEnd('获取数据');

    console.info("程序执行完毕。")
```
    执行 main.js 文件，代码如下所示:

``` bash
    $ node main.js
    程序开始执行：
    计数: 10
    获取数据: 0ms
    程序执行完毕
```

#### process
process 是一个全局变量，即 global 对象的属性。
它用于描述当前Node.js 进程状态的对象，提供了一个与操作系统的简单接口。
通常在你写本地命令行程序的时候，少不了要 和它打交道。下面将会介绍 process 对象的一些最常用的成员方法。

- exit 当进程准备退出时触发。
- beforeExit 当 node 清空事件循环，并且没有其他安排时触发这个事件。通常来说，当没有进程安排时 node 退出，但是 'beforeExit' 的监听器可以异步调用，这样 node 就会继续执行。
- uncaughtException 当一个异常冒泡回到事件循环，触发这个事件。如果给异常添加了监视器，默认的操作（打印堆栈跟踪信息并退出）就不会发生。
- Signal 事件 当进程接收到信号时就触发。信号列表详见标准的 POSIX 信号名，如 SIGINT、SIGUSR1 等。

##### 实例
``` javascript
    process.on('exit', function(code) {

        // 以下代码永远不会执行
        setTimeout(function() {
            console.log("该代码不会执行");
        }, 0);
            
        console.log('退出码为:', code);
    });
    console.log("程序执行结束");
```
执行 main.js 文件，代码如下所示:

``` bash
    $ node main.js
    程序执行结束
    退出码为: 0
```

##### 退出状态码
##### Process属性
##### 实例
##### process方法
##### 实例





















