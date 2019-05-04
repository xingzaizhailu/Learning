### Node.js模块系统
为了让Node.js的文件可以相互调用，Node.js提供了一个简单的模块系统。
模块是Node.js 应用程序的基本组成部分，文件和模块是一一对应的。
换言之，一个 Node.js 文件就是一个模块，这个文件可能是JavaScript 代码、JSON 或者编译过的C/C++ 扩展

#### 创建模块
在 Node.js 中，创建一个模块非常简单，如下我们创建一个 'main.js' 文件，代码如下:  

    var hello = require('./hello');
    hello.world();
以上实例中，代码`require('./hello')`引入了当前目录下的`hello.js`文件（`./`为当前目录，node.js默认后缀为js）。
Node.js 提供了`exports`和`require`两个对象，其中`exports`是模块公开的接口，require 用于从外部获取一个模块的接口，即所获取模块的`exports`对象。
接下来我们就来创建hello.js文件，代码如下：

    exports.world = function() {
        console.log('Hello World');
    }

有时候我们只是想把一个对象封装到模块中，格式如下：

    module.exports = function() {
        // ...
    }
例如：

``` javascript
    //hello.js 
    function Hello() { 
        var name; 
        this.setName = function(thyName) { 
            name = thyName; 
        }; 
        this.sayHello = function() { 
            console.log('Hello ' + name); 
        }; 
    }; 
    module.exports = Hello;
```

#### require
#### 优先级
缓存区中 > 原生模块(原生模块缓存区内的 > 不在的) > 文件模块
找到后加载(包含包装和编译)到缓存中
#### 参数的传递：
- http、fs、path等，原生模块。
- ./mod或../mod，相对路径的文件模块。
- /pathtomodule/mod，绝对路径的文件模块。
- mod，非原生模块的文件模块。

### Node.js Function
在JavaScript中，一个函数可以作为另一个函数的参数。我们可以先定义一个函数，然后传递，也可以在传递参数的地方直接定义函数。
Node.js中函数的使用与Javascript类似，举例来说，你可以这样做：

``` javascript
    function say(word) {
        console.log(word);
    }

    function execute(someFunction, value) {
        someFunction(value);
    }

    execute(say, "Hello");
```
以上代码中，我们把 say 函数作为execute函数的第一个变量进行了传递。这里返回的不是 say 的返回值，而是 say **本身**！
这样一来， say 就变成了execute 中的本地变量 someFunction ，execute可以通过调用 someFunction() （带括号的形式）来使用 say 函数。
当然，因为 say 有一个变量， execute 在调用 someFunction 时可以传递这样一个变量。
#### 匿名函数
我们可以把一个函数作为变量传递。但是我们不一定要绕这个"先定义，再传递"的圈子，我们可以直接在另一个函数的括号中定义和传递这个函数：

``` javascript
    function execute(someFunction, value) {
        someFunction(value);
    }

    execute(function(word){ console.log(word) }, "Hello");
```
#### 函数传递是如何让HTTP服务器工作的
带着这些知识，我们再来看看我们简约而不简单的HTTP服务器：

``` javascript
    var http = require("http");

    http.createServer(function(request, response) {
        response.writeHead(200, {"Content-Type": "text/plain"});
        response.write("Hello World");
        response.end();
    }).listen(8888);
```
现在它看上去应该清晰了很多：我们向 createServer 函数传递了一个匿名函数。
用这样的代码也可以达到同样的目的：

``` javascript
    var http = require("http");

    function onRequest(request, response) {
        response.writeHead(200, {"Content-Type": "text/plain"});
        response.write("Hello World");
        response.end();
    }

    http.createServer(onRequest).listen(8888);
```
