### [Node.js 文件系统](http://www.runoob.com/nodejs/nodejs-fs.html)

``` javascript
    var fs = require("fs");
```
#### 异步和同步
有异步的 fs.readFile() 和同步的 fs.readFileSync()。
异步的方法函数最后一个参数为回调函数，回调函数的第一个参数包含了错误信息(error)。
建议大家是用异步方法，比起同步，异步方法性能更高，速度更快，而且没有阻塞。  
##### 实例
创建 input.txt 文件，内容如下：
>    菜鸟教程官网地址：www.runoob.com
>    文件读取实例

创建 file.js 文件, 代码如下：

``` javascript
    var fs = require("fs");

    // 异步读取
    fs.readFile('input.txt', function (err, data) {
        if (err) {
            return console.error(err);
        }
        console.log("异步读取: " + data.toString());
    });

    // 同步读取
    var data = fs.readFileSync('input.txt');
    console.log("同步读取: " + data.toString());

    console.log("程序执行完毕。");
```
以上代码执行结果如下：

``` javascript
    $ node file.js 
    同步读取: 菜鸟教程官网地址：www.runoob.com
    文件读取实例

    程序执行完毕。
    异步读取: 菜鸟教程官网地址：www.runoob.com
    文件读取实例
```
#### 打开文件
#### 获取文件信息
#### 写入文件
语法: `fs.writeFile(file, data[, options], callback)`
如果文件存在，该方法写入的内容**会覆盖**旧的文件内容。
#### 读取文件
#### 关闭文件
#### 截取文件
#### 删除文件
#### 创建文件
#### 读取目录
#### 删除目录
#### 方法

