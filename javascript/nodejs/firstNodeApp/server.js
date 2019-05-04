// Step 1: 引入required模块
// 使用 require 指令来载入 http 模块，并将实例化的 HTTP 赋值给变量 http
var http = require("http");

// Step 2: 创建服务器
// 接下来我们使用 http.createServer() 方法创建服务器，并使用 listen 方法绑定 8888 端口。 
// 函数通过 request, response 参数来接收和响应数据。
// 实例如下，在你项目的根目录下创建一个叫 server.js 的文件，并写入以下代码：

http.createServer(function (request, response) {
    // 发送 HTTP 头部
    // HTTP 状态值：200 ： OK
    // 内容类型： text/plain
    response.writeHead(200, {'Content-Type': 'text/plain'});

    // 发送响应数据 "Hello, World"
    response.end('Hello World\n');
}).listen(6100);

// 终端打印如下信息
console.log('Server running at http://127.0.0.1:6100/');
