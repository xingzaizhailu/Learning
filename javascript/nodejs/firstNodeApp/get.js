var http = require('http');
var url  = require('url');
var util = require('util');
 
http.createServer(function(req, res){
    res.writeHead(200, {'Content-Type': 'text/plain; charset=utf-8'});
    res.end(util.inspect(url.parse(req.url, true)));
}).listen(3000);
// 在浏览器中访问 http://localhost:3000/user?name=菜鸟教程&url=www.runoob.com 然后查看返回结果
