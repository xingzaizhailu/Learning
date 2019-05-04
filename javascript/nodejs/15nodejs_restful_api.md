### Node.js Restful API
#### HTTP 方法
以下为 REST 基本架构的四个方法：
- GET    - 用于获取数据。
- PUT    - 用于更新或添加数据。
- DELETE - 用于删除数据。
- POST   - 用于添加数据。

#### RESTful Web Services
#### 创建 RESTful
创建一个 json 数据资源文件 users.json，内容如下：

``` json
    {
       "user1" : {
            "name" : "mahesh",
                "password" : "password1",
                "profession" : "teacher",
                "id": 1
        },
        "user2" : {
            "name" : "suresh",
            "password" : "password2",
            "profession" : "librarian",
            "id": 2
        },
        "user3" : {
            "name" : "ramesh",
            "password" : "password3",
            "profession" : "clerk",
            "id": 3
        }
    }
```

基于以上数据，我们创建以下 RESTful API：
序号    URI HTTP    方法   发送内容     结果
1       listUsers   GET    空           显示所有用户列表
2       addUser     POST   JSON 字符串  添加新用户
3       deleteUser  DELETE JSON 字符串  删除用户
4       :id         GET    空           显示用户详细信息

##### 获取用户列表：
以下代码，我们创建了 RESTful API listUsers，用于读取用户的信息列表， server.js 文件代码如下所示：

``` javascript
    var express = require('express');
    var app = express();
    var fs = require("fs");

    app.get('/listUsers', function (req, res) {
        fs.readFile( __dirname + "/" + "users.json", 'utf8', function (err, data) {
            console.log( data );
            res.end( data );
        });
    })

    var server = app.listen(8081, function () {
        var host = server.address().address
        var port = server.address().port

        console.log("应用实例，访问地址为 http://%s:%s", host, port)

    })
```
接下来执行以下命令：

    $ node server.js 
    应用实例，访问地址为 http://0.0.0.0:8081

在浏览器中访问 http://127.0.0.1:8081/listUsers，结果如下所示：

``` 
    {
       "user1" : {
            "name" : "mahesh",
            "password" : "password1",
            "profession" : "teacher",
            "id": 1
        },
        "user2" : {
            "name" : "suresh",
            "password" : "password2",
            "profession" : "librarian",
            "id": 2
        },
        "user3" : {
            "name" : "ramesh",
            "password" : "password3",
            "profession" : "clerk",
            "id": 3
        }
    }
```

##### 添加用户
我们创建了 RESTful API addUser， 用于添加新的用户数据，server.js 文件代码如下所示：

``` javascript
    var express = require('express');
    var app = express();
    var fs = require("fs");

    //添加的新用户数据
    var user = {
        "user4" : {
            "name" : "mohit",
            "password" : "password4",
            "profession" : "teacher",
            "id": 4
        }
    }

    app.get('/addUser', function (req, res) {
        // 读取已存在的数据
        fs.readFile( __dirname + "/" + "users.json", 'utf8', function (err, data) {
            data = JSON.parse( data );
            data["user4"] = user["user4"];
            console.log( data );
            res.end( JSON.stringify(data));
        });
    })

    var server = app.listen(8081, function () {
        var host = server.address().address
        var port = server.address().port
        console.log("应用实例，访问地址为 http://%s:%s", host, port)
    })
```

接下来执行以下命令：

    $ node server.js 
    应用实例，访问地址为 http://0.0.0.0:8081

在浏览器中访问 http://127.0.0.1:8081/addUser，结果如下所示：

``` 
    { user1:
        { name: 'mahesh',
          password: 'password1',
          profession: 'teacher',
          id: 1 },
      user2:
        { name: 'suresh',
          password: 'password2',
          profession: 'librarian',
          id: 2 },
      user3:
        { name: 'ramesh',
          password: 'password3',
          profession: 'clerk',
          id: 3 },
      user4:
        { name: 'mohit',
          password: 'password4',
          profession: 'teacher',
          id: 4 } 
    }
```
##### 显示用户详情
以下代码，我们创建了 RESTful API :id（用户id）， 用于读取指定用户的详细信息，server.js 文件代码如下所示：

``` javascript
    var express = require('express');
    var app = express();
    var fs = require("fs");

    app.get('/:id', function (req, res) {
        // 首先我们读取已存在的用户
        fs.readFile( __dirname + "/" + "users.json", 'utf8', function (err, data) {
            data = JSON.parse( data );
            var user = data["user" + req.params.id] 
            console.log( user );
            res.end( JSON.stringify(user));
        });
    })

    var server = app.listen(8081, function () {

        var host = server.address().address
        var port = server.address().port
        console.log("应用实例，访问地址为 http://%s:%s", host, port)
    })
```
接下来执行以下命令：

    $ node server.js 
    应用实例，访问地址为 http://0.0.0.0:8081
在浏览器中访问 http://127.0.0.1:8081/2，结果如下所示：

    {
        "name":"suresh",
        "password":"password2",
        "profession":"librarian",
        "id":2
    }

##### 删除用户
以下代码，我们创建了 RESTful API deleteUser， 用于删除指定用户的详细信息，以下实例中，用户 id 为 2，server.js 文件代码如下所示：

``` javascript
    var express = require('express');
    var app = express();
    var fs = require("fs");

    var id = 2;

    app.get('/deleteUser', function (req, res) {
        // First read existing users.
        fs.readFile( __dirname + "/" + "users.json", 'utf8', function (err, data) {
            data = JSON.parse( data );
            delete data["user" + 2];
            
            console.log( data );
            res.end( JSON.stringify(data));
        });
    })

    var server = app.listen(8081, function () {
        var host = server.address().address
        var port = server.address().port
        console.log("应用实例，访问地址为 http://%s:%s", host, port)
    })
```
接下来执行以下命令：

    $ node server.js 
    应用实例，访问地址为 http://0.0.0.0:8081
在浏览器中访问 http://127.0.0.1:8081/deleteUser，结果如下所示：

```
    { user1:
        { name: 'mahesh',
          password: 'password1',
          profession: 'teacher',
          id: 1 },
      user3:
        { name: 'ramesh',
          password: 'password3',
          profession: 'clerk',
          id: 3 } 
    }
```














