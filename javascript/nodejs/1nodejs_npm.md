## [NodeJS NPM](http://www.runoob.com/nodejs/nodejs-npm.html)

### NPM Intro
NPM是随同NodeJS一起安装的包管理工具，能解决NodeJS代码部署上的很多问题，常见的使用场景有以下几种：
- 允许用户从NPM服务器下载别人编写的第三方包到本地使用。
- 允许用户从NPM服务器下载并安装别人编写的命令行程序到本地使用。
- 允许用户将自己编写的包或命令行程序上传到NPM服务器供别人使用。

``` bash
    $ npm -v
    $ sudo npm install npm -g
```

#### 使用npm命令安装模块

``` bash
    $ npm install <Module Name>
```

以下实例，我们使用 npm 命令安装常用的 Node.js web框架模块 express:

``` bash
    $ npm install express
```
安装好之后，express 包就放在了工程目录下的`node_modules`目录中，因此在代码中只需要通过 `require('express')`的方式就好，无需指定第三方包路径。

``` javascript
    var express = require('express');
```

#### 全局安装与本地安装
npm 的包安装分为本地安装（local）、全局安装（global）两种，从敲的命令行来看，差别只是有没有-g而已，比如

``` bash
    npm install express          # 本地安装
    npm install express -g   # 全局安装
```
如果出现以下错误：

    npm err! Error: connect ECONNREFUSED 127.0.0.1:8087 
解决办法为：

    $ npm config set proxy null

##### 本地安装
1. 将安装包放在`./node_modules`下（运行`npm`命令时所在的目录），
如果没有`node_modules`目录，会在当前执行`npm`命令的目录下生成`node_modules`目录。
2. 可以通过 require() 来引入本地安装的包。
##### 全局安装
1. 将安装包放在`/usr/local`下或者你`node`的安装目录。
2. 可以直接在命令行里使用。
如果你希望具备两者功能，则需要在两个地方安装它或使用 npm link。

##### 查看安装信息
    $ npm list -g
    $ npm list grunt    // 查看某个模块的版本号

#### 使用 package.json
package.json位于模块的目录下，用于定义包的属性。  
express包的`package.json`文件，位于`node_modules/express/package.json`

##### Package.json 属性说明
- name         - 包名。
- version      - 包的版本号。
- description  - 包的描述。
- homepage     - 包的官网 url 。
- author       - 包的作者姓名。
- contributors - 包的其他贡献者姓名。
- dependencies - 依赖包列表。如果依赖包没有安装，npm 会自动将依赖包安装在`node_module`目录下。
- repository   - 包代码存放的地方的类型，可以是 git 或 svn，git 可在 Github 上。
- main         - main 字段是一个模块ID，它是一个指向你程序的主要项目。就是说，如果你包的名字叫 express，然后用户安装它，然后require("express")。
- keywords     - 关键字

#### 卸载模块
    $ npm uninstall express
卸载后，你可以到`/nrde_modules/`目录下查看包是否还存在，或者使用以下命令查看：

    $ npm ls
#### 更新模块
    $ npm update express
#### 搜索模块
    $ npm search express
#### 创建模块
    $ npm init
接下来我们可以使用以下命令在 npm 资源库中注册用户（使用邮箱注册）：

    $ npm adduser
    Username: mcmohd
    Password:
    Email: (this IS public) mcmohd@gmail.com
接下来我们就用以下命令来发布模块：

    $ npm publish
如果你以上的步骤都操作正确，你就可以跟其他模块一样使用 npm 来安装。
#### 版本号
语义版本号分为X.Y.Z三位，分别代表主版本号、次版本号和补丁版本号。
- 当代码变更时，版本号按以下原则更新。 如果只是修复bug，需要更新Z位。
- 如果是新增了功能，但是向下兼容，需要更新Y位。
- 如果有大变动，向下不兼容，需要更新X位。

#### NPM 常用命令
除了本章介绍的部分外，NPM还提供了很多功能，package.json里也有很多其它有用的字段。  
除了可以在npmjs.org/doc/查看官方文档外，这里再介绍一些NPM常用命令。
- NPM提供了很多命令，例如`install`和`publish`，使用`npm help`可查看所有命令。
- 使用`npm help <command>`可查看某条命令的详细帮助，例如`npm help install`。
- 在`package.json`所在目录下使用`npm install . -g` 可先在本地安装当前命令行程序，可用于发布前的本地测试。
- 使用`npm update <package>`可以把当前目录下`node_modules`子目录里边的对应模块更新至最新版本。
- 使用`npm update <package> -g`可以把全局安装的对应命令行程序更新至最新版。
- 使用`npm cache clear`可以清空NPM本地缓存，用于对付使用相同版本号发布新版本代码的人。
- 使用`npm unpublish <package>@<version>`可以撤销发布自己发布过的某个版本代码。
#### 使用淘宝 NPM 镜像
大家都知道国内直接使用 npm 的官方镜像是非常慢的，这里推荐使用淘宝 NPM 镜像。
淘宝 NPM 镜像是一个完整 npmjs.org 镜像，你可以用此代替官方版本(只读)，同步频率目前为 10分钟 一次以保证尽量与官方服务同步。
你可以使用淘宝定制的 cnpm (gzip 压缩支持) 命令行工具代替默认的 npm:

    $ npm install -g cnpm --registry=https://registry.npm.taobao.org
这样就可以使用 cnpm 命令来安装模块了：

$ cnpm install [name]
