## [React](http://www.runoob.com/react/react-tutorial.html)
### React 特点
1. 声明式设计 −React采用声明范式，可以轻松描述应用。
2. 高效 −React通过对DOM的模拟，最大限度地减少与DOM的交互。
3. 灵活 −React可以与已知的库或框架很好地配合。
4. JSX − JSX 是 JavaScript 语法的扩展。React 开发不一定使用 JSX ，但我们建议使用它。
5. 组件 − 通过 React 构建组件，使得代码更加容易得到复用，能够很好的应用在大项目的开发中。
6. 单向响应的数据流 − React 实现了单向响应的数据流，从而减少了重复代码，这也是它为什么比传统数据绑定更简单。

### React Installation
- react.min.js     - React 的核心库
- react-dom.min.js - 提供与 DOM 相关的功能
- babel.min.js     - Babel 可以将 ES6 代码转为 ES5 代码，这样我们就能在目前不支持 ES6 浏览器上执行 React 代码。Babel 内嵌了对 JSX 的支持。通过将 Babel 和 babel-sublime 包（package）一同使用可以让源码的语法渲染上升到一个全新的水平。

### 通过 npm 使用 React
我们建议在 React 中使用 CommonJS 模块系统，比如 browserify 或 webpack，本教程使用 webpack。
国内使用 npm 速度很慢，你可以使用淘宝定制的 cnpm (gzip 压缩支持) 命令行工具代替默认的 npm:

``` bash
    $ npm install -g cnpm --registry=https://registry.npm.taobao.org
    $ npm config set registry https://registry.npm.taobao.org
    // 这样就可以使用 cnpm 命令来安装模块了：
    $ cnpm install [name]
```

### 使用 create-react-app 快速构建 React 开发环境
create-react-app 是来自于 Facebook，通过该命令我们无需配置就能快速构建 React 开发环境。
create-react-app 自动创建的项目是基于 Webpack + ES6 。
执行以下命令创建项目：

``` bash
    $ cnpm install -g create-react-app
    // if you see error info like this: /usr/bin/env: ‘node’: No such file or directory
    // execute: sudo ln -s /usr/bin/nodejs /usr/bin/node
    $ create-react-app my-app
    $ cd my-app/
    $ npm start
```
在浏览器中打开 http://localhost:3000/
尝试修改 src/App.js 文件代码：

#### create-react-app 执行慢的解决方法：
在使用`create-react-app my-app`来创建一个新的React应用，在拉取各种资源时,往往会非常慢,一直卡在那：
fetchMetadata: sill mapToRegistry uri http://registry.npmjs.org/whatwg-fetch
可以看到资源还是使用了 npmjs.org，解决方法是换成淘宝的资源：
$ npm config set registry https://registry.npm.taobao.org
-- 配置后可通过下面方式来验证是否成功
$ npm config get registry
-- 或 npm info express

### React JSX
我们不需要一定使用 JSX，但它有以下优点：
- JSX 执行更快，因为它在编译为 JavaScript 代码后进行了优化。
- 它是类型安全的，在编译过程中就能发现错误。
- 使用 JSX 编写模板更加简单快速。

#### 使用 JSX
JSX 看起来类似 HTML ，我们可以看下实例:

``` javascript
    ReactDOM.render(
        <h1>Hello, world!</h1>,
        document.getElementById('example')
    );
```
我们可以在以上代码中嵌套多个 HTML 标签，需要使用一个 div 元素包裹它，实例中的 p 元素添加了自定义属性 data-myattribute，添加自定义属性需要使用`data-`前缀。

``` javascript
    ReactDOM.render(
        <div>
        <h1>菜鸟教程</h1>
        <h2>欢迎学习 React</h2>
        <p data-myattribute = "somevalue">这是一个很不错的 JavaScript 库!</p>
        </div>
        ,
        document.getElementById('example')
    );
```

##### 独立文件
你的 React JSX 代码可以放在一个独立文件上，例如我们创建一个 helloworld_react.js 文件，代码如下：

``` javascript
    ReactDOM.render(
        <h1>Hello, world!</h1>,
        document.getElementById('example')
    );
```
然后在 HTML 文件中引入该 JS 文件：

``` javascript
    <body>
    <div id="example"></div>
    <script type="text/babel" src="helloworld_react.js"></script>
    </body>
```

#### JavaScript 表达式
我们可以在 JSX 中使用 JavaScript 表达式。表达式写在花括号 {} 中。实例如下：

``` javascript
    ReactDOM.render(
        <div>
        <h1>{1+1}</h1>
        </div>
       ,
        document.getElementById('example')
    );
```
在 JSX 中不能使用 if else 语句，但可以使用 conditional (三元运算) 表达式来替代。以下实例中如果变量 i 等于 1 浏览器将输出 true, 如果修改 i 的值，则会输出 false.

``` javascript
    ReactDOM.render(
        <div>
        <h1>{i == 1 ? 'True!' : 'False'}</h1>
        </div>
        ,
        document.getElementById('example')
    );
```

#### 样式
React 推荐使用内联样式。我们可以使用 camelCase 语法来设置内联样式. React 会在指定元素数字后自动添加 px 。以下实例演示了为 h1 元素添加 myStyle 内联样式：

``` javascript
    var myStyle = {
        fontSize: 100,
        color: '#FF0000'
    };
    ReactDOM.render(
        <h1 style = {myStyle}>菜鸟教程</h1>,
        document.getElementById('example')
    );
```
#### 注释
1、在标签内部的注释需要花括号
2、在标签外的的注释不能使用花括号

``` javascript
    ReactDOM.render(
        /*注释 */
        <h1>孙朝阳 {/*注释*/}</h1>,
        document.getElementById('example')
    );
```
#### 数组

X 允许在模板中插入数组，数组会自动展开所有成员：

```
    var arr = [
        <h1>菜鸟教程</h1>,
        <h2>学的不仅是技术，更是梦想！</h2>,
    ];
    ReactDOM.render(
        <div>{arr}</div>,
        document.getElementById('example')
    );
```

#### HTML 标签 vs. React 组件
React 可以渲染 HTML 标签 (strings) 或 React 组件 (classes)。
要渲染 HTML 标签，只需在 JSX 里使用小写字母的标签名。

    var myDivElement = <div className="foo" />;
    ReactDOM.render(myDivElement, document.getElementById('example'));
要渲染 React 组件，只需创建一个大写字母开头的本地变量。

    var MyComponent = React.createClass({/*...*/});
    var myElement = <MyComponent someProperty={true} />;
    ReactDOM.render(myElement, document.getElementById('example'));

React 的 JSX 使用大、小写的约定来区分本地组件的类和 HTML 标签。
注意: 由于 JSX 就是 JavaScript，一些标识符像 class 和 for 不建议作为 XML 属性名。作为替代，React DOM 使用 className 和 htmlFor 来做对应的属性。

### React组件
``` javascript
    var HelloMessage = React.createClass({
        render: function() {
            return <h1>Hello World！</h1>;
        }
    });

    ReactDOM.render(
        <HelloMessage />,
        document.getElementById('example')
    );
```

#### 实例解析：
React.createClass 方法用于生成一个组件类 HelloMessage。
<HelloMessage /> 实例组件类并输出信息。
注意，原生 HTML 元素名以小写字母开头，而自定义的 React 类名以**大写字母**开头，比如 HelloMessage 不能写成 helloMessage。除此之外还需要注意组件类只能包含一个顶层标签，否则也会报错。  

如果我们需要向组件传递参数，可以使用 this.props 对象,实例如下：

``` javascript
    var HelloMessage = React.createClass({
        render: function() {
            return <h1>Hello {this.props.name}</h1>;
        }
    });

    ReactDOM.render(
        <HelloMessage name="Runoob" />,
        document.getElementById('example')
    );
```

以上实例中 name 属性通过 this.props.name 来获取。
**注意**，在添加属性时， class 属性需要写成 className ，for 属性需要写成 htmlFor ，这是因为 class 和 for 是 JavaScript 的保留字。

#### 复合组件
我们可以通过创建多个组件来合成一个组件，即把组件的不同功能点进行分离。
以下实例我们实现了输出网站名字和网址的组件：

``` javascript
    var WebSite = React.createClass({
        render: function() {
            return (
                <div>
                    <Name name={this.props.name} />
                    <Link site={this.props.site} />
                </div>
            );
        }
    });

    var Name = React.createClass({
        render: function() {
            return (
                <h1>{this.props.name}</h1>
            );
        }
    });

    var Link = React.createClass({
        render: function() {
            return (
                <a href={this.props.site}>
                    {this.props.site}
                </a>
            );
        }
    });

    ReactDOM.render(
        <WebSite name="菜鸟教程" site=" http://www.runoob.com" />,
        document.getElementById('example')
    );
```

实例中 WebSite 组件使用了 Name 和 Link 组件来输出对应的信息，也就是说 WebSite 拥有 Name 和 Link 的实例。

### React State
React 把组件看成是一个状态机（State Machines）。通过与用户的交互，实现不同状态，然后渲染 UI，让用户界面和数据保持一致。  
当用户点击组件，导致状态变化，this.setState 方法就修改状态值，每次修改以后，自动调用 this.render 方法，再次渲染组件。

``` javascript
    ({liked: !this.state.liked});
      },
      render: function() {
        var text = this.state.liked ? '喜欢' : '不喜欢';
        return (
          <p onClick={this.handleClick}>
            你<b>{text}</b>我。点我切换状态。
          </p>
        );
      }
    });

    ReactDOM.render(
        <LikeButton />,
        document.getElementById('example')
    );
```

### React Props
#### 使用 Props
以下实例演示了如何在组件中使用 props：

``` javascript
    var HelloMessage = React.createClass({
        render: function() {
            return <h1>Hello {this.props.name}</h1>;
        }
    });

    ReactDOM.render(
        <HelloMessage name="Runoob" />,
        document.getElementById('example')
    )
```

实例中 name 属性通过 this.props.name 来获取。
#### 默认 Props
你可以通过 getDefaultProps() 方法为 props 设置默认值，实例如下：

``` javascript
    var HelloMessage = React.createClass({
        getDefaultProps: function() {
            return {
                name: 'Runoob'
            };
        },
        render: function() {
            return <h1>Hello {this.props.name}</h1>;
        }
    });

    ReactDOM.render(
        <HelloMessage />,
        document.getElementById('example')
    );
```

#### State和Props
以下实例演示了如何在应用中组合使用 state 和 props 。我们可以在父组件中设置 state， 并通过在子组件上使用 props 将其传递到子组件上。在 render 函数中, 我们设置 name 和 site 来获取父组件传递过来的数据。

``` javascript
    var WebSite = React.createClass({
        getInitialState: function() {
            return {
                name: "菜鸟教程",
                site: "http://www.runoob.com"
            };
        },

        render: function() {
            return (
                <div>
                    <Name name={this.state.name} />
                    <Link site={this.state.site} />
                </div>
            );
        }
    });

    var Name = React.createClass({
        render: function() {
            return (
                <h1>{this.props.name}</h1>
            );
        }
    });

    var Link = React.createClass({
        render: function() {
            return (
                <a href={this.props.site}>
                    {this.props.site}
                </a>
            );
        }
    });

    ReactDOM.render(
        <WebSite />,
        document.getElementById('example')
    );
```

#### Props 验证
Props 验证使用 propTypes，它可以保证我们的应用组件被正确使用，React.PropTypes 提供很多验证器 (validator) 来验证传入数据是否有效。当向 props 传入无效数据时，JavaScript 控制台会抛出警告。
以下实例创建一个 Mytitle 组件，属性 title 是必须的且是字符串，非字符串类型会自动转换为字符串 ：

``` javascript
    var title = "菜鸟教程";
    // var title = 123;
    var MyTitle = React.createClass({
        propTypes: {
            title: React.PropTypes.string.isRequired,
        },

        render: function() {
            return <h1> {this.props.title} </h1>;
        }
    });
    ReactDOM.render(
        <MyTitle title={title} />,
        document.getElementById('example')
    );
```

##### 更多验证器
``` javascript
    React.createClass({
        propTypes: {
            // 可以声明 prop 为指定的 JS 基本数据类型，默认情况，这些数据是可选的
            optionalArray: React.PropTypes.array,
            optionalBool: React.PropTypes.bool,
            optionalFunc: React.PropTypes.func,
            optionalNumber: React.PropTypes.number,
            optionalObject: React.PropTypes.object,
            optionalString: React.PropTypes.string,

            // 可以被渲染的对象 numbers, strings, elements 或 array
            optionalNode: React.PropTypes.node,

            //  React 元素
            optionalElement: React.PropTypes.element,

            // 用 JS 的 instanceof 操作符声明 prop 为类的实例。
            optionalMessage: React.PropTypes.instanceOf(Message),

            // 用 enum 来限制 prop 只接受指定的值。
            optionalEnum: React.PropTypes.oneOf(['News', 'Photos']),

            // 可以是多个对象类型中的一个
            optionalUnion: React.PropTypes.oneOfType([
                React.PropTypes.string,
                React.PropTypes.number,
                React.PropTypes.instanceOf(Message)
            ]),

            // 指定类型组成的数组
            optionalArrayOf: React.PropTypes.arrayOf(React.PropTypes.number),

            // 指定类型的属性构成的对象
            optionalObjectOf: React.PropTypes.objectOf(React.PropTypes.number),

            // 特定 shape 参数的对象
            optionalObjectWithShape: React.PropTypes.shape({
                color: React.PropTypes.string,
                fontSize: React.PropTypes.number
            }),

            // 任意类型加上 `isRequired` 来使 prop 不可空。
            requiredFunc: React.PropTypes.func.isRequired,

            // 不可空的任意类型
            requiredAny: React.PropTypes.any.isRequired,

            // 自定义验证器。如果验证失败需要返回一个 Error 对象。不要直接使用 `console.warn` 或抛异常，因为这样 `oneOfType` 会失效。
            customProp: function(props, propName, componentName) {
                if (!/matchme/.test(props[propName])) {
                    return new Error('Validation failed!');
                }
            }
        },
        /* ... */
    });
```

#### React组件API
##### 设置状态：setState
setState(object nextState[, function callback])
###### 参数说明
- nextState，将要设置的新状态，该状态会和当前的state合并
- callback，可选参数，回调函数。该函数会在setState设置成功，且组件重新渲染后调用。
**合并**nextState和当前state，并重新渲染组件。setState是React事件处理函数中和请求回调函数中触发UI更新的主要方法。
###### 关于setState
不能在组件内部通过this.state修改状态，因为该状态会在调用setState()后被替换。
setState()并不会立即改变this.state，而是创建一个即将处理的state。setState()并不一定是同步的，为了提升性能React会批量执行state和DOM渲染。  
setState()总是会触发一次组件重绘，除非在shouldComponentUpdate()中实现了一些条件渲染逻辑。  
###### 实例
``` javascript
    var Counter = React.createClass({
        getInitialState: function () {
            return { clickCount: 0 };
        },
        handleClick: function () {
            this.setState(function(state) {
                return {clickCount: state.clickCount + 1};
            });
        },
        render: function () {
            return (<h2 onClick={this.handleClick}>点我！点击次数为: {this.state.clickCount}</h2>);
        }
    });
    ReactDOM.render(
        <Counter />,
        document.getElementById('message')
    );
```
##### 替换状态：replaceState
    replaceState(object nextState[, function callback])
- nextState，将要设置的新状态，该状态会替换当前的state。
- callback，可选参数，回调函数。该函数会在replaceState设置成功，且组件重新渲染后调用。
    replaceState()方法与setState()类似，但是方法只会保留nextState中状态，原state不在nextState中的状态都会被删除。
##### 设置属性：setProps
    setProps(object nextProps[, function callback])
- nextProps，将要设置的新属性，该状态会和当前的props合并
- callback，可选参数，回调函数。该函数会在setProps设置成功，且组件重新渲染后调用。
设置组件属性，并重新渲染组件。  
    props相当于组件的数据流，它总是会从父组件向下传递至所有的子组件中。当和一个外部的JavaScript应用集成时，我们可能会需要向组件传递数据或通知React.render()组件需要重新渲染，可以使用setProps()。
更新组件，我可以在节点上再次调用React.render()，也可以通过setProps()方法改变组件属性，触发组件重新渲染。  

##### 替换属性：replaceProps
    replaceProps(object nextProps[, function callback])
nextProps，将要设置的新属性，该属性会替换当前的props。
callback，可选参数，回调函数。该函数会在replaceProps设置成功，且组件重新渲染后调用。
replaceProps()方法与setProps类似，但它会删除原有props  
##### 强制更新：forceUpdate
    forceUpdate([function callback])
###### 参数说明
callback，可选参数，回调函数。该函数会在组件render()方法调用后调用。
forceUpdate()方法适用于this.props和this.state之外的组件重绘（如：修改了this.state后），通过该方法通知React需要调用render()
一般来说，应该尽量避免使用forceUpdate()，而仅从this.props和this.state中读取状态并由React触发render()调用。
##### 获取DOM节点：findDOMNode
    DOMElement findDOMNode()
返回值：DOM元素DOMElement
如果组件已经挂载到DOM中，该方法返回对应的本地浏览器 DOM 元素。当render返回null 或 false时，this.findDOMNode()也会返回null。从DOM 中读取值的时候，该方法很有用，如：获取表单字段的值和做一些 DOM 操作。
##### 判断组件挂载状态：isMounted
    bool isMounted()
返回值：true或false，表示组件是否已挂载到DOM中
isMounted()方法用于判断组件是否已挂载到DOM中。可以使用该方法保证了setState()和forceUpdate()在异步场景下的调用不会出错。

#### React组件生命周期
##### 组件的生命周期可分成三个状态：
- Mounting：已插入真实 DOM
- Updating：正在被重新渲染
- Unmounting：已移出真实 DOM
##### 生命周期的方法有：
- componentWillMount 在渲染前调用,在客户端也在服务端。
- componentDidMount : 在第一次渲染后调用，只在客户端。之后组件已经生成了对应的DOM结构，可以通过this.getDOMNode()来进行访问。 如果你想和其他JavaScript框架一起使用，可以在这个方法中调用setTimeout, setInterval或者发送AJAX请求等操作(防止异部操作阻塞UI)。
- componentWillReceiveProps 在组件接收到一个新的prop时被调用。这个方法在初始化render时不会被调用。
- shouldComponentUpdate 返回一个布尔值。在组件接收到新的props或者state时被调用。在初始化时或者使用forceUpdate时不被调用。可以在你确认不需要更新组件时使用。
- componentWillUpdate在组件接收到新的props或者state但还没有render时被调用。在初始化时不会被调用。
- componentDidUpdate 在组件完成更新后立即调用。在初始化时不会被调用。
- componentWillUnmount在组件从 DOM 中移除的时候立刻被调用。

#### React Ajax
#### React表单与事件
##### React事件

#### React Refs
React 支持一种非常特殊的属性 Ref ，你可以用来绑定到 render() 输出的任何组件上。
这个特殊的属性允许你引用 render() 返回的相应的支撑实例（ backing instance ）。这样就可以确保在任何时间总是拿到正确的实例。
使用方法
绑定一个 ref 属性到 render 的返回值上：

    <input ref="myInput" />
在其它代码中，通过 this.refs 获取支撑实例:

    var input = this.refs.myInput;
    var inputValue = input.value;
    var inputRect = input.getBoundingClientRect();

##### 完整实例
你可以通过使用 this 来获取当前 React 组件，或使用 ref 来获取组件的引用，实例如下：

``` javascript
    var MyComponent = React.createClass({
        handleClick: function() {
            // 使用原生的 DOM API 获取焦点
            this.refs.myInput.focus();
        },
        render: function() {
            //  当组件插入到 DOM 后，ref 属性添加一个组件的引用于到 this.refs
            return (
                <div>
                    <input type="text" ref="myInput" />
                    <input type="button" value="点我输入框获取焦点" onClick={this.handleClick} />
                </div>
            );
        }
    });

    ReactDOM.render(
        <MyComponent />,
        document.getElementById('example')
    );

实例中，我们获取了输入框的支撑实例的引用，子点击按钮后输入框获取焦点。
我们也可以使用 getDOMNode()方法获取DOM元素
