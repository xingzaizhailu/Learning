## [React 入门实例教程](http://www.ruanyifeng.com/blog/2015/03/react.html)
### HTML Model
``` html
    <!DOCTYPE html>
    <html>
      <head>
        <script src="../build/react.js"></script>
        <script src="../build/react-dom.js"></script>
        <script src="../build/browser.min.js"></script>
      </head>
      <body>
        <div id="example"></div>
        <script type="text/babel">
          // ** Our code goes here! **
        </script>
      </body>
    </html>
```
React独有的语法JSX跟JS不兼容，所以凡是使用 JSX 的地方，都要加上 type="text/babel"
Browser.js 的作用是将 JSX 语法转为 JavaScript 语法，这一步很消耗时间，实际上线的时候，应该将它放到服务器完成。 

    $ babel src --out-dir build

### ReactDOM.render()
ReactDOM.render 是 React 的最基本方法，用于将模板转为 HTML 语言，并插入指定的 DOM 节点。  

``` react
    ReactDOM.render(
      <h1>Hello, world!</h1>,
      document.getElementById('example')
    );
```

### JSX Syntax
JSX 的基本语法规则：遇到 HTML 标签（以 `<` 开头），就用 HTML 规则解析；遇到代码块（以 `{` 开头），就用 JavaScript 规则解析)
JSX 允许直接在模板插入 JavaScript 变量。如果这个变量是一个数组，则会展开这个数组的所有成员

### Component
React 允许将代码封装成组件（component），然后像插入普通 HTML 标签一样，在网页中插入这个组件。React.createClass 方法就用于生成一个组件类。  
所有组件类都必须有自己的 render 方法，用于输出组件。  

``` react
    var HelloMessage = React.createClass({
      render: function() {
        return <h1>Hello {this.props.name}</h1>;
      }
    });

    ReactDOM.render(
      <HelloMessage name="John" />,
      document.getElementById('example')
    );
```
注意，组件类的第一个字母必须大写，否则会报错。另外，组件类只能包含一个顶层标签，否则也会报错。  

``` react
    // 报错， 因为包含了两个顶层标签：`h1`和`p`。
    var HelloMessage = React.createClass({
      render: function() {
        return <h1>
          Hello {this.props.name}
        </h1><p>
          some text
        </p>;
      }
    });
```
组件的用法与原生的 HTML 标签完全一致，可以任意加入属性，比如 `<HelloMessage name="John">` ，就是 HelloMessage 组件加入一个 `name` 属性，值为 `John`。  
添加组件属性，有一个地方需要注意，就是`class`属性需要写成`className`，`for`属性需要写成`htmlFor`，这是因为`class`和`for`是 JavaScript 的保留字。  

### this.props.children
`this.props`对象的属性与组件的属性一一对应，但是有一个例外，就是`this.props.children`属性。它表示组件的所有子节点。  

``` react
    var NotesList = React.createClass({
      render: function() {
        return (
          <ol>
          {
            React.Children.map(this.props.children, function (child) {
              return <li>{child}</li>;
            })
          }
          </ol>
        );
      }
    });

    ReactDOM.render(
      <NotesList>
        <span>hello</span>
        <span>world</span>
      </NotesList>,
      document.body
    );
```
上面代码的`NoteList`组件有两个`span`子节点，它们都可以通过`this.props.children`读取。    
这里需要注意，`this.props.children`的值有三种可能：
1. 如果当前组件没有子节点，它就是`undefined`;
2. 如果有一个子节点，数据类型是`object`；
3. 如果有多个子节点，数据类型就是`array`。
所以，处理`this.props.children`的时候要小心。 React提供一个工具方法`React.Children`来处理`this.props.children`。我们可以用`React.Children.map`来遍历子节点，而不用担心`this.props.children`的数据类型是`undefined`还是`object`。  

### PropTypes
组件的属性可以接受任意值，字符串、对象、函数等等都可以。有时，我们需要一种机制，验证别人使用组件时，提供的参数是否符合要求。 组件类的`PropTypes`属性，就是用来验证组件实例的属性是否符合要求。

``` react
    var MyTitle = React.createClass({
      propTypes: {
        title: React.PropTypes.string.isRequired,    // PropTypes 告诉 React，这个 title 属性是必须的，而且它的值必须是字符串。
      },

      render: function() {
        return <h1> {this.props.title} </h1>;
      }
    });
```
此外，getDefaultProps 方法可以用来设置组件属性的默认值。

``` react
    var MyTitle = React.createClass({
      getDefaultProps : function () {
        return {
          title : 'Hello World'
        };
      },

      render: function() {
        return <h1> {this.props.title} </h1>;
      }
    });

    ReactDOM.render(
      <MyTitle />,
      document.body
    );
```

### 获取真实的DOM节点
组件并不是真实的 DOM 节点，而是存在于内存之中的一种数据结构，叫做虚拟 DOM （virtual DOM）。只有当它插入文档以后，才会变成真实的 DOM 。  
根据 React 的设计，所有的 DOM 变动，都先在虚拟 DOM 上发生，然后再将实际发生变动的部分，反映在真实 DOM上，这种算法叫做 DOM diff ，它可以极大提高网页的性能表现。  
但是，有时需要从组件获取真实 DOM 的节点，这时就要用到`ref`属性。

``` react
    var MyComponent = React.createClass({
      handleClick: function() {
        this.refs.myTextInput.focus();
      },
      render: function() {
        return (
          <div>
            <input type="text" ref="myTextInput" />
            <input type="button" value="Focus the text input" onClick={this.handleClick} />
          </div>
        );
      }
    });

    ReactDOM.render(
      <MyComponent />,
      document.getElementById('example')
    );
```

上面代码中，组件`MyComponent`的子节点有一个文本输入框，用于获取用户的输入。这时就必须获取真实的 DOM 节点，虚拟 DOM 是拿不到用户输入的。为了做到这一点，文本输入框必须有一个`ref`属性，然后`this.refs.[refName]`就会返回这个真实的 DOM 节点。  
需要注意的是，由于`this.refs.[refName]`属性获取的是真实 DOM ，所以必须等到虚拟 DOM 插入文档以后，才能使用这个属性，否则会报错。上面代码中，通过为组件指定`Click`事件的回调函数，确保了只有等到真实 DOM 发生`Click`事件之后，才会读取`this.refs.[refName]`属性。
React 组件支持很多事件，除了`Click`事件以外，还有`KeyDown`、`Copy`、`Scroll`等，完整的事件清单请查看官方文档。

### this.state
组件免不了要与用户互动，React 的一大创新，就是将组件看成是一个**状态机**，一开始有一个初始状态，然后用户互动，导致状态变化，从而触发重新渲染 UI。  

``` react
    var LikeButton = React.createClass({
      getInitialState: function() {
        return {liked: false};
      },
      handleClick: function(event) {
        this.setState({liked: !this.state.liked});
      },
      render: function() {
        var text = this.state.liked ? 'like' : 'haven\'t liked';
        return (
          <p onClick={this.handleClick}>
            You {text} this. Click to toggle.
          </p>
        );
      }
    });

    ReactDOM.render(
      <LikeButton />,
      document.getElementById('example')
    );
```
上面代码是一个`LikeButton`组件，它的`getInitialState`方法用于定义初始状态，也就是一个对象，这个对象可以通过`this.state`属性读取。  
当用户点击组件，导致状态变化，`this.setState`方法就修改状态值，每次修改以后，自动调用`this.render`方法，再次渲染组件。  
由于`this.props`和`this.state`都用于描述组件的特性，可能会产生混淆。一个简单的区分方法是，`this.props`表示那些一旦定义，就不再改变的特性，而`this.state`是会随着用户互动而产生变化的特性。

### Form
用户在表单填入的内容，属于用户跟组件的互动，所以不能用`this.props`读取。

``` react
    var Input = React.createClass({
      getInitialState: function() {
        return {value: 'Hello!'};
      },
      handleChange: function(event) {
        this.setState({value: event.target.value});
      },
      render: function () {
        var value = this.state.value;
        return (
          <div>
            <input type="text" value={value} onChange={this.handleChange} />
            <p>{value}</p>
          </div>
        );
      }
    });

    ReactDOM.render(<Input/>, document.body);
```
上面代码中，文本输入框的值，不能用`this.props.value`读取，而要定义一个`onChange`事件的回调函数，通过`event.target.value`读取用户输入的值。`textarea`元素、`select`元素、`radio`元素都属于这种情况，更多介绍请参考官方文档。

### 组建的生命周期
组件的生命周期分成三个状态：
- Mounting：已插入真实 DOM
- Updating：正在被重新渲染
- Unmounting：已移出真实 DOM
React 为每个状态都提供了两种处理函数，`will`函数在进入状态之前调用，`did`函数在进入状态之后调用，三种状态共计五种处理函数。
- componentWillMount()
- componentDidMount()
- componentWillUpdate(object nextProps, object nextState)
- componentDidUpdate(object prevProps, object prevState)
- componentWillUnmount()

此外，React 还提供两种特殊状态的处理函数。
- componentWillReceiveProps(object nextProps)：已加载组件收到新的参数时调用
- shouldComponentUpdate(object nextProps, object nextState)：组件判断是否重新渲染时调用

``` react
    var Hello = React.createClass({
      getInitialState: function () {
        return {
          opacity: 1.0
        };
      },

      componentDidMount: function () {
        this.timer = setInterval(function () {
          var opacity = this.state.opacity;
          opacity -= .05;
          if (opacity < 0.1) {
            opacity = 1.0;
          }
          this.setState({
            opacity: opacity
          });
        }.bind(this), 100);
      },

      render: function () {
        return (
          <div style={{opacity: this.state.opacity}}>
            Hello {this.props.name}
          </div>
        );
      }
    });

    ReactDOM.render(
      <Hello name="world"/>,
      document.body
    );
```
上面代码在hello组件加载以后，通过 componentDidMount 方法设置一个定时器，每隔100毫秒，就重新设置组件的透明度，从而引发重新渲染。  
另外，组件的style属性的设置方式也值得注意，不能写成  

``` react
    style="opacity:{this.state.opacity};"
```
而要写成

``` react
    style={{opacity: this.state.opacity}}
```
这是因为 React 组件样式是一个对象，所以第一重大括号表示这是 JavaScript 语法，第二重大括号表示样式对象。

### Ajax
组件的数据来源，通常是通过 Ajax 请求从服务器获取，可以使用`componentDidMount`方法设置 Ajax 请求，等到请求成功，再用`this.setState`方法重新渲染 UI （查看 demo11 ）。

``` react
    var UserGist = React.createClass({
      getInitialState: function() {
        return {
          username: '',
          lastGistUrl: ''
        };
      },

      componentDidMount: function() {
        $.get(this.props.source, function(result) {
          var lastGist = result[0];
          if (this.isMounted()) {
            this.setState({
              username: lastGist.owner.login,
              lastGistUrl: lastGist.html_url
            });
          }
        }.bind(this));
      },
  
      render: function() {
        return (
          <div>
            {this.state.username}'s last gist is
            <a href={this.state.lastGistUrl}>here</a>.
          </div>
        );
      }
    });

    ReactDOM.render(
      <UserGist source="https://api.github.com/users/octocat/gists" />,
      document.body
    );
```

上面代码使用 jQuery 完成 Ajax 请求，这是为了便于说明。React 本身没有任何依赖，完全可以不用jQuery，而使用其他库。 我们甚至可以把一个Promise对象传入组件  

``` react
    ReactDOM.render(
      <RepoList
        promise={$.getJSON('https://api.github.com/search/repositories?q=javascript&sort=stars')}
      />,
      document.body
    );
```

上面代码从Github的API抓取数据，然后将Promise对象作为属性，传给`RepoList`组件。  
如果Promise对象正在抓取数据（pending状态），组件显示"正在加载"；
如果Promise对象报错（rejected状态），组件显示报错信息；
如果Promise对象抓取数据成功（fulfilled状态），组件显示获取的数据。

``` react
    var RepoList = React.createClass({
      getInitialState: function() {
        return { loading: true, error: null, data: null};
      },

      componentDidMount() {
        this.props.promise.then(
          value => this.setState({loading: false, data: value}),
          error => this.setState({loading: false, error: error}));
      },

      render: function() {
        if (this.state.loading) {
          return <span>Loading...</span>;
        }
        else if (this.state.error !== null) {
          return <span>Error: {this.state.error.message}</span>;
        }
        else {
          var repos = this.state.data.items;
          var repoList = repos.map(function (repo) {
            return (
              <li>
                <a href={repo.html_url}>{repo.name}</a> ({repo.stargazers_count} stars) <br/> {repo.description}
              </li>
            );
          });
          return (
            <main>
              <h1>Most Popular JavaScript Projects in Github</h1>
              <ol>{repoList}</ol>
            </main>
          );
        }
      }
    });
```

### References
1. React's official site
1. React's official examples
1. React (Virtual) DOM Terminology, by Sebastian Markbåge
1. The React Quick Start Guide, by Jack Callister
1. Learning React.js: Getting Started and Concepts, by Ken Wheeler
1. Getting started with React, by Ryan Clark
1. React JS Tutorial and Guide to the Gotchas, by Justin Deal
1. React Primer, by Binary Muse
1. jQuery versus React.js thinking, by zigomir
