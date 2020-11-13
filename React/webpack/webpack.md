## 
### Installation
install Webpack and webpack-dev-server globally.

    $ npm i -g webpack@1.x webpack-dev-server@1.x
Then, clone the repo and install the dependencies.

    $ cd webpack-demos
    $ npm install
Now, play with the source files under the repo's demo\* directories.

    $ cd demo01
    $ webpack-dev-server [--host 192.168.70.200 -- port 6120]
    Visit http://127.0.0.1:8080 with your browser.

### Foreword: What is Webpack
Webpack is a front-end build systems like Grunt and Gulp.
It can be used as a module bundler similar to Browserify, and do much more.

``` bash
    $ browserify main.js > bundle.js
    # be equivalent to
    $ webpack main.js bundle.js
```
Its configuration file is webpack.config.js.

``` javascript
    // webpack.config.js
    module.exports = {
      entry: './main.js',
      output: {
        filename: 'bundle.js'
      }
    };
```
After having webpack.config.js, you can invoke Webpack without any arguments.

    $ webpack
Some command-line options you should know.

- webpack          – for building once for development
- webpack -p       – for building once for production (minification)
- webpack --watch  – for continuous incremental build
- webpack -d       – to include source maps
- webpack --colors – for making things pretty
To produce a production ready application, you could write scripts field in your package.json file as following.

``` json
    // package.json
    {
      // ...
      "scripts": {
        "dev": "webpack-dev-server --devtool eval --progress --colors",
        "deploy": "NODE_ENV=production webpack -p"
      },
      // ...
    }
```

### Demo01: Entry file
Entry file is a file which Webpack will read to build bundle.js.

For example, main.js is an entry file.

    // main.js
    document.write('<h1>Hello World</h1>');
index.html

    <html>
      <body>
        <script type="text/javascript" src="bundle.js"></script>
      </body>
    </html>
Webpack follows webpack.config.js to build bundle.js.

    // webpack.config.js
    module.exports = {
      entry: './main.js',
      output: {
        filename: 'bundle.js'
      }
    };
Launch the server, visit http://127.0.0.1:8080 .

$ webpack-dev-server

### Demo02: Multiple entry files
    <body>
      <script src="bundle1.js"></script>
      <script src="bundle2.js"></script>
    </body>

    entry: {
      bundle1: './main1.js',
      bundle2: './main2.js'
    },
### Demo03: Babel-loader
    module.exports = {
      entry: './main.jsx',
      output: {
        filename: 'bundle.js'
      },
      module: {
        loaders:[
          {
            test: /\.js[x]?$/,
            exclude: /node_modules/,
            loader: 'babel-loader?presets[]=es2015&presets[]=react'
          },
        ]
      }
    };

    module: {
      loaders: [
        {
          test: /\.jsx?$/,
          exclude: /node_modules/,
          loader: 'babel-loader',
          query: {
            presets: ['es2015', 'react']
          }
        }
      ]
    }
### Demo04: CSS-loader
    loaders:[
      { test: /\.css$/, loader: 'style-loader!css-loader' },
    ]
### Demo05: Image loader
    #main.js

    var img1 = document.createElement("img");
    img1.src = require("./small.png");
    document.body.appendChild(img1);

    # webpack.config.js
    { test: /\.(png|jpg)$/, loader: 'url-loader?limit=8192' }
### Demo06: CSS Module
    #app.css

    .h1 {
      color:red;
    }

    :global(.h2) {
      color: blue;
    }
#### Demo07: UglyfyJs Plugin
    # main.js
    var longVariableName = 'Hello';
    longVariableName += ' World';
    document.write('<h1>' + longVariableName + '</h1>');

    # webpack.config.js
    var webpack = require('webpack');
    var uglifyJsPlugin = webpack.optimize.UglifyJsPlugin;
    module.exports = {
      entry: './main.js',
      output: {
        filename: 'bundle.js'
      },
      plugins: [
        new uglifyJsPlugin({
          compress: {
            warnings: false
          }
        })
      ]
    };

After launching the server, main.js will be minified into following.

    var o="Hello";o+=" World",document.write("<h1>"+o+"</h1>")
#### Demo08: HTML Webpack Plugin and Open Browser Webpack Plugin
This demo shows you how to load 3rd-party plugins.
html-webpack-plugin could create index.html for you, and open-browser-webpack-plugin could open a
new browser tab when Webpack loads.

    var HtmlwebpackPlugin = require('html-webpack-plugin');
    var OpenBrowserPlugin = require('open-browser-webpack-plugin');

    plugins: [
      HtmlwebpackPlugin({
        title: 'Webpack-demos',
        filename: 'index.html'
      }),
      new OpenBrowserPlugin({
        url: 'http://localhost:8080'
      })
    ]

#### Demo09: Environment flags
    #main.js
    document.write('<h1>Hello World</h1>');

    if (__DEV__) {
          document.write(new Date());
    }

    # webpack.config.js
    var webpack = require('webpack');

    var devFlagPlugin = new webpack.DefinePlugin({
      __DEV__: JSON.stringify(JSON.parse(process.env.DEBUG || 'false'))
    });

    module.exports = {
      entry: './main.js',
      output: {
        filename: 'bundle.js'
      },
      plugins: [devFlagPlugin]
    };

    Now pass environment variable into webpack.

    # Linux & Mac
    $ env DEBUG=true webpack-dev-server

#### Demo10: Code Splitting
    // main.js
    require.ensure(['./a'], function(require) {
      var content = require('./a');
      document.open();
      document.write('<h1>' + content + '</h1>');
      document.close();
    });

    module.exports = 'Hello World';
#### Demo11: Code splitting with bundle-loader
Another way of code splitting is using bundle-loader.

    // main.js

    // Now a.js is requested, it will be bundled into another file
    var load = require('bundle-loader!./a.js');

    // To wait until a.js is available (and get the exports)
    //  you need to async wait for it.
    load(function(file) {
      document.open();
      document.write('<h1>' + file + '</h1>');
      document.close();
    });
require('bundle-loader!./a.js') tells Webpack to load a.js from another chunk.  

Now Webpack will build main.js into bundle.js, and a.js into 1.bundle.js.

#### Demo12: Common chunk
When multi scripts have common chunks, you can extract the common part into a separate file with
CommonsChunkPlugin.

    // main1.jsx
    var React = require('react');
    var ReactDOM = require('react-dom');

    ReactDOM.render(
      <h1>Hello World</h1>,
      document.getElementById('a')
    );

    // main2.jsx
    var React = require('react');
    var ReactDOM = require('react-dom');

    ReactDOM.render(
      <h2>Hello Webpack</h2>,
      document.getElementById('b')
    );

    // index.html
    <html>
      <body>
        <div id="a"></div>
        <div id="b"></div>
        <script src="init.js"></script>
        <script src="bundle1.js"></script>
        <script src="bundle2.js"></script>
      </body>
    </html>

    // webpack.config.js
    var CommonsChunkPlugin = require("webpack/lib/optimize/CommonsChunkPlugin");
    module.exports = {
      entry: {
        bundle1: './main1.jsx',
        bundle2: './main2.jsx'
      },
      output: {
        filename: '[name].js'
      },
      module: {
        loaders:[
          {
            test: /\.js[x]?$/,
            exclude: /node_modules/,
            loader: 'babel-loader',
            query: {
              presets: ['es2015', 'react']
            }
          },
        ]
      },
      plugins: [
        new CommonsChunkPlugin('init.js')
      ]
    }

#### Demo13: Vendor chunk
main.js

var $ = require('jquery');
$('h1').text('Hello World');
index.html

    <html>
      <body>
        <h1></h1>
        <script src="vendor.js"></script>
        <script src="bundle.js"></script>
      </body>
    </html>

    // webpack.config.js
    var webpack = require('webpack');
    module.exports = {
      entry: {
        app: './main.js',
        vendor: ['jquery'],
      },
      output: {
        filename: 'bundle.js'
      },
      plugins: [
        new webpack.optimize.CommonsChunkPlugin(/* chunkName= */'vendor', /* filename= */'vendor.js')
      ]
    };
If you want a module available as variable in every module, such as making $ and jQuery available in every module without writing require("jquery"). You should use ProvidePlugin.

    // main.js
    $('h1').text('Hello World');


    // webpack.config.js
    var webpack = require('webpack');

    module.exports = {
      entry: {
        app: './main.js'
      },
      output: {
        filename: 'bundle.js'
      },
      plugins: [
        new webpack.ProvidePlugin({
          $: "jquery",
          jQuery: "jquery",
          "window.jQuery": "jquery"
        })
      ]
    };
#### Demo14: Global variables
If you want to use some global variables, and don't want to include them in the Webpack bundle, you can enable externals field in webpack.config.js

    // webpack.config.js
    externals: {
      // require('data') is external and available
      //  on the global var data
      'data': 'data'
    }
Now, you require data as a module variable in your script. but it actually is a global variable.

    // main.jsx
    var data = require('data');
    var React = require('react');
    var ReactDOM = require('react-dom');

    ReactDOM.render(
      <h1>{data}</h1>,
      document.body
    );

#### Demo15: Hot Module Replacement
##### Specify --hot and --inline on the command line
    $ webpack-dev-server --hot --inline
Meaning of the options:
- --hot: adds the HotModuleReplacementPlugin and switch the server to hot mode.
- --inline: embed the webpack-dev-server runtime into the bundle.
- --hot --inline: also adds the webpack/hot/dev-server entry.
##### Modify webpack.config.js.
- add new webpack.HotModuleReplacementPlugin() to the plugins field
- add webpack/hot/dev-server and webpack-dev-server/client?http://localhost:8080 to the entry field

webpack.config.js looks like the following.

    module.exports = {
      entry: [
        'webpack/hot/dev-server',
        'webpack-dev-server/client?http://localhost:8080',
        './index.js'
      ],
      output: {
        filename: 'bundle.js',
        publicPath: '/static/'
      },
      plugins: [
        new webpack.HotModuleReplacementPlugin()
      ],
      module: {
        loaders: [{
          test: /\.jsx?$/,
          exclude: /node_modules/,
          loader: 'babel-loader',
          query: {
            presets: ['es2015', 'react']
          },
          include: path.join(__dirname, '.')
        }]
      }
    };

#### Demo16: React router
