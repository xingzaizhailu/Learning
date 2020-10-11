### Arrow Functions
#### Basic Syntax
``` javascript
(param1, param2, …, paramN) => { statements }
(param1, param2, …, paramN) => expression
// equivalent to: (param1, param2, …, paramN) => { return expression; }

// Parentheses are optional when there's only one parameter name:
(singleParam) => { statements }
singleParam => { statements }

// A function with no parameters should be written with a pair of parentheses.
() => { statements }
```

#### Advanced Syntax
``` javascript
// Parenthesize the body of function to return an object literal expression:
params => ({foo: bar})

// Rest parameters and default parameters are supported
(param1, param2, ...rest) => { statements }
(param1 = defaultValue1, param2, …, paramN = defaultValueN) => { statements }

// Destructuring within the parameter list is also supported
let f = ([a, b] = [1, 2], {x: c} = {x: a + b}) => a + b + c;
f();  
// 6
```

#### Description
##### Shorter functions
##### No binding of this
###### Relation with strict mode
###### Invoked through call or apply
##### No binding of arguments
##### Arrow functions used as methods
``` javascript
'use strict';
var obj = {
  i: 10,
  b: () => console.log(this.i, this),
  c: function() {
  	console.log(this.i, this);
  }
}
obj.b(); // prints undefined, Window {...} (or the global object)
obj.c(); // prints 10, Object {...}
```

##### Use of the new operator
Arrow functions cannot be used as constructors and will throw an error when used with new.

```javascript
var Foo = () => {};
var foo = new Foo(); // TypeError: Foo is not a constructor
```
##### Use of the prototype property
Arrow functions do not have a prototype property.

```javascript
var Foo = () => {};
console.log(Foo.prototype); // undefined
```

#### Function body
Arrow functions can have either a "concise body" or the usual "block body".  
In a concise body, only an expression is needed, and an implicit return is attached. In a block
body, you must use an explicit return statement.

```javascript
var func = x => x * x;                  
// concise syntax, implied "return"

var func = (x, y) => { return x + y; }; 
// with block body, explicit "return" needed
```
#### Returning object literals
Keep in mind that returning object literals using the concise syntax `params => {object:literal}`
will not work as expected.

```javascript
var func = () => { foo: 1 };               
// Calling func() returns undefined!

var func = () => { foo: function() {} };   
// SyntaxError: function statement requires a name
```
This is because the code inside braces `({})` is parsed as a sequence of statements (i.e. foo is treated like a label, not a key in an object literal).  

Remember to wrap the object literal in parentheses.

```javascript
var func = () => ({foo: 1});
```

#### Line breaksEDIT
An arrow function cannot contain a line break between its parameters and its arrow.

```javascript
var func = ()
           => 1; 
// SyntaxError: expected expression, got '=>'
```
#### Parsing orderEDIT
Although the arrow in an arrow function is not an operator, arrow functions have special parsing rules that interact differently with operator precedence compared to regular functions.  

```javascript
let callback;

callback = callback || function() {}; // ok

callback = callback || () => {};      // SyntaxError: invalid arrow-function arguments

callback = callback || (() => {});    // ok
```

#### More examples
```javascript
// Easy array filtering, mapping, ...

var arr = [5, 6, 13, 0, 1, 18, 23];

var sum = arr.reduce((a, b) => a + b);  
// 66

var even = arr.filter(v => v % 2 == 0); 
// [6, 0, 18]

var double = arr.map(v => v * 2);       
// [10, 12, 26, 0, 2, 36, 46]

// More concise promise chains
promise.then(a => {
    // ...
}).then(b => {
    // ...
});

// Parameterless arrow functions that are visually easier to parse
setTimeout( () => {
    console.log('I happen sooner');
    setTimeout( () => {
        // deeper code
        console.log('I happen later');
    }, 1);
}, 1);
```

