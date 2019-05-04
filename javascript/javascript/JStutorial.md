## [A re-introduction to JavaScript(JS tutorial)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)

### Overview

#### Number
Numbers in JavaScript are "double-precision 64-bit format so there's no such thing
as an integer in JavaScript.

    0.1 + 0.2 == 0.30000000000000004;

You can also use the unary + operator to convert values to numbers:

    + '42';   // 42
    + '010';  // 10
    + '0x10'; // 16
    + '10.2abc'             // NaN

    parseInt('10.2abc');    // 10
    parseFloat('10.2abc');  // 10.2
NaN is toxic:

    NaN + 5; // NaN

avaScript also has the special values Infinity and -Infinity:

    1 / 0; //  Infinity
    -1 / 0; // -Infinity
    isFinite(1 / 0);  // false

#### Variables
- let allows you to declare block-level variables. The declared variable is
available from the block it is enclosed in.
- const allows you to declare variables whose values are never intended to
change. The variable is available from the block it is declared in.
- A variable declared with the var keyword is available from the function it is
declared in.
- If you declare a variable without assigning any value to it, its type is
undefined.

In JavaScript, blocks do not have scope; only functions have scope.

#### Operators
    123 === '123'; // false
    1 === true;    // false

#### Control structures
JavaScript also contains two other prominent for loops: for...of

    for (let value of array) {
          // do something with value
    }
and for...in:

    for (let property in object) {
          // do something with object property
    }
The && and || operators use short-circuit logic, which means whether they will
execute their second operand is dependent on the first. This is useful for
checking for null objects before accessing their attributes:

    var name = o && o.getName();
Or for caching values (when falsy values are invalid):

    var name = cachedName || (cachedName = getName());

#### Objects
JavaScript objects can be thought of as simple collections of name-value pairs.  
There are two basic ways to create an empty object:

    var obj1 = new Object();
    var obj21 = {};

    var obj22 = {
      name: 'Carrot',
      for: 'Max', // 'for' is a reserved word, use '_for' instead.
      details: {
        color: 'orange',
        size: 12
      }
    }
Attribute access can be chained together:

    obj.details.color; // orange
    obj['details']['size']; // 12

You can iterate over an array using a `for...in`  

If you want to append an item to an array simply do it like this:

    a.push(item1, item2, ...)


#### Functions
``` javascript
    function add(x, y) {
      var total = x + y;
      return total;
    }

    add(2, 3, 4); // => 5
    // added the first two; 4 was ignored
```

``` javascript
    function avg() {
      var sum = 0;
      for (var i = 0, j = arguments.length; i < j; i++) {
        sum += arguments[i];
      }
      return sum / arguments.length;
    }

    avg(2, 3, 4, 5); // 3.5
```
This is pretty useful, but it does seem a little verbose. To diminish this code a bit more we can look at substituting the use of the arguments array through Rest parameter syntax.
In this way we can pass in any number of arguments into the function while keeping our code minimal.
The rest parameter operator is used in function parameter lists with the format: `...variable`

``` javascript
    function avg(...args) {
      var sum = 0;
      for (let value of args) {
        sum += value;
      }
      return sum / args.length;
    }

    avg(2, 3, 4, 5); // 3.5
```
But it does lead us to a new problem. The avg() function takes a comma separated list of arguments —
but what if you want to find the average of an array?
Luckily, JavaScript lets you call a function with an arbitrary array of arguments, using the apply() method of any function object.

    avg.apply(null, [2, 3, 4, 5]); // 3.5

#### Anonymous Functions
``` javascirpt
    var avg = function(){
      ...
      return ...;
    };
```
How do you call them recursively if they don't have a name?

``` javascript
    var charsInBody = (function counter(elm) {
      if (elm.nodeType == 3) { // TEXT_NODE
        return elm.nodeValue.length;
      }
      var count = 0;
      for (var i = 0, child; child = elm.childNodes[i]; i++) {
        count += counter(child);
      }
      return count;
    })(document.body);
```
The name provided to a function expression as above is only available to the function's own scope.  
**Note** that JavaScript functions are themselves objects — like everything else in JavaScript — and you
can add or change properties on them just like we've seen earlier in the Objects section.  

#### Custom objects
Prettry ugli:

``` javascript
    function makePerson(first, last) {
      return {
        first: first,
        last: last
      };
    }
    function personFullName(person) {
      return person.first + ' ' + person.last;
    }
    function personFullNameReversed(person) {
      return person.last + ', ' + person.first;
    }

    s = makePerson('Simon', 'Willison');
    personFullName(s); // "Simon Willison"
    personFullNameReversed(s); // "Willison, Simon"
```

Since functions are objects, this is easy:

``` javascript
    function makePerson(first, last) {
      return {
        first: first,
        last: last,
        fullName: function() {
          return this.first + ' ' + this.last;
        },
        fullNameReversed: function() {
          return this.last + ', ' +
          this.first;
        }
      };
    }

    s = makePerson('Simon', 'Willison');
    s.fullName(); // "Simon Willison"
    s.fullNameReversed(); // "Willison, Simon"
```
We can take advantage of the this keyword to improve our makePerson function:

``` javascript

    function Person(first, last) {
      this.first = first;
      this.last = last;
      this.fullName = function() {
        return this.first + ' ' + this.last;
      };
      this.fullNameReversed = function() {
        return this.last + ', ' + this.first;
      };
    }
    var s = new Person('Simon', 'Willison');
```
We are creating the method functions only once, and assigning references to them
inside the constructor. Can we do any better than that? The answer is yes:

``` javascript
    function Person(first, last) {
      this.first = first;
      this.last = last;
    }
    Person.prototype.fullName = function() {
      return this.first + ' ' + this.last;
    };
    Person.prototype.fullNameReversed = function() {
      return this.last + ', ' + this.first;
    };
```
Person.prototype is an object shared by all instances of Person. It forms part of a lookup chain
(that has a special name, "prototype chain"): any time you attempt to access a property of Person
that isn't set, JavaScript will check Person.prototype to see if that property exists there instead.
As a result, anything assigned to Person.prototype becomes available to all instances of that
constructor via the this object.

##### Inner Functions

#### Closures
``` javascript
    function makeAdder(a) {
      return function(b) {
        return a + b;
      };
    }
    var x = makeAdder(5);
    var y = makeAdder(20);
    x(6); // ? => 11
    y(7); // ? => 27
```



















