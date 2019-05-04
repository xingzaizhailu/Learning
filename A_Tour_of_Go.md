## [A Tour of Go](https://tour.golang.org/)

### Basics

#### TODO!!!

#### Zero values
0 for numeric types,
false for the boolean type, and
"" (the empty string) for strings.

#### Type conversions
    var i int = 42
    var f float64 = float64(i)
    var u uint = uint(f)

Or, put more simply:

    i := 42
    f := float64(i)
    u := uint(f)

#### Type inference


#### Switch
Switch cases evaluate cases from top to bottom, stopping when a case succeeds.
A case body breaks automatically. 

    switch os := runtime.GOOS; os {
      case "darwin":
        fmt.Println("OS X.")
      ...

      default:
        ...
    }

##### Switch with no condition

#### Defer
A defer statement defers the execution of a function until the surrounding function returns.
The deferred call's arguments are evaluated immediately, but the function call is not executed until
the surrounding function returns.
##### Stacking defers
Deferred function calls are pushed onto a stack. When a function returns, its deferred calls are
executed in last-in-first-out order.

#### Pointers
its zero value is nil

#### Structs 

    type Name struct {
      Var1 type1
      Var2 type2
    }

    // access by: Name.Var1

#### Arrays

    var a [n]type

    primes := [6]int{2, 3, 5, 7, 11, 13}

#### Slices
A dynamically-sized, flexible view into the elements of an array.

    var s []int = primes[1:4]
##### Slices are like references to arrays
A slice does not store any data, it just describes a section of an underlying array.  

Changing the elements of a slice modifies the corresponding elements of its underlying array.

#### Slice defaults
When slicing, you may omit the high or low bounds to use their defaults instead.  

The default is zero for the low bound and the length of the slice for the high bound.  

For the array

  var a [10]int

these slice expressions are equivalent:

    a[0:10]
    a[:10]
    a[0:]
    a[:]

##### Slice length and capacity
A slice has both a length and a capacity.  
The length of a slice is the number of elements it contains.  
The capacity of a slice is the number of elements in the underlying array, counting from the first
element in the slice.  
The length and capacity of a slice s can be obtained using the expressions len(s) and cap(s).  

##### Creating a slice with make
Slices can be created with the built-in make function; this is how you create dynamically-sized
arrays.  

The make function allocates a zeroed array and returns a slice that refers to that array:  

    a := make([]int, 5)  // len(a)=5

To specify a capacity, pass a third argument to make:

    b := make([]int, 0, 5) // len(b)=0, cap(b)=5
##### Slices of slices
Slices can contain any type, including other slices.

##### Appending to a slice
    var s []int
    printSlice(s)

    // append works on nil slices.
    s = append(s, 0)
    s = append(s, 1, 2, ...)

#### Range
You can skip the index or value by assigning to \_.  
If you only want the index, drop the ", value" entirely.  

    var pow = []int{1, 2, 4, 8, 16, 32, 64, 128}

    func main() {
      for i, v := range pow {
        fmt.Printf("2**%d = %d\n", i, v)
      }
    }

#### Maps
A map maps keys to values.  
The zero value of a map is nil. A nil map has no keys, nor can keys be added.  
The make function returns a map of the given type, initialized and ready for use.  

``` golang
    type Vertex struct {
        Lat, Long float64
    }

    var m map[string]Vertex

    func main() {
        m = make(map[string]Vertex)
        m["Bell Labs"] = Vertex{
            40.68433, -74.39967,
        }
        fmt.Println(m["Bell Labs"])
    }

    // or
    var m = map[string]Vertex{
        "Bell Labs": Vertex{      // and Vertex here is omittable
            40.68433, -74.39967,
        },
        "Google": Vertex{
            37.42202, -122.08408,
        },
    }

    // Delete an element:
    delete(m, key)

    // Test that a key is present with a two-value assignment:
    elem, ok = m[key]
    // If key is in m, ok is true. If not, ok is false.
    // If key is not in the map, then elem is the zero value for the map's element type.
```

#### Function values
Functions are values too. They can be passed around just like other values.  
Function values may be used as function arguments and return values.  

``` golang
  func compute(fn func(float64, float64) float64) float64 {
      return fn(3, 4)
  }

  func main() {
      hypot := func(x, y float64) float64 {
          return math.Sqrt(x*x + y*y)
      }
      fmt.Println(hypot(5, 12))

      fmt.Println(compute(hypot))
      fmt.Println(compute(math.Pow))
  }
```

#### Function closures
Go functions may be closures. A closure is a function value that references variables from outside
its body. The function may access and assign to the referenced variables; in this sense the function
is "bound" to the variables.

For example, the adder function returns a closure. Each closure is bound to its own sum variable.

``` golang
  func adder() func(int) int {
      sum := 0
      return func(x int) int {
          sum += x
          return sum
      }
  }

  func main() {
      pos, neg := adder(), adder()
      for i := 0; i < 10; i++ {
          fmt.Println(
              pos(i),
              neg(-2*i),
          )
      }
  }
```

### Methods and Interfaces
#### Methods
Go does not have classes. However, you can define methods on types.  
A method is a function with a special receiver argument.  
The receiver appears in its own argument list between the func keyword and the method name.

``` golang
    func (v Vertex) Abs() float64 {
        return math.Sqrt(v.X*v.X + v.Y*v.Y)
    }

    func main() {
        v := Vertex{3, 4}
        fmt.Println(v.Abs())
    }
```

##### Methods and pointer indirection
v.Scale(5) as (&v).Scale(5) since the Scale method has a pointer receiver.
``` golang
    func (v *Vertex) Scale(f float64) {
        v.X = v.X * f
        v.Y = v.Y * f
    }

    func ScaleFunc(v *Vertex, f float64) {
        v.X = v.X * f
        v.Y = v.Y * f
    }

    var v Vertex
    ScaleFunc(v)  // Compile error!
    ScaleFunc(&v) // OK

    // while methods with pointer receivers take either a value or a pointer as the receiver when they are called:

    var v Vertex
    v.Scale(5)  // OK
    p := &v
    p.Scale(10) // OK
```

#### Interfaces
``` golang
    type Abser interface {
        Abs() float64
    }

    var a Abser
    f := MyFloat(-math.Sqrt2)
    v := Vertex{3, 4}

    a = f  // a MyFloat implements Abser
    a = &v // a *Vertex implements Abser


    func (f MyFloat) Abs() float64 {
        if f < 0 {
            return float64(-f)
        }
        return float64(f)
    }

    type MyFloat float64

    func (v *Vertex) Abs() float64 {
        return math.Sqrt(v.X*v.X + v.Y*v.Y)
    }
```

##### Interfaces are implemented implicitly
A type implements an interface by implementing its methods.  
There is no explicit declaration of intent, no "implements" keyword.  
Implicit interfaces decouple the definition of an interface from its implementation, which could
then appear in any package without prearrangement.

##### Interface values
##### Interface values with nil underlying values.
##### Nil interface values
A nil interface value holds neither value nor concrete type.  

Calling a method on a nil interface is a run-time error because there is no type inside the
interface tuple to indicate which concrete method to call.
##### The empty interface
The interface type that specifies zero methods is known as the empty interface:  

    interface{}
An empty interface may hold values of any type. (Every type implements at least zero methods.)  
Empty interfaces are used by code that handles values of unknown type. For example, fmt.Print
takes any number of arguments of type interface{}.

#### Type assertions
A type assertion provides access to an interface value's underlying concrete value.

    t := i.(T)

This statement asserts that the interface value i holds the concrete type T and assigns the
underlying T value to the variable t.

If i does not hold a T, the statement will trigger a panic.

``` golang
    var i interface{} = "hello"

    s := i.(string)
    fmt.Println(s)

    f, ok := i.(float64)
    fmt.Println(f, ok)
```

#### Type switches
``` gozlang
    func do(i interface{}) {
        switch v := i.(type) {
          case int:
              fmt.Printf("Twice %v is %v\n", v, v*2)
          case string:
              fmt.Printf("%q is %v bytes long\n", v, len(v))
          default:
              fmt.Printf("I don't know about type %T!\n", v)
        }
    }

    func main() {
        do(21)
        do("hello")
        do(true)
    }

```

#### Stringers
One of the most ubiquitous interfaces is Stringer defined by the fmt package.

    type Stringer interface {
        String() string
    }

    func (p Person) String() string {
        return fmt.Sprintf("%v (%v years)", p.Name, p.Age)
    }

#### Errors
The error type is a built-in interface similar to fmt.Stringer:

``` golang
    type error interface {
        Error() string
    }
```
(As with fmt.Stringer, the fmt package looks for the error interface when printing values.)  
Functions often return an error value, and calling code should handle errors by testing whether
the error equals nil.  
A nil error denotes success; a non-nil error denotes failure.

``` golang
    type MyError struct {
        When time.Time
        What string
    }

    func (e *MyError) Error() string {
        return fmt.Sprintf("at %v, %s",
        e.When, e.What)
    }

    func run() error {
        return &MyError{
            time.Now(),
            "it didn't work",
        }
    }

    func main() {
        if err := run(); err != nil {
            fmt.Println(err)
        }
    }
```

#### Readers
``` golang
    r := strings.NewReader("Hello, Reader!")

    b := make([]byte, 8)
    for {
        n, err := r.Read(b)
        fmt.Printf("n = %v err = %v b = %v\n", n, err, b)
        fmt.Printf("b[:n] = %q\n", b[:n])
        if err == io.EOF {
            break
        }
    }
```

#### rot13Reader
#### Images

### Concurrency
#### Goroutines
A goroutine is a lightweight thread managed by the Go runtime.

``` elixir
    go f(x, y, z)

    // starts a new goroutine running
    f(x, y, z)
```
The evaluation of f, x, y, and z happens in the current goroutine and
the execution of f happens in the new goroutine.  

Goroutines run in the same address space, so access to shared memory must be synchronized. The
sync package provides useful primitives, although you won't need them much in Go as there are
other primitives.

#### Channels
Channels are a typed conduit through which you can send and receive values with the channel
operator, <-.

``` golang
    ch <- v    // Send v to channel ch.
    v := <-ch  // Receive from ch, and
               // assign value to v.
```
(The data flows in the direction of the arrow.)

Like maps and slices, channels must be created before use:

    ch := make(chan int)

By default, sends and receives block until the other side is ready. This allows goroutines to
synchronize without explicit locks or condition variables.

``` golang
    func sum(s []int, c chan int) {
        sum := 0
        for _, v := range s {
            sum += v
        }
        c <- sum // send sum to c
    }

    func main() {
        s := []int{7, 2, 8, -9, 4, 0}

        c := make(chan int)
        go sum(s[:len(s)/2], c)
        go sum(s[len(s)/2:], c)
        x, y := <-c, <-c // receive from c

        fmt.Println(x, y, x+y)
    }
```

#### Buffered Channels
Channels can be buffered. Provide the buffer length as the second argument to make to initialize a
buffered channel:  

    ch := make(chan int, 100)
Sends to a buffered channel block only when the buffer is full. Receives block when the buffer is
empty.  

#### Range and Close
A sender can close a channel to indicate that no more values will be sent. Receivers can test
whether a channel has been closed by assigning a second parameter to the receive expression: after  

    v, ok := <-ch
ok is false if there are no more values to receive and the channel is closed.  

The loop for i := range c receives values from the channel repeatedly until it is closed.  

Note: Only the sender should close a channel, never the receiver. Sending on a closed channel will
cause a panic.  

Another note: Channels aren't like files; you don't usually need to close them. Closing is only
necessary when the receiver must be told there are no more values coming, such as to terminate a
range loop.  

``` golang
    func fibonacci(n int, c chan int) {
        x, y := 0, 1
        for i := 0; i < n; i++ {
            c <- x
            x, y = y, x+y
        }
        close(c)
    }

    func main() {
        c := make(chan int, 10)
        go fibonacci(cap(c), c)
        for i := range c {
            fmt.Println(i)
        }
    }
```

#### Select
The select statement lets a goroutine wait on multiple communication operations.  

A select blocks until one of its cases can run, then it executes that case. It chooses one at random
if multiple are ready.  

``` golang
    func fibonacci(c, quit chan int) {
        x, y := 0, 1
        for {
            select {
                case c <- x:
                    x, y = y, x+y
                case <-quit:
                    fmt.Println("quit")
                    return
            }
        }
    }

    func main() {
        c := make(chan int)
        quit := make(chan int)
        go func() {
            for i := 0; i < 10; i++ {
                fmt.Println(<-c)
            }
            quit <- 0
        }()
        fibonacci(c, quit)
    }
```

##### Default Selection
The default case in a select is run if no other case is ready.  
Use a default case to try a send or receive without blocking:  

``` golang
    select {
        case i := <-c:
            // use i
        default:
            // receiving from c would block
    }

    //
    func main() {
        tick := time.Tick(100 * time.Millisecond)
        boom := time.After(500 * time.Millisecond)
        for {
            select {
                case <-tick:
                    fmt.Println("tick.")
                case <-boom:
                    fmt.Println("BOOM!")
                    return
                default:
                    fmt.Println("    .")
                    time.Sleep(50 * time.Millisecond)
            }
        }
    }
```

#### sync.Mutex
We've seen how channels are great for communication among goroutines.  
But what if we don't need communication? What if we just want to make sure
only one goroutine can access a variable at a time to avoid conflicts?  
This concept is called mutual exclusion, and the conventional name for
the data structure that provides it is mutex.  
Go's standard library provides mutual exclusion with sync.Mutex and its two methods:  

    Lock
    Unlock
We can define a block of code to be executed in mutual exclusion by
surrounding it with a call to Lock and Unlock as shown on the Inc method.  
We can also use defer to ensure the mutex will be unlocked as in the Value method.

``` golang
    import (
        "fmt"
        "sync"
        "time"
    )

    // SafeCounter is safe to use concurrently.
    type SafeCounter struct {
        v   map[string]int
        mux sync.Mutex
    }

    // Inc increments the counter for the given key.
    func (c *SafeCounter) Inc(key string) {
        c.mux.Lock()
        // Lock so only one goroutine at a time can access the map c.v.
        c.v[key]++
        c.mux.Unlock()
    }

    // Value returns the current value of the counter for the given key.
    func (c *SafeCounter) Value(key string) int {
        c.mux.Lock()
        // Lock so only one goroutine at a time can access the map c.v.
        defer c.mux.Unlock()
        return c.v[key]
    }

    func main() {
        c := SafeCounter{v: make(map[string]int)}
        for i := 0; i < 1000; i++ {
            go c.Inc("somekey")
        }

        time.Sleep(time.Second)
        fmt.Println(c.Value("somekey"))
    }
```
