## Promises

Essentially, a **promise** is a returned object to which you attach callbacks, instead of passing callbacks into a function.

```js
function successCallback(result) {
  console.log("Audio file ready at URL: " + result);
}

function failureCallback(error) {
  console.error("Error generating audio file: " + error);
}

createAudioFileAsync(audioSettings, successCallback, failureCallback);
```

If `createAudioFileAsync()` were rewritten to return a promise, using it could be as simple as this:

```js
const promise = createAudioFileAsync(audioSettings); 
promise.then(successCallback, failureCallback);
```

or

```js
createAudioFileAsync(audioSettings).then(successCallback, failureCallback);
```

### Guarantees

Unlike old-fashioned *passed-in* callbacks, a promise comes with some guarantees:

- Callbacks will never be called before the [completion of the current run](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop#Run-to-completion) of the JavaScript event loop.
- Callbacks added with `then()`, as above, will be called even *after* the success or failure of the asynchronous operation.
- Multiple callbacks may be added by calling `then()` several times. Each callback is executed one after another, in the order in which they were inserted.

One of the great things about using promises is **chaining**.

### Chaining

In the old days, doing several asynchronous operations in a row would lead to the classic callback pyramid of doom:

```js
doSomething(function(result) {
  doSomethingElse(result, function(newResult) {
    doThirdThing(newResult, function(finalResult) {
      console.log('Got the final result: ' + finalResult);
    }, failureCallback);
  }, failureCallback);
}, failureCallback);	// seeing `failureCallback` three times
```

With modern functions, we attach our callbacks to the returned promises instead, forming a promise chain:

```js
doSomething()
.then(function(result) {
  return doSomethingElse(result);
})
.then(function(newResult) {
  return doThirdThing(newResult);
})
.then(function(finalResult) {
  console.log('Got the final result: ' + finalResult);
})
.catch(failureCallback);
```

The arguments to `then` are optional, and `catch(failureCallback)` is short for `then(null, failureCallback)`. You might see this expressed with [arrow functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions) instead:

```js
doSomething()
.then(result => doSomethingElse(result))
.then(newResult => doThirdThing(newResult))
.then(finalResult => {
  console.log(`Got the final result: ${finalResult}`);
})
.catch(failureCallback);
// Some more `.then()` if needed
```

### Promise rejection events

Whenever a promise is rejected, one of two events (of type [`PromiseRejectionEvent`](https://developer.mozilla.org/en-US/docs/Web/API/PromiseRejectionEvent)) is sent to the global scope (generally, this is either the [`window`](https://developer.mozilla.org/en-US/docs/Web/API/Window) or, if being used in a web worker, it's the [`Worker`](https://developer.mozilla.org/en-US/docs/Web/API/Worker) or other worker-based interface). The two events are:

- [`rejectionhandled`](https://developer.mozilla.org/en-US/docs/Web/API/Window/rejectionhandled_event)

  Sent when a promise is rejected, after that rejection has been handled by the executor's `reject` function.

- [`unhandledrejection`](https://developer.mozilla.org/en-US/docs/Web/API/Window/unhandledrejection_event)

  Sent when a promise is rejected but there is no rejection handler available.

### Creating a Promise around an old callback API

A [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) can be created from scratch using its constructor. This should be needed only to wrap old APIs.

In an ideal world, all asynchronous functions would already return promises. Unfortunately, some APIs still expect success and/or failure callbacks to be passed in the old way. The most obvious example is the [`setTimeout()`](https://developer.mozilla.org/en-US/docs/Web/API/WindowTimers/setTimeout) function:

```js
setTimeout(() => saySomething("10 seconds passed"), 10*1000);
```

Mixing old-style callbacks and promises is problematic. If `saySomething()` fails or contains a programming error, nothing catches it. `setTimeout` is to blame for this.

Luckily we can wrap `setTimeout` in a promise. Best practice is to wrap problematic functions at the lowest possible level, and then never call them directly again:

```js
const wait = ms => new Promise(resolve => setTimeout(resolve, ms));

wait(10*1000).then(() => saySomething("10 seconds")).catch(failureCallback);
```

Basically, the promise constructor takes an executor function that lets us resolve or reject a promise manually. Since `setTimeout()` doesn't really fail, we left out reject in this case.
