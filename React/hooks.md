## Hooks

**What is a Hook?** A Hook is a special function that lets you “hook into” React features. For example, `useState` is a Hook that lets you add React state to function components.

**When would I use a Hook?** If you write a function component and realize you need to add some state to it, previously you had to convert it to a class. Now you can use a Hook inside the existing function component.

Hooks are a way to reuse *stateful logic*, not state itself. The state of each component is completely independent. That's why you can use any hooks in one component multiple times.

### State Hook

We declare a state variable called `count`, and set it to `0`. React will remember its current value between re-renders, and provide the most recent one to our function. If we want to update the current `count`, we can call `setCount`.

**Note:** Unlike `this.setState` in a class, updating a state variable always *replaces* it instead of merging it.

Read: [How React knows which component `useState` corresponds to.](https://reactjs.org/docs/hooks-faq.html#how-does-react-associate-hook-calls-with-components)

### Effect Hook

Effect is short for "side effects". When you call `useEffect`, you’re telling React to run your “effect” function after flushing changes to the DOM.

#### Effects without Cleanup

```javascript
import React, { useState, useEffect } from 'react';
function Example() {
  const [count, setCount] = useState(0);

  useEffect(() => {    document.title = `You clicked ${count} times`;  });
  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}
```

#### Effects with Cleanup

`useEffect` cleans up the previous effects before applying the next effects.

```javascript
import React, { useState, useEffect } from 'react';

function FriendStatus(props) {
  const [isOnline, setIsOnline] = useState(null);

  useEffect(() => {
    function handleStatusChange(status) {
      setIsOnline(status.isOnline);
    }
    ChatAPI.subscribeToFriendStatus(props.friend.id, handleStatusChange);
    
    // Specify how to clean up after this effect:
    return function cleanup() {
      ChatAPI.unsubscribeFromFriendStatus(props.friend.id, handleStatusChange);
    };
  });
  
  if (isOnline === null) {
    return 'Loading...';
  }
  return isOnline ? 'Online' : 'Offline';
}
```

##### Optimizing Performance by Skipping Effects

You can tell React to *skip* applying an effect if certain values haven’t changed between re-renders. To do so, pass an array as an optional second argument to `useEffect`:

```javascript
useEffect(() => {
  document.title = `You clicked ${count} times`;
}, [count]); // Only re-run the effect if count changes
```

If you want to run an effect and clean it up only once (on mount and unmount), you can pass an empty array (`[]`) as a second argument.

### Rules of Hooks

Hooks are JavaScript functions, but they impose two additional rules:

- Only call Hooks **at the top level**.
  - Don’t call Hooks inside loops, conditions, or nested functions.
    - If we want to run an effect conditionally, we can put that condition *inside* our Hook
  - By following this rule, you ensure that Hooks are called in the same order each time a component renders. That’s what allows React to correctly preserve the state of Hooks between multiple `useState` and `useEffect` calls.
- Only call Hooks **from React function components**.
  - Don’t call Hooks from regular JavaScript functions. 
  - There is just one other valid place to call Hooks — your own custom Hooks. 

[Explanation.](https://reactjs.org/docs/hooks-rules.html#explanation)

### Custom Hooks

Custom Hooks are more of a convention than a feature. If a function’s name starts with ”`use`” and it calls other Hooks, we say it is a custom Hook. The `useSomething` naming convention is how our linter plugin is able to find bugs in the code using Hooks.

#### Extracting a Custom Hook

```javascript
import { useState, useEffect } from 'react';

function useFriendStatus(friendID) {  const [isOnline, setIsOnline] = useState(null);

  useEffect(() => {
    function handleStatusChange(status) {
      setIsOnline(status.isOnline);
    }

    ChatAPI.subscribeToFriendStatus(friendID, handleStatusChange);
    return () => {
      ChatAPI.unsubscribeFromFriendStatus(friendID, handleStatusChange);
    };
  });

  return isOnline;
}
```

#### Using a Custom Hook

```javascript
function FriendStatus(props) {
  const isOnline = useFriendStatus(props.friend.id);
  if (isOnline === null) {
    return 'Loading...';
  }
  return isOnline ? 'Online' : 'Offline';
}
```