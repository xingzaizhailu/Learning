## Main concepts

### JSX

Always receive user input after rendered. 

Safe

### React elements

### Components

**All React components must act like pure functions with respect to their props.**

### Props

### State

Three things to know:

##### Do Not Modify State Directly

For example, this will not re-render a component:

``` javascript
// Wrong
this.state.comment = 'Hello';

// Instead, use setState():
// Correct
this.setState({comment: 'Hello'});
```

The only place where you can assign this.state is the constructor.

##### State Updates May Be Asynchronous

React may batch multiple `setState()` calls into a single update for performance.

Because `this.props` and `this.state` may be updated asynchronously, you should not rely on their values for calculating the next state.  

For example, this code may fail to update the counter:

``` javascript
// Wrong
this.setState({
  counter: this.state.counter + this.props.increment,
});
```

To fix it, use a second form of `setState()` that accepts a function rather than an object. That function will receive the previous state as the first argument, and the props at the time the update is applied as the second argument:  

``` javascript
// Correct
this.setState((prevState, props) => ({
  counter: prevState.counter + props.increment
}));
```

##### State Updates are Merged

When you call `setState()`, React merges the object you provide into the current state. 
For example, your state may contain several independent variables:

``` javascript
constructor(props) {
  super(props);
  this.state = {
    posts: [],
    comments: []
  };
}
```

Then you can update them independently with separate setState() calls:

``` javascript
componentDidMount() {
  fetchPosts().then(response => {
    this.setState({
      posts: response.posts
    });
  });

  fetchComments().then(response => {
    this.setState({
      comments: response.comments
    });
  });
}
```

The merging is shallow, so `this.setState({comments})` leaves `this.state.posts` intact, but completely replaces `this.state.comments`.

### Lifecycle

1. Component is passed to ReactDOM.render().
   1. React calls the constructor of the component.
   2. Initialised `this.state`
2. React calls the component's `render()` method. This is how React learns what should be displayed on the screen.
3. When the component output is inserted in the DOM, React calls the `componentDidMount()` lifecycle method.
4. `render()` will be called again when state changes. React updates the DOM accordingly.
5. `componentWillUnmount()` lifecycle method is called when the component is ever removed from the DOM.

#### Others

- `componentWillReceivePorps(nextProps) {}`
- `shouldComponentUpdate(nextProps, nextState) {}`  - should return `true` or `false`
- `componentDidUpdate(prevProps, prevState) {}`

### [Handling Events](https://reactjs.org/docs/handling-events.html)

Handling events with React elements is very similar to handling events on DOM elements. There are some syntax differences:

- React events are named using camelCase, rather than lowercase.
- With JSX you pass a function as the event handler, rather than a string.

### Conditional Rendering

1. If ... else
2. `&&` operator: `{ expression && <>...</> }`
3. `... ? ... : ...`

**Tip:** You can use a prop to show or hide (return `null`) a component.

### List and Keys

- Don't use index as keys if the order of items may change.

- A good rule of thumb is that elements inside the `map()` call need keys
  - e.g. put key in your extracted component `<ListItem />` rather than the element `<li>`. 

- Keys Must Only Be Unique Among Siblings

### Forms

#### Controlled Components

In HTML, form elements such as `<input>`, `<textarea>`, and `<select>` typically maintain their own state and update it based on user input. In React, mutable state is typically kept in the state property of components, and only updated with [`setState()`](https://reactjs.org/docs/react-component.html#setstate).

We can combine the two by making the React state be the “single source of truth”. Then the React component that renders a form also controls what happens in that form on subsequent user input. An input form element whose value is controlled by React in this way is called a **“controlled component”**.

```javascript
class NameForm extends React.Component {
  ...

  render() {
    return (
      <form onSubmit={this.handleSubmit}>
        <label>
          Name:
          <input type="text" value={this.state.value} onChange={this.handleChange} />
        </label>
        <input type="submit" value="Submit" />
      </form>
    );
  }
}
```

#### The `select` tag

```javascript
<select value={this.state.value} onChange={this.handleChange}>
	<option value="grapefruit">Grapefruit</option>
  <option value="lime">Lime</option>
  <option value="mango">Mango</option>
</select>

<select multiple={true} value={['B', 'C']}>
```

#### [Uncontrolled Components](https://reactjs.org/docs/uncontrolled-components.html)

#### [Formik](https://formik.org)- A Fully-Fledged Solutions

### Lifting State Up

