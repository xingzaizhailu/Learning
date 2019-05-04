## React

#### Using State Correctly
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

Because this.props and this.state may be updated asynchronously, you should not rely on their values for calculating the next state.  

For example, this code may fail to update the counter:

``` javascript
    // Wrong
    this.setState({
      counter: this.state.counter + this.props.increment,
    });
```
To fix it, use a second form of setState() that accepts a function rather than an object. That function will receive the previous state as the first argument, and the props at the time the update is applied as the second argument:  

``` javascript
    // Correct
    this.setState((prevState, props) => ({
      counter: prevState.counter + props.increment
    }));
```
We used an arrow function above, but it also works with regular functions:

``` javascirpt
    // Correct
    this.setState(function(prevState, props) {
      return {
        counter: prevState.counter + props.increment
      };
    });
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
The merging is shallow, so this.setState({comments}) leaves this.state.posts intact, but completely
replaces this.state.comments.
