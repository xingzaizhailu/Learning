## [Testing Recipes](https://reactjs.org/docs/testing-recipes.html)

**Jest:** a JavaScript test runner that lets you access the DOM via [`jsdom`](https://reactjs.org/docs/testing-environments.html#mocking-a-rendering-surface). While jsdom is only an approximation of how the browser works, it is often good enough for testing React

[**React Testing Library**](https://testing-library.com/docs/react-testing-library/intro/)

### [Setup/Teardown](https://reactjs.org/docs/testing-recipes.html#setup--teardown)

For each test, we usually want to render our React tree to a DOM element that’s attached to `document`, so that it can receive DOM events. When the test ends, we want to “clean up” and unmount the tree from the `document`.

A common way to do it is to use a pair of `beforeEach` and `afterEach` blocks so that they’ll always run and isolate the effects of a test to itself:

```javascript
import { unmountComponentAtNode } from "react-dom";

let container = null;
beforeEach(() => {
  // setup a DOM element as a render target
  container = document.createElement("div");
  document.body.appendChild(container);
});

afterEach(() => {
  // cleanup on exiting
  unmountComponentAtNode(container);
  container.remove();
  container = null;
});
```

**Note:** We want to execute the cleanup *even if a test fails*. Otherwise, one test can change the behaviour of another test.

### [`act()`](https://reactjs.org/docs/testing-recipes.html#act)

When writing UI tests, tasks like rendering, user events, or data fetching can be considered as “units” of interaction with a user interface.  [`act()`](https://reactjs.org/docs/test-utils.html#act) in `react-dom/test-utils` makes sure all updates related to these “units” have been processed and applied to the DOM before you make any assertions:

```javascript
act(() => {
  // render components
});
// make assertions
```

**Note:** The name `act` comes from the [Arrange-Act-Assert](http://wiki.c2.com/?ArrangeActAssert) pattern.

### [Rendering](https://reactjs.org/docs/testing-recipes.html#rendering)

```javascript
import React from "react";
import { render, unmountComponentAtNode } from "react-dom";
import { act } from "react-dom/test-utils";

import Hello from "./hello";

it("renders with or without a name", () => {
  act(() => { render(<Hello />, container); });
  expect(container.textContent).toBe("Hey, stranger");
  
  act(() => {
    render(<Hello name="Jenny" />, container);
  });
  expect(container.textContent).toBe("Hello, Jenny!");

  act(() => {
    render(<Hello name="Margaret" />, container);
  });
  expect(container.textContent).toBe("Hello, Margaret!");
});
```

### [Data fetching](https://reactjs.org/docs/testing-recipes.html#data-fetching)

Mocking data fetching with “fake” data prevents flaky tests due to an unavailable backend, and makes them run faster. 

```javascript
// in User component, it fetches user data by userId

it("renders user data", async () => {
  const fakeUser = { name: "Joni Baez", age: "32", address: "123, Charming Avenue" };
  jest.spyOn(global, "fetch").mockImplementation(() =>
    Promise.resolve({
      json: () => Promise.resolve(fakeUser)
    })
  );
  
  // Use the asynchronous version of act to apply resolved promises
  await act(async () => {
    render(<User id="123" />, container);
  });

  expect(container.querySelector("some_tag").textContent).toBe(fakeUser.name);
  expect(container.querySelector("another_tag").textContent).toBe(fakeUser.age);
  expect(container.textContent).toContain(fakeUser.address);

  // remove the mock to ensure tests are completely isolated
  global.fetch.mockRestore();
});
```

**Note:** you may still want to run a subset of tests using an [“end-to-end”](https://reactjs.org/docs/testing-environments.html#end-to-end-tests-aka-e2e-tests) framework that tells whether the whole app is working together.

### [Mocking Modules](https://reactjs.org/docs/testing-recipes.html#mocking-modules)

```javascript
import MockedMap from "./map";

jest.mock("./map", () => {
	return function DummyMap(props) {
		return (
			<div data-testid="map">
				{props.center.lat}:{props.center.long}
			</div>
		);
	};
});
```

### [Events](https://reactjs.org/docs/testing-recipes.html#events)

Dispatching real DOM events on DOM elements is recommended, and then asserting on the result.

```javascript
it("changes value when clicked", () => {
  const onChange = jest.fn();
  act(() => {
    render(<Toggle onChange={onChange} />, container);
  });

  // get ahold of the button element, and trigger some clicks on it
  const button = document.querySelector("[data-testid=toggle]");
  expect(button.innerHTML).toBe("Turn on");

  act(() => {
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  });
  expect(onChange).toHaveBeenCalledTimes(1);
  expect(button.innerHTML).toBe("Turn off");

  act(() => {
    for (let i = 0; i < 5; i++) {
      button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    }
  });

  expect(onChange).toHaveBeenCalledTimes(6);
  expect(button.innerHTML).toBe("Turn on");
});
```

Different DOM events and their properties are described in [MDN](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent).

**Note** that you need to pass `{ bubbles: true }` in each event you create for it to reach the React listener because React automatically delegates events to the root.

### [Timers](https://reactjs.org/docs/testing-recipes.html#timers)

Testing code uses timer-based functions like `setTimeout`.

```javascript
jest.useFakeTimers();

it("should select null after timing out", () => {
  const onSelect = jest.fn();
  act(() => {
    render(<Card onSelect={onSelect} />, container);
  });

  // move ahead in time by 100ms
  act(() => {
    jest.advanceTimersByTime(100);
  });
  expect(onSelect).not.toHaveBeenCalled();

  // and then move ahead by 5 seconds
  act(() => {
    jest.advanceTimersByTime(5000);
  });
  expect(onSelect).toHaveBeenCalledWith(null);
});

it("should cleanup on being removed", () => {
  const onSelect = jest.fn();
  act(() => {
    render(<Card onSelect={onSelect} />, container);
  });
  act(() => {
    jest.advanceTimersByTime(100);
  });
  expect(onSelect).not.toHaveBeenCalled();

  // unmount the app
  act(() => {
    render(null, container);
  });
  act(() => {
    jest.advanceTimersByTime(5000);
  });
  expect(onSelect).not.toHaveBeenCalled();
});

it("should accept selections", () => {
  const onSelect = jest.fn();
  act(() => {
    render(<Card onSelect={onSelect} />, container);
  });

  act(() => {
    container
      .querySelector("[data-testid='2']")
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
  });

  expect(onSelect).toHaveBeenCalledWith(2);
});
```

### [snapshot-testing](https://reactjs.org/docs/testing-recipes.html#snapshot-testing)

Save “snapshots” of data with [`toMatchSnapshot` / `toMatchInlineSnapshot`](https://jestjs.io/docs/en/snapshot-testing).

With these, we can “save” the rendered component output and ensure that a change to it has to be explicitly committed as a change to the snapshot.

It’s typically better to make more specific assertions than to use snapshots. These kinds of tests include implementation details so they break easily, and teams can get desensitized to snapshot breakages. Selectively [mocking some child components](https://reactjs.org/docs/testing-recipes.html#mocking-modules) can help reduce the size of snapshots and keep them readable for the code review.

### [multiple-renderers](https://reactjs.org/docs/testing-recipes.html#multiple-renderers)

