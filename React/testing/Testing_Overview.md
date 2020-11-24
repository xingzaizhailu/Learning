## [Testing Overview](https://reactjs.org/docs/testing.html)

### Broadly, two categories:

- **Rendering component trees** in a simplified test environment and asserting on their output.
- **Running a complete app** in a realistic browser environment (AKA “end-to-end” tests).

### Tradeoffs

When choosing testing tools, it's worth considering a few tradeoffs:

- **Iteration speed vs Realistic environment:**
  - Some tools offer a very quick feedback loop between making a change and seeing the result, but don’t model the browser behavior precisely.
  - Other tools might use a real browser environment, but reduce the iteration speed and are flakier on a continuous integration server.
- **How much to mock:** With components, the distinction between a “unit” and “integration” test can be blurry. If you’re testing a form, should its test also test the buttons inside of it? Or should a button component have its own test suite? Should refactoring a button ever break the form test?

Different answers may work for different teams and products.