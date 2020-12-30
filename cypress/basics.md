## Basics

### Querying Elements

#### By Class Name

Cypress bundles jQuery, and it also wraps all DOM queries with retry-and-timeout logic.

```javascript
// Each method is equivalent to its jQuery counterpart. Use what you know!
cy.get('#main-content')
  .find('.article')
  .children('img[src^="/static"]')
  .first()
```

To interact with a DOM element directly, call `.then()` with a callback function that receives the element as its first argument.

To skip the retry-and-timeout functionality and perform traditional synchrounous work, use `Cypress.$` which will return an empty collection immediately when not found.

[Best practice for selecting elements.](https://docs.cypress.io/guides/references/best-practices.html#Selecting-Elements)

#### By Text Content

```javascript
cy.contains('New Post')

// Find an element within '.main' containing the text 'New Post'
cy.get('.main', { timeout: 4000 })
  .contains('New Post')
```

### Chains of Commands

#### [Interacting with Elements](https://docs.cypress.io/guides/core-concepts/interacting-with-elements.html)

You can chaining the following commands onto `cy.get()` or `cy.contains()`:

- `.click()`
- `.type()`
- `.blur()` - Make a focused DOM element blur
- `.focus()`
- `.clear()` - Clear the value of an input or textarea
- `.check()` - Check checkbox(es) or radio(s)
- `.uncheck()`
- `.select()`
- `.dbclick()`
- `.rightclick()`

When Cypress run `.click()`, it will ensure that the element is able to be interacted with (like a real user would):

- Not being hidden
- Not being covered
- Not being disabled
- Not animating

#### Asserting about Elements

```javascript
cy.get(':checkbox').should('be.disabled')
cy.get('form').should('have.class', 'form-horizontal')
cy.get('input').should('not.have.value', 'US')
```

#### Subject Management

##### Some commands can be chained from ...

- `cy` only, meaning they don't operate on a subject: `cy.clearCookies()`
- commands yielding particular kinds of subjects (like DOM elements): `.type()`, `.click`
- both: `.contains()`

##### Some commands yield...

- `null`, meaning no commands can be chained after the command: `cy.clearCookies()`
- the same subject they were originally yielded: `.click()`
- a new subject, as appropriate for the command `.wait()`

##### Using Aliases to Refer to Previous Subjects

Quickly referring back to past subjects.

```javascript
cy
  .get('.my-selector')
  .as('myElement') // sets the alias
  .click()

/* many more actions */

cy
  .get('@myElement') // re-queries the DOM as before (only if necessary)
  .click()
```

#### Commands are Asynchronous

Cypress commands don't do anything at the moment they are invoked, but enqueue themselves to be run faster.

So don't write synchronous code between Cypress commands, wrap them in a `.then()`

### Assertions

#### Default Assertions

- `cy.visit()` expects the page to send `text/html` content with a `200` status code
- `cy.request()` expects the remote server to exist and provide a response
- `cy.get()`,   `cy.contains()` and `.find()` expect the element to eventually exist in the DOM
- `.type()` expects eventually typeable
- `.its()` expects to eventually find a property on the current subject

#### Reversing the default Assertion

```javascript
// wait until this button is not in the DOM after the click
cy.get('button.close').click().should('not.exist')
```

#### Writing Assertions

Two ways

1. Implicit Subjects: Using `.should()` or `.and()`
2. Explicit Subjects: Using `expect`
   1. When to use?
      1. Assert multiple things about the same subject
      2. Massage the subject in some way prior to making the assertion

##### A complex example

```javascript
cy.get('p')
  .should(($p) => {
    // massage our subject from a DOM element
    // into an array of texts from all of the p's
    let texts = $p.map((i, el) => {
      return Cypress.$(el).text()
    })

    // jQuery map returns jQuery object
    // and .get() converts this to an array
    texts = texts.get()

    // array should have length of 3
    expect(texts).to.have.length(3)

    // with this specific content
    expect(texts).to.deep.eq([
      'Some text from first p',
      'More text from second p',
      'And even more text from third p'
    ])
  })
```

### Timeouts

Timeouts can be modified per command and this will affect all default assertions and any assertions chained after that command.

```javascript
// we've modified the timeout which affects default + added assertions
cy.get('.mobile-nav', { timeout: 10000 })			// waits up to 10 seconds for it to exist in the DOM
  .should('be.visible')												// waits up to 10 seconds for it to be visible
  .and('contain', 'Home')											// waits up to 10 seconds for it to contain the text: ‘Home’
```

