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

TODO

### Timeouts

