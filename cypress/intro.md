## Introduction

### Set up

```bash
$ npm install cypress --save-dev
$ node_modules/.bin/cypress open
```

### First Test

```javascript
describe('Test_suit', function() {
  beforeEach(() => {
    cy.visit('https://...')
  })
  
	it('Test_case', function() {
		// Act & Assert
    expect(true).to.equal(false)
		cy.contains('type').click()
		cy.url().should('include', '/some/url')
		
		cy.get('.class-name')
		  .type('some content')
		  .should('have.value', 'some content')
	})
})
```

**Cypress output additional information in the console:**

- Command (that was issued)
- Yielded (what was returned by this command)
- Elements (the number of elements found)
- Selector (the argument we used)

### Testing Strategies

#### Stub Requests

Stub the JSON responses coming from `cy.visit()`.

- :) Fast, easy, Flexible
- :) No Server/ DB
- :| Requires Fixtures
- :( Not true E2E

**Tip:** 

1. You could have the server generate all of the fixture stubs for you ahead of time so that data will reflect what the server will actually send.
2. Or a more balanced approach - write a single e2e test without stubs, and then stub the rest.

#### Static User

- :) Real Session E2E
- :| Requires Server
- :| Seed the DB
- :( Shares Test State

##### Seeding data

- `cy.exec()` - to run system commands
  - e.g. `cy.exec('npm run db:reset && npm run db:seed'`
- `cy.task()` - to run code in Node via the `PluginsFile`
- `cy.request()` - to make HTTP requests

#### Dynamic User

- :) No State Mutations
- :) Flexible / Powerful
- :| DB Setup / Teardown
- :( Slow / Complex

### Some Practices

- Don't use the UI to build up state. Set state directly/ programmatically
  - For example, fully test the login flow – but only once!

- Don't use page objects to share UI knowledge. Write specs in isolation, avoid coupling.

- Don't limit yourself trying to act like a user. You have native access to everything.
  - Control time: `cy.clock()`
  - Stub Objects: `cy.stub()`
  - Modify Stores: `cy.window()`
    - ![](/Users/leo/Desktop/Screen Shot 2021-02-18 at 15.46.35.png)