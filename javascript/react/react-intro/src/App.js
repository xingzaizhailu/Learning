import React from 'react';
import logo from './logo.svg';
import './App.css';
import Greetings from './Greetings.js'
import SimpleForm from './SimpleForm'
import RefactForm from './RefactForm'

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
      <body>
          <Greetings firstName="leo" lastName="wu" /> // why not {Greetings('leo', 'wu')} ?
          <SimpleForm />
          <RefactForm />
      </body>
    </div>
  );
}

export default App;
