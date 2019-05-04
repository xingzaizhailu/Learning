from flask import Flask, flash, redirect, render_template, session, abort, url_for
from random import randint

app = Flask(__name__)

@app.route("/")
def index():
    return "Flask App!"

@app.route("/hello")
@app.route("/hello/<string:name>/")
def hello(name=None):
    ## V1
    #return "helo" + name

    ## V2
    #if name:
    #    return render_template('test.html', name=name)
    #else:
    #    return "Hello World!"

    ## V3
    quotes = [ "'If people do not believe that mathematics is simple, it is only because they do not realize how complicated life is.' -- John Louis von Neumann ",
               "'Computer science is no more about computers than astronomy is about telescopes' --  Edsger Dijkstra ",
               "'To understand recursion you must first understand recursion..' -- Unknown",
               "'You look at things that are and ask, why?  I dream of things that never were and ask, why not?' -- Unknown",
               "'Mathematics is the key and door to the sciences.' -- Galileo Galilei",
               "'Not everyone will understand your journey.  Thats fine.  Its not their journey to make sense of.  Its yours.' -- Unknown"
             ]
    randomNumber = randint(0,len(quotes)-1) 
    quote = quotes[randomNumber] 
    return render_template('test.html',**locals())

if __name__ == "__main__":
    #with app.test_request_context():
    #    print(url_for('hello'))
    #    print(url_for('login', next='/'))
    #    print(url_for('profile', username='John Doe'))
    app.run(host='0.0.0.0', port=5000)

    #app.run()
