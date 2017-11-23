# The Pragmatic Programmer
## Chap:1 A Pragmatic Philosophy
### The Cat Ate My Source Code
#### Take Responsibility
Provide solutions, not excuses when make some mistakes what everyone may do.
Tip 3: Provide Options, Don't Make Lame Excuses
### Software Entropy
Tip 4: Don't Live with Broken Windows
Fix each one as soon as it is discovered( bad designs, wrong decisions, or poor code)
#### Putting Out Fires
### Stone Soup and Boiled Frogs
Tip 5: Be a Catalyst for Change
#### The Villagers' Side
Tip 6: Remember the Big Picture
### Good-Enough Software
Tip 7: Make Quality a Requirements Issue
### Your Knowledge Portfolio
Your knowledge becomes out of date as new techniques, languages, and environments are developed.
#### Building Your Portfolio
- Invest regularly.
- Diversify. The more different things you know, the more valuable you are.
- Manage risk. Technology exists along a spectrum from risky, potentially high-reward to low-risk.
Don't put all your technical eggs in one basket.
- Buy low, sell high.
- Review and rebalance.

Tip 8: Invest Regulaly in Your Knowledge Portfolio.
#### Goals
- Learn at least one new language every year.
- Read a technical book each quarter. Once you're in the habbit, read a book a month. After you've
mastered the technologies you're currently using, branch out and study some that don't relate to your project.
- Read nontechnical books, too.
- Take classes. (Coursera)
- Participate in local user groups. Being actively participate! Not just go and listen!
- Experiment with different environments.(windows/ linux, IDE/ terminal)
- Stay current. Choose some that cover technology different from that of your current project.
- Get wired.

#### Opportunities for Learning
Don't let it stop there, when got a problem. Search the Web, go to the library, or find someone who
can.
#### Critical Thinking
Infos showed up on the top of search engine or popular book may not equal to matched best or good.
They may been paid to place it there.
Tip 9: Critically Analyze What You Read and Hear.
### Communicate!
#### Know What You Want To Say
Plan what you want to say. Write an outline. Then ask yourself, "Doss this get across whatever I'm
trying to say?" Refine it until it does.  
#### Know Your Audience
#### Choose Your Moment
#### Choose a Style
#### Make It Look Good
#### Involve Your Audience
#### Be a Listener
Encourage people to talk by asking questions, or have them summarize what you tell them.
#### Get Back to People
Tip 10: It's Both What You Say and the Way You Say It.

### E-Mail Communication
- Proofread before you send. Check the spelling.


## Chap:2 A pragmatic Approach
### The evils of Duplication
Tip 11: DRY-- Don't Repeat Yourself
The alternative is to have the same thing expressed in two or more places. If you change one, you
have to remember to change the others.
#### How Does Duplication Arise?
##### Imposed duplication
Developers feel they have no choice -- the environment seems to require duplication.
###### Multiple representations of information.
###### Documentation in code.
Good code has lots of comments, bad code requires lots of comments.  
The DRY principle tells us to keep the low-level knowledge in the code, where it belongs, and
reserve the comments for other, high-level explanations.  
###### Documentation and code.
###### Language issues.

##### Inadvertent duplication
Developers don't realize that they are duplicating information.

##### Impatient duplication
Developers get lazy and duplicate because it seems easier.
##### Interdeveloper duplication
Tip 12: Make it easy to Reuse.

### Orthogonality
#### What Is Orthogonality?
Two or more things are orthogonal if changes in one do not affect any of the others. In a well-designed
system, the database code will be orthogonal to the user interface: you can change the interface
without affecting the database, and swap databases without changing the interface.  
#### Benefits of Orthogonality
Tip 13: Eliminate effects between unrelated things
##### Gain Productivity
- Changes are localized, so development time and testing time are reduced.
- Promotes reuse.
- Get more functionality per unit when combined.
##### Reduce Risk
- If a module is sick, it is less likely to spread the sympotoms around the rest of the system.
- The resulting system is less fragile.
- Will be better tested.
#### Project Teams
Organize teams into groups with well-defined responsibilities and minimal overlap. One way is to
divide people by each major infrastructore component(database, communications interface, middleware
layer, and so on)  
#### Design
Don't rely on the properties of things you can't control.  
#### Toolkits and Libraries
Carefully choose.
#### Coding
#### Documentation


### Reversibility
`Nothing is more dangerous than an idea if it's the only one you have.`
Tip 14: There Are No Final Decisions.
#### Flexible Architecture
### Tracer Bullets
Tip 15: Use Tracer Bullect to Find the Target
#### Tracer bullets Don't Always Hit Their Target
You then adjust your aim until they're on target. That's the point.  
#### Tracer Code versus Prototyping

### Prototypes and Post-it Notes
What sorts of things might you choose to investigate with a prototype? Anything that carries risk.
Anything that hasn't been tried before, or that is absolutely critical to the final system.
Anything unproven, experimental, or doubtful.  Anything you aren't comfortable with. You can prototype
- Architecture
- New functionality in an existing system
- Structure or contents of external data
- Third-party tools or components
- Performance issues
- User interface design
Its value lies not in the code produced, but in the lessons learned.  

Tip 16: Prototype to Learn
#### How to Use Prototype
When building a prototype, what details can you ignore?  
- Correctness.   You may be able to use dummy data where appropriate.
- Completeness.   The prototype may function only in a very limited sense, perhaps with only one
preselected piece of input data and one menu item.
- Robustness.   Error checking is likely to be incomplete or missing entirely. If you stray from the
predefined path, the prototype may crash and burn in a glorious display of pyrotechnics. That's okay.
- Style.   It is painful to admit this in print, but prototype code probably doesn't have much in the
way of comments or documentation. You may produce reams of documentation as a result of your experience
with the prototype, but comparatively very little on the prototype system itself.  

Since a prototype should gloss over details, and focus in on specific aspects of the system being
considered, you may want to implement prototypes using a very high-level language—higher than
the rest of the project (maybe a languagesuch as Perl, Python, or Tcl). A high-level scripting
language lets you defer many details (including specifying data types) and still produce
a functional (albeit incomplete or slow) piece of code.  

####Prototyping Architecture
Here are some specific areas you may want to look for in the architectural prototype:
- Are the responsibilities of the major components well defined and appropriate?
- Are the collaborations between major components well defined?
- Is coupling minimized?
- Can you identify potential sources of duplication?
- Are interface definitions and constraints acceptable?
- Does every module have an access path to the data it needs during execution? Does it have that access when it needs it?

#### How Not to Use Prototypes

### Domain Languages
Tip 17: Program Close to the Problem domain.
#### Implementing a Maini-Language
#### Data Language and Imperative Language
The languages you implement can be used in two different ways.  
Data languages produce some form of data structure used by an application. These languages are often
used to represent configuration information.  
Imperative languages take this a step further. Here the language is actually executed, and so can
contain statements, control constructs, and the like (such as the script on page 58).  

#### Stand-Alone and Embedded Languages
#### Easy Development or Easy Maintenance?
Given that most applications exceed their expected lifetimes, you're probably better off biting the
bullet and adopting the more complex and readable language up front. The initial effort will be repaid
many times in reduced support and maintenance costs.  

### Estimating
Tip 18: Estimate to Avoid Superises
#### How Accurate is Accurate Enough?
> Duration     Quote estimate in
> 1-15 days    days
> 3-8 weeks    weeks
> 8-30 weeks   months
> 30+ weeks    think hard before giving an estimate

So, if after doing all the necessary work, you decide that a project will take 125 working days (25
weeks), you might want to deliver an estimate of "about six months.  
The same concepts apply to estimates of any quantity: choose the units of your answer to reflect the
accuracy you intend to convey.

#### Where Do Estimates Come From?
All estimates are based on models of the problem.
But before we get too deeply into the techniques of building models, we have to mention a basic
estimating trick that always gives good answers: ask someone who's already done it. 
##### Understand What's Being Asked
As well as the accuracy issues discussed above, you need to have a grasp of the scope of the domain.
Often this is implicit in the question, but you need to make it a habit to think about the scope
before starting to guess.   

##### Build a Model of the System
For a project, the model may be the steps that your organization uses during development, along with
a very rough picture of how the system might be implemented.  
Building the model introduces inaccuracies into the estimating process. Doubling the effort on the
model may give you only a slight increase in accuracy. Your experience will tell you when to stop refining.  

##### Break the Model into Components
You'll find that each component will typically have parameters that affect how it contributes to the
overall model. At this stage, simply identify each parameter.  

##### Give Each Parameter a Value
##### Calculate the Answers
##### Keep Track of Your Estimating Prowess
##### Estimating Project Schedules
- Check requirements
- Analyze risk
- Design, implement, integrate
- Validate with the users

Tip 19: Iterate the Schedule with the Code

##### What to Say When Asked for an Estimate
You say "I'll get back to you."


## Chap:3 The Basic Tools
#TODOOO
### The Power of Plain Text
### Shell Games
### Power Editing
### Source Code Control
### But My Team Isn't Using Source Code Control
### Source Code Control Products
### Debugging
### Text Manipulation
### Exercises
### Code Generators

## Chap:4 Pragmatic Paranoia
## Chap:5 Bend or Break
## Chap:6 While You Are Coding
## Chap:7 Before the Project
## Chap:8 Pragmatic Projects

