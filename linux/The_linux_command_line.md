# The linux command line
# Part I - Learning the Shell
## 1 What is shell
The shell is a program that takes keyboard commands and passes them to the operating system to carry out.  

shell prompt: username@machinename  
### Try some simple commands
- data: displays the current time and date
- cal: current month
- df: To see the current amount of free space on your disk drives
- free: to display the amount of free memory
### Ending a terminal session
- exit
### The Console Behind The Curtain
Called virtual terminals or virtual consoles. Accessed by pressing CtrlAlt-F1 through Ctrl-Alt-F6.
To switch from one virtual console to another, press Alt and F1-F6.
To return to the graphical desktop, press Alt-F7.  
## 2 Navigation
- pwd - Print name of current working directory
- cd - Change directory
- ls - List directory contents
### Understanding the file system tree
Note that unlike Windows, which has a separate file system tree for each storage device,
Unix-like systems such as Linux always have a single file system tree, regardless of how
many drives or storage devices are attached to the computer. Storage devices are attached
(or more correctly, mounted) at various points on the tree according to the whims of the
system administrator.  

### Changing the current working directory
#### Absolute Pathnames
An absolute pathname begins with the root directory and follows the tree branch by
branch until the path to the desired directory or file is completed. 

  $ cd /usr/bin
#### Relative Pathnames
A relative pathname starts from the working directory. The "." notation refers to the working directory and the ".." notation refers to the working directory's parent directory.  

    cd ./bin
In almost all cases, we can 10Changing The Current Working Directory omit the "./".
#### Some helpful shortcuts
- cd    Changes the working directory to your home directory.
- cd -  Changes the working directory to the previous working directory.
- cd ~user_name Changes the working directory to the home directory of user\_name.
#### Important Facts aoubt filenames
1. Filenames that begin with a period character are hidden. This only means that
ls will not list them unless you say `ls -a`. 
2. Filenames and commands in Linux, like Unix, are case sensitive.
3. Linux has no concept of a “file extension” like some other operating systems.
You may name files any way you like. Although Unix-like operating systems don’t use
file extensions to determine the contents/purpose of files, many application
programs do.
4. Though Linux supports long filenames which may contain embedded spaces and punctuation
characters, limit the punctuation characters in the names of files you create to period, dash,
and underscore. Most importantly, do not embed spaces in filenames. If you want to represent
spaces between words in a filename, use underscore characters. You will thank yourself later.
## 3 Exploring the system
# TODOOOO
## 4 Manipulating files and directories
## 5 Working with commands
## 6 Redirection
## 7 Seeing the world as the shell sees it
## 8 Advanced Keyboard Tricks
## 9 Permissions
## 10 Processes

# Part II - Configuration and the environment
# Part III - Common tasks and essential tools
# Part IV - Writing shell scripts
