# Tmux
## Basics
### Installation
    sudo apt-get install tmux

### Splitting Panes
All commands in tmux are triggered by a prefix key followed by a command key (quite similar to
emacs). By default, tmux uses `C-b` as prefix key.  

The shortcut to split panes into a left and a right pane is C-b %.  
- %: vertical
- ": horizonal

### Navigating Panes
- C-b <arrow key>

### Close Panes
- exit
- Ctrl-d
- prefix x

### Creating Windows
- C-b c
- C-b p: Switch to previous window
- C-b n: Switch to next window
- C-b <number>: go to a window directly by typing its number

### Session Handling
- C-b s: list sessions
- C-b d: detach your current session
- C-b D: to have tmux give you a choice which of your sessions you want to detach
Figure out which sessions are running by using:  

    tmux ls
    tmux a -t <session_name>
    tmux new -s <session_name> // create a session with a name
    tmux rename-session -t ori_name new_name

## Moving on
- C-b z: make a pane full screen
- C-b C-<arrow key>: resize pane in direction of <arrow key>
- C-b ,: rename the current window

建议对调ctrl和Caps-lock


### TODO: to config
prefix + |
prefix h k j l

