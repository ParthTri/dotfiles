set fish_greeting
set TERM "xterm-256color"

# alias for running ghc
alias ghc "stack ghc"

# alias for running ghci
alias ghci "stack ghci"

# alias for invoice-maker
alias invoice "~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

# alias for fzf directory switching
alias sd "cd && cd (find ~/* -type d -path (string join '/' $HOME 'Library/*') -prune -o -print | fzf)"

# keyboard shortcut to open neovim in current directory
bind \cn 'nvim'

# keyboard shortcut to fuzzy find and switch to directory
bind \cf 'sd'

# keyboard shortcut to open a tmux session in current directory
bind \ct 'tmux new -s (basename (pwd))'

# keyboard shortcut to attach to last tmux session  
bind \cr 'tmux a'

starship init fish | source
