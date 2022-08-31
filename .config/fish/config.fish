set fish_greeting
set TERM "xterm-256color"

# alias for running ghc
alias ghc "stack ghc"

# alias for running ghci
alias ghci "stack ghci"

# alias for invoice-maker
alias invoice "~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

# alias for fzf directory switching
alias sd "cd && cd (find ~/Work ~/Projects ~/Developer -type d -depth 1 | fzf) "

# alias for python httpserver on port 8000 (replacement for nodejs live-server)
alias pyttp "python3 -m http.server"

# keyboard shortcut to open neovim in current directory
bind \cn 'nvim'

# keyboard shortcut to fuzzy find and switch to directory
bind \cf 'sd'

# keyboard shortcut to open a tmux session in current directory
bind \ct 'tmux new -s (basename (pwd))'

# keyboard shortcut to attach to last tmux session  
bind \cr 'tmux a'

source ~/.config/fish/hledger.fish
starship init fish | source
