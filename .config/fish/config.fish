set fish_greeting
set TERM "xterm-256color"
set EDITOR nvim

# alias for running ghc
alias ghc "stack ghc"

# alias for running ghci
alias ghci "stack ghci"

# alias for invoice-maker
alias invoice "~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

# alias for fzf directory switching
alias sd "cd && cd (find ~/Work ~/Projects ~/Developer -maxdepth 1 | fzf) "

# alias for creating new tmux session in current directory
alias tpwd "tmux new -s (basename (pwd))"

# alias for python httpserver on port 8000 (replacement for nodejs live-server)
alias pyttp "python3 -m http.server"

# Adding shortcut to open nnn
bind \cf nnn -a 

# keyboard shortcut to open neovim in current directory
bind \cn 'nvim'

# keyboard shortcut to fuzzy find and switch directory and start new tmux session
bind \ce 'sd && tpwd'

# keyboard shortcut to fuzzy find and switch to directory
# bind \cf 'sd'

# keyboard shortcut to open a tmux session in current directory
bind \ct 'tpwd'

# keyboard shortcut to attach to last tmux session  
bind \cr 'tmux a'

source ~/.config/fish/hledger.fish
source ~/.config/fish/prod.fish
source ~/.config/fish/nnn.fish
starship init fish | source

# pnpm
set -gx PNPM_HOME "/home/parth/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH
# pnpm end
