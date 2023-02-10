set fish_greeting
set TERM "xterm-256color"
set EDITOR nvim

export PATH="$HOME/.bin/:$PATH"
export PATH="$HOME/.local/bin/:$PATH"

# alias for running ghc
alias ghc "stack ghc"

# alias for running ghci
alias ghci "stack ghci"

# Alias for startnig vlime server
alias vlime 'sbcl --load ~/.local/share/nvim/site/pack/packer/start/vlime/lisp/start-vlime.lisp'

# alias for invoice-maker
alias invoice "~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

# alias for fzf directory switching
alias sd "cd && cd (find ~/Work/ ~/Projects/ ~/Developer/ -maxdepth 1 | fzf) "

# alias for creating new tmux session in current directory
alias tpwd "tmux new -s (basename (pwd))"

# alias for python httpserver on port 8000 (replacement for nodejs live-server)
alias pyttp "python3 -m http.server"

# Quick command for newsboat
alias ns "newsboat"

# Adding shortcut to open nnn
bind \cf nnn -a 

# keyboard shortcut to open neovim in current directory
bind \cn 'nvim'

# keyboard shortcut to fuzzy find and switch directory and start new tmux session
bind \ce 'sd && tpwd'

# keyboard shortcut to fuzzy find and switch to directory
# bind \cf 'sd'

# Keyboard shortcut to open lazygit
bind \cg 'lazygit'

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
