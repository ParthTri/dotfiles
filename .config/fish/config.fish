set fish_greeting
set TERM "xterm-256color"

# alias for running ghc
alias ghc "stack ghc"

# alias for running ghci
alias ghci "stack ghci"

# alias for invoice-maker
alias invoice "~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

starship init fish | source
