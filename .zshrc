# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# export PATH=$HOME/bin

plugins=(git)
source $ZSH/oh-my-zsh.sh

export TERM=xterm-256color
export limelight=/usr/local/bin/limeight

# PATH=~/.doom.d/bin/doom
alias pipenv="python3 -m pipenv"

# neofetch
alias md-org="pandoc -f markdown -t org -o"
alias download="~/.local/share/virtualenvs/Media-Downloader-Q9ismdSu/bin/python3 ~/Developer/Projects/Media-Downloader/src/main.py"
alias invoice="~/Projects/Invoice-Maker/venv/bin/python3 ~/Projects/Invoice-Maker/src/main.py"

eval "$(starship init zsh)"
