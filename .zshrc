# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
#fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# export PATH=$HOME/bin

plugins=(git)

source $ZSH/oh-my-zsh.sh
source ~/.oh-my-zsh/custom/themes/powerlevel10k

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export TERM=xterm-256color
export limelight=/usr/local/bin/limeight

# PATH=~/.doom.d/bin/doom
alias pipenv="python3 -m pipenv"

# typeset -g POWERLEVEL9K_INSTANT_PROMPT=off
# neofetch

alias md-org="pandoc -f markdown -t org -o"
alias download="~/.local/share/virtualenvs/Media-Downloader-Q9ismdSu/bin/python ~/Developer/Media-Downloader/src/main.py"
source /usr/local/opt/powerlevel10k/powerlevel10k.zsh-theme
export ANDROID_SDK_ROOT="/usr/local/share/android-sdk"
