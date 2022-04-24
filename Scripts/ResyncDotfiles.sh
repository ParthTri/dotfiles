#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Symlink dotfiles
# @raycast.mode inline
# @raycast.packageName Raycast Scripts
#
# Optional parameters:
# @raycast.icon 🤖
# @raycast.currentDirectoryPath ~
# @raycast.needsConfirmation false
#
# Documentation:
# @raycast.description Script command to symlink .dotfiles directory
# @raycast.author Parth Trivedi
# @raycast.authorURL An URL for one of your social medias#!/bin/bash

cd ~/.dotfiles/
stow .
