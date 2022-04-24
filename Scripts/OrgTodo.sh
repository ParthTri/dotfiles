#!/bin/bash


# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Emacs Capture Todo
# @raycast.mode inline
# @raycast.packageName Raycast Scripts
#
# Optional parameters:
# @raycast.icon 🤖
# @raycast.currentDirectoryPath ~
# @raycast.needsConfirmation false
#
# Documentation:
# @raycast.description Script command to open a new emacs frame into a capture template
# @raycast.author Parth Trivedi
# @raycast.authorURL An URL for one of your social medias#!/bin/bash

emacsclient -ce "(open-org-capture-todo)"
