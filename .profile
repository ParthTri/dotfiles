export EDITOR="emacsclient -c"

# if [ -e /home/parth/.nix-profile/etc/profile.d/nix.sh ]; then . /home/parth/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export PATH="$HOME/.bin/:$PATH"
export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.cargo/bin/:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.nix-profile/bin:$PATH"
export PATH="$HOME/.local/share/pnpm:$PATH"
export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
