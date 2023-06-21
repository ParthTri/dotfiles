set fish_greeting
set TERM "xterm-256color"
set -gx TMUXIFIER_LAYOUT_PATH "$HOME/.config/tmuxifier/"
set -gx EDITOR "nvim"

set -gx PATH "$HOME/.bin/:$PATH"
set -gx PATH "$HOME/.local/bin/:$PATH"
set -gx PATH "$HOME/.cargo/bin/:$PATH"
set -gx PATH "$HOME/.nix-profile/bin:$PATH"
set -gx PATH "$HOME/.local/share/pnpm:$PATH"
export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"

source ~/.config/fish/hledger.fish
source ~/.config/fish/prod.fish
source ~/.config/fish/nnn.fish
source ~/.config/fish/bindings.fish
source ~/.nix-profile/etc/profile.d/nix.fish 
starship init fish | source

# pnpm
set -gx PNPM_HOME "/home/parth/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

fish_add_path /home/parth/.spicetify
