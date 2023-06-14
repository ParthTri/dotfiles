set fish_greeting
set TERM "xterm-256color"
set -gx TMUXIFIER_LAYOUT_PATH "$HOME/.config/tmuxifier/"
set -gx EDITOR "nvim"

export PATH="$HOME/.bin/:$PATH"
export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.cargo/bin/:$PATH"
export PATH="$HOME/.nix-profile/bin:$PATH"
export PATH="$HOME/.local/share/pnpm:$PATH"
export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"

source ~/.config/fish/hledger.fish
source ~/.config/fish/prod.fish
source ~/.config/fish/nnn.fish
source ~/.config/fish/bindings.fish
source ~/.nix-profile/etc/profile.d/nix.fish 
starship init fish | source

# Tmuxifier
export PATH="$HOME/.tmuxifier/bin:$PATH"
eval (tmuxifier init - fish)

# pnpm
set -gx PNPM_HOME "/home/parth/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

