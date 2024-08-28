# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/devy/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Configure CTRL+arrow behavior
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# Configure ALT+arrow behavior
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

# Configure Shift+arrow
bindkey "^[[1;2C" forward-word
bindkey "^[[1;2D" backward-word

eval "$(oh-my-posh init zsh --config ~/powerlevel10k_modern.omp.json)"

source ~/.aliases

# Load cargo deps
. "$HOME/.cargo/env"

# Load Brew
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

export PATH="$HOME/bin:$PATH"
