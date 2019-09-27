# Setup fzf
# ---------
if [[ ! "$PATH" == */home/s/hell/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/s/hell/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/s/hell/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/s/hell/.fzf/shell/key-bindings.zsh"
