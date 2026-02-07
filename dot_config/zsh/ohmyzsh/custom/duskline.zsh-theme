function shorten_path {
  echo $(pwd | sed -e "s,^$HOME,~," -e "s;\([^/]*\)/\([^/]*\)$;\1,\2;" -e "s,\([^/]\)[^/]*/,\1/,g" -e "s;,;/;")
}

function no_git_prompt() {
  if [[ -z $(git_prompt_info) ]]; then
    echo $'%K{black}%F{blue}\uE0B0'
  fi
}

PROMPT=$'\n%K{blue}%F{black} @%B%m%b '
PROMPT+='$(git_prompt_info)$(no_git_prompt)'
PROMPT+='%{$bg[black]%}%{$fg[white]%} $(shorten_path) '
PROMPT+=$'%K{default}%F{black}\uE0B0%f%k%F{8} %#%f%k '

#RPROMPT=$'%{$reset_color%}%K{default}%F{0}\uE0B2%K{0}%F{blue} %D{%F %T} %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=$'%K{white}%F{blue}\uE0B0 %F{#000000}\uE0A0 '
ZSH_THEME_GIT_PROMPT_SUFFIX=$' %K{black}%F{white}\uE0B0'
ZSH_THEME_GIT_PROMPT_DIRTY=" %B%F{red}**%b"

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  # Simple prompt for use inside Emacs
  PROMPT=$'\n%F{blue}%n@%m %F{green}%2~ %F{cyan}%#%{$reset_color%} '
  RPROMPT=''
fi

if [[ -n $SSH_CONNECTION ]]; then
  ZSH_THEME_TERM_TAB_TITLE_IDLE="%m - %15<..<%~%<<"
else
  ZSH_THEME_TERM_TAB_TITLE_IDLE="%15<..<%~%<<"
fi

export LSCOLORS="exfxgxdxcxegedabagacad"
