function shorten_path {
	echo $(pwd | sed -e "s,^$HOME,~," -e "s;\([^/]*\)/\([^/]*\)$;\1,\2;" -e "s,\([^/]\)[^/]*/,\1/,g" -e "s;,;/;")
}

function rbenv_prompt() {
	rubyver=$(rbenv version-name)
	echo $rubyver
}

function preexec() {
  echo -n "\e[0m"
}

PROMPT='
%{$fg_bold[blue]%}%n%{$fg_no_bold[grey]%}@%{$fg_bold[green]%}%m %{$fg_no_bold[blue]%}$(shorten_path)$(git_prompt_info) %{$reset_color%}%(!.#.>) '
RPROMPT='%{$fg_no_bold[grey]%}rbenv:%{$fg_bold[grey]%}$(rbenv_prompt)%{$fg_no_bold[grey]%}%{$reset_color%}%{$fg_bold[white]%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}⭑"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[magenta]%}⭑"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}✔"
