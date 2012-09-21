function shorten_path {
	echo $(pwd | sed -e "s,^$HOME,~," -e "s;\([^/]*\)/\([^/]*\)$;\1,\2;" -e "s,\([^/]\)[^/]*/,\1/,g" -e "s;,;/;")
}

function rbenv_prompt() {
	rubyver=$(rbenv version-name)
	echo $rubyver
}
 
PROMPT='%{$fg_bold[yellow]%}%n%{$fg_no_bold[grey]%}@%{$fg[yellow]%}%m %{$fg[green]%}$(shorten_path)$(git_prompt_info)%{$reset_color%}%(!.#.>) '
RPROMPT='%{$fg_no_bold[grey]%}rbenv:%{$fg_no_bold[blue]%}$(rbenv_prompt)%{$fg_no_bold[grey]%}%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[blue]%}/"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[red]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""
