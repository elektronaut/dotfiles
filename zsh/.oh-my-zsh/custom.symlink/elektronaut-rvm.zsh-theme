
function shorten_path {
	echo $(pwd | sed -e "s,^$HOME,~," -e "s;\([^/]*\)/\([^/]*\)$;\1,\2;" -e "s,\([^/]\)[^/]*/,\1/,g" -e "s;,;/;")
}

function fast_rvm_prompt() {
	rubyver=$(which ruby | xargs dirname | xargs dirname | xargs basename)
	gemset=$(rvm gemset name)
	if [ $gemset ]
	then
		echo $rubyver'@'$gemset
	else
		echo $rubyver
	fi
	;
}
 
PROMPT='%{$fg_bold[yellow]%}%n%{$fg_no_bold[grey]%}@%{$fg[yellow]%}%m %{$fg[green]%}$(shorten_path)$(git_prompt_info)%{$reset_color%}%(!.#.>) '
RPROMPT='%{$fg_no_bold[grey]%}rvm:%{$fg_no_bold[blue]%}$(fast_rvm_prompt)%{$fg_no_bold[grey]%}%{$reset_color%}'
#RPROMPT='%{$fg_no_bold[grey]%}rvm:%{$fg_no_bold[blue]%}$(rvm-prompt s i v g)%{$fg_no_bold[grey]%}%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[blue]%}/"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[red]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""
