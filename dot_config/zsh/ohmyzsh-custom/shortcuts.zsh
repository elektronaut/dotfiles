b3s=~/Dev/projects/b3s
aa=~/Dev/anyone/apps
pr=~/Dev/projects
dgems=~/Dev/gems
scratch=~/Dev/scratch
puppet=~/Dev/anyone/puppet

alias deploy="git push; cap production deploy"

alias cm="chezmoi"
compdef cm=chezmoi

alias assistant-session="(cd ~/Dev/make && claude --agent assistant -n assistant)"
