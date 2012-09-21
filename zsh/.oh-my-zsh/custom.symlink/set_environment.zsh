if [ -f $HOME/.environment ]; then
  export RAILS_ENV=$(cat $HOME/.environment)
  export RACK_ENV=$(cat $HOME/.environment)
fi