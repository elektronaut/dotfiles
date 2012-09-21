# dotfiles

## Installation

Warning: this will destroy your current configuration without asking.

```sh
git clone https://github.com/elektronaut/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
rake install
```

To update, run

```sh
update_dotfiles
```

To update everything including external dependencies, run

```sh
update_dotfiles all
```

## Configuring Rails environment

```sh
set_environment [environment]
```

will set your RAILS_ENV and RACK_ENV variables on the next login.