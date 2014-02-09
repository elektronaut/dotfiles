# dotfiles

Behind every productive member of the developer society stands a collection of haphazard
configurations and half baked shell scripts. These are mine.

I like things simple and neat. This configuration is based on
[Oh My ZSH](https://github.com/robbyrussell/oh-my-zsh) and
[Janus](https://github.com/carlhuda/janus), with just a few little tweaks. If you like
OS X, zsh, Ruby, rbenv, Homebrew, Sublime Text 2, Vim and Git, chances are you'll agree
with most of these settings. I use this setup both on my Macbook Pro and various
Ubuntu servers.


## Installation

Fair warning: This will overwrite your current dotfiles, no questions asked.

```sh
git clone https://github.com/elektronaut/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
rake install
```

Now is the time to grab a cup of coffee, the dependencies take a while to check out.

Updating is quite simple:

```sh
update_dotfiles
```

To update absolutely everything, including Oh My ZSH and Janus, run:

```sh
update_dotfiles all
```

## How it works

The Rakefile is at the heart of it all. Running rake without any arguments will
perform an update.

Configuration files are organized in subfolders by topic, where the relative path
from the subfolder corresponds with your $HOME dir.

Anything with the .symlink extension will be symlinked in. Files named .erb will be
run through ERB. The rest will be copied verbatim.


## Configuring the Rails environment variables

```sh
set_environment [environment]
```

This will set your RAILS_ENV and RACK_ENV automatically. Ruby can't change the environment
variables for the parent process, so you'll have to log out for the changes to take effect.


## Credits and thanks

Organizing files by topic was inspired by [Zach Holman's dotfiles](https://github.com/holman/dotfiles).
