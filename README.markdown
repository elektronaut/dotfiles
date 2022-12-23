# dotfiles

Behind every productive member of the developer society stands a
collection of haphazard configurations and half baked shell
scripts. These are mine.

## Setup

First, install [chezmoi](https://www.chezmoi.io/) and [oh-my-zsh](https://ohmyz.sh/).

Checkout the repo:

``` sh
chezmoi init https://github.com/elektronaut/dotfiles.git
```

Inspect the changes:

``` sh
chezmoi diff
```

Finally, apply them:

``` sh
chezmoi apply
```
