# Restaurant

[![Travis Build Status](https://api.travis-ci.org/restaurant-ide/restaurant.svg?branch=master)](https://travis-ci.org/restaurant-ide/restaurant)

## What is Restaurant IDE and Why (just another IDE?).

**Restaurant** software is an Integrated Development Environment for Chef developers.

### Objectives

There is no specialized Chef IDE for DevOps for free usage (there is RubyMine with chef plugin, but only for non-commercial usage, or paid). So, **Restaurant** is a project of free and opensource ready-to-use chef IDE...

### Tools and Technologies

...which supports *code highlight*, *autocompletion*, inline *syntax* and *style checking* (via, `rubocop` and `foodcritic`) *code expansion* (via `code snippets`), *code refactoring*, built-in *unit* (via `rspec`) and *integration* tests (via `kitchen`) launching and interaction. Also, Restaurant supports tools `rvm`, `bundler`, `berkshelf` (via built-in extensions to this tools), `git` and `github` integrations, *yaml*, *json* and *markdown* file tools (like, reformatting, visualising etc).

### Integrations

* Chef
* PRY
* rspec/chefspec
* rubocop
* foodcritic
* kitchen/serverspec/inspec
* berkshelf
* bundler
* RVM
* git/github

## Quick Start

Let's suppose, that you have no Restaurant, no ruby, no bundler... actually nothing

  * Get Restautant (we recommend to use last binary build): [link](https://github.com/restaurant-ide/restaurant/releases/download/v0.3.0/restaurant-0.3.0-gtk2.tar.gz)
	* Unpack it: `tar -xvzf restaurant-0.3.0-gtk2.tar.gz`
	* Install rvm environment with helper: `cd restaurant && ./bootstrap.sh <your.favorite.ruby.version|2.2>` or in your favorite way (Gemfile present)
        * At your option: drag-n-drop ``Restaurant'' icon to your quick launch place (e.g. to Desktop)
	* GO GO GO: `cd restaurant && ./restaurant`
  * Add source
	* Right click on **Root** (top-right corner) > **Add Source Path**
	* Add your cookbooks to source path for quick navigate

## Quick HOWTO

TODO

## Supported linux distributives

* Linux
  * Debian 9 (Stretch)
  * Ubuntu 17.04
  * Fedora 26
  * Opensuse 42.2

Other linuxes officially not supported, and Restaurant is not tested on it. But it does not mean, that Restaurant can't be launched on it. Just use it on own risk there.

WARNING: Restaurant is not working on systems with glibc < 2.18. But, you can try to use external emacs to run Restaurant.

You can use Restaurant with external emacs, built for your OS. Just run `EMACS=/path/to/emacs ./restaurant` (of course, it is not tested with it too).

## Keybindings

### General

* `Shift+F1` Restaurant Keybindings Help

* `F11` - toggle code browser
* `Ctrl+F11` - toggle compile window (bottom)
* `Shift+TAB` - complete at point
* `Ctrl+TAB` - expand from template at point

### Toggle interactive consoles

* `Ctrl+F12` open ruby interactive console (based on PRY)
* `F12` open built-in shell terminal
* `Shift+F12` open elisp interactive console

### Kitchen-specific

* `F9` run `kitchen converge` (choose instance from drop-down menu)
* `Shift+F9` run `kitchen verify` (choose instance from drop-down menu)
* `Ctrl+F9` run `kitchen converge` for all available instances
* `Ctrl+Shift+F9` run `kitchen verify` for all available instances

### Linters

* `F5` foodcritic current chef cookbook
* `Shift+F5` foodcritic current cookbook file

* `F6` check current cookbook with rubocop (with respect to cookstyle)
* `Shift-F6` current file with rubocop (with respect to cookstyle)

### Unit Testing (rspec/chefspec)

* `F8` run all rspec unit tests
* `Ctrl+F8` run rspec tests, described only in current file
* `Shift+F8` run single rspec test (under cursor)

### Search/Highlight

* `F7` search
* `Alt+F7` search in project (bound to repository directory)

* `Shift+F7` highlight word/regexp in current file
* `Ctrl+Shift+F7` highlight word/regexp in current file
* `Ctrl+c w`, `Ctrl+c Ctrl+w` (un)highlight whitespaces in current file
* `Ctrl+c Ctrl+f` flash-cross

### JSON-specific

* `Ctrl+Alt+q` json-reformat-region
* `Ctrl+c Ctrl+g` jsons-print-path

### Other

* `Ctrl+F3` select ruby version from rvm-installed (like `rvm use`)
* `Shift+F3` run `bundle update` command in current project
* `F3` run `berks update` command in current project
* `Shift+SPC` hs-toggle-hiding




Ctrl+Alt+S helm-swoop -> TODO: REMOVE
