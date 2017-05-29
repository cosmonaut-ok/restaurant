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
