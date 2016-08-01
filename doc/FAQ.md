# Frequently Asked Questions

## What does mean "M-x", "C-c" etc?
It's first letters for key-modificators:
- "M-..." is for "Alt+..." (Meta key)
- "C-..." is for "Control+..."
- "s-..." is for "Winkey+..." (Super key)
- "S-..." is for "Shift+..."
- "C-M-..." is for "Control+Alt+..."

### How to apply Key combinations
If you face expressions like: ``C-c h``, ``C-x C-c`` etc, it means, that you should input:
``Control+x`` than ``h``, ``Control+x`` than ``Control+c``

## How to exit from Restaurant
Restaurant is an emacs-based IDE, so, key combinations are the same, like in emacs.
Exit combination is ``C-x C-c`` 

## How can I find keybindings for Restaurant
You can use eamcs keybindings. You can find it [here](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf) or [here](http://www.ast.cam.ac.uk/~vasily/idl/emacs_commands_list.html)

## How to change Restaurant parameters
- TODO:

## How to change theme
- M-x
- type ``customize-themes``
- choose your favorite theme
- press ``Save Theme Settings``
- close buffer (C-x k, or "cross" symbol at toolbar menu)
- Enjoy

## What possibilities/technologies/utils restaurant supports
- ruby/chef syntax highlight
- ruby/chef style errors check (including foodcritic)
- ruby/chef syntax errors check
- ruby/chef autocomplete
- ruby/chef integrated unit testing with rspec/chefspec
- chef cookbooks integration testing with kitchen
- multiple ruby/gems versions and sets via rvm
- yaml files syntax highlight
- yaml files encoding and pretty printing
- xml files syntax highlight
- markdown syntax highlighting/helping
- markdown toc autogeneration
- markdown in-browser preview
- git/github support (pull/diff/stage/push/etc)

## I am a **TRUE** emacser and want to get back my C-c, C-x and C-v keybindings
- type: ``M-x customize-group`` -> ``restaurant``
- go to ``System``
- set restaurant/familiar-copy-paste-cut to false

## How to change font on my restaurant
- Go to menu, choose ``Options`` -> ``Set default font``
- Set font
