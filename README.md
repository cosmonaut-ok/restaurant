# Restaurant

## What is Restaurant IDE

There is no specialized Chef IDE for DevOps for free usage (there is RubyMine with chef plugin, but only for non-commercial usage, or paid). So, Restaurant is the project of ready-to-use chef IDE, integrated with ``ruby``, ``foodcritic``, ``chefspec``, ``knife`` and ``kitchen``.

## Installation

**NOTE:** To use Restaurant, you must have working **ruby, bundler, chef, knife, rspec and kitchen**

### Install basic packages
```bash
user@host$ apt-get install emacs24 git gpg
```

### Install RVM (optional)
```bash
user@host$ gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
user@host$ curl -sSL https://get.rvm.io | bash -s stable --ruby --autolibs=bundler
user@host$ rvm install 2.1 # or what ruby version you want
user@host$ rvm docs generate-ri # optional. Generate documentation
```

### Install basic packages (w/o RVM)
```bash
user@host$ apt-get install emacs24 git gpg ruby gem ruby-bundler
```

### Install Restaurant
```bash
user@host$ git clone https://github.com/cosmonaut-ok/restaurant.git restaurant
user@host$ cd restaurant
user@host$ ./bootstrap.sh install # local installation
user@host$ sudo ./bootstrap.sh install --global # global installation
user@host$ . ~/.profile # only for local installation
user@host$ restaurant # Enjoy
```
