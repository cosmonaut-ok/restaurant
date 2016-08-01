# Installation

## Install basic packages
```bash
user@host$ apt-get install emacs24 realpath git gpg
```

## Install RVM
```bash
user@host$ gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
user@host$ curl -sSL https://get.rvm.io | bash -s stable --ruby --autolibs=bundler
user@host$ rvm install 2.1 # or what ruby version you want
```

## Install Restaurant
```bash
user@host$ git clone https://github.com/cosmonaut-ok/restaurant.git restaurant
user@host$ cd restaurant
user@host$ ./bootstrap.sh
```
