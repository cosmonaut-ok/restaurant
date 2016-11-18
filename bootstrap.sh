#!/bin/sh

set -e

if [ ! -z $1 ]; then
    RUBY_VERSION=$1
else
    RUBY_VERSION=2.2
fi

SCRIPT_HOME="$(dirname `realpath $0`)"

rvm_installed_p ()
{
  which rvm 2>/dev/null
}


bootstrap_rvm ()
{
  if rvm_installed_p; then
      echo "RVM alreary installed. Skipping installation. Installing ruby and required gems"
      rvm install ${RUBY_VERSION}
      rvm use ${RUBY_VERSION}
      gem install bundler
  else
    echo "RVM is not installed. Installing rvm, ruby and required gems"
    # gpg --keyserver ${GNUPG_URL} --recv-keys ${RVM_KEY}
    curl -sSL https://rvm.io/mpapis.asc | gpg --import -
    # bootstrap RVM
    curl -sSL https://get.rvm.io | bash -s stable --ruby=${RUBY_VERSION} --auto-dotfiles --gems=bundler
  fi
  # install required gems
  . ${HOME}/.rvm/scripts/rvm
  cd $SCRIPT_HOME
  bundle install
}

bootstrap_rvm

FIX_PATH="$(echo $SCRIPT_HOME | sed 's/\//\\\//g')"

sed "s/\@HERE\@/$FIX_PATH/g" $SCRIPT_HOME/data/restaurant.desktop.in > $SCRIPT_HOME/Restaurant.desktop
	
printf "\n\nYou must run command 'source ${HOME}/.rvm/scripts/rvm' before launching restaurant, or just restart 'restaurant' if it already launched\n\n"
