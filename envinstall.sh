#!/bin/sh

set -e

RUBY_VERSION=$(test $1 || echo 2.2)

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
    . ${HOME}/.rvm/scripts/rvm
  fi
  # install required gems
  bundle install
}

bootstrap_rvm

CURRENT_PATH="$(dirname `realpath $0`)"
FIX_PATH="$(echo $CURRENT_PATH | sed 's/\//\\\//g')"

sed "s/\@HERE\@/$FIX_PATH/g" $CURRENT_PATH/Restaurant.desktop.in > $CURRENT_PATH/Restaurant.desktop
	
echo "You must run command 'source ${HOME}/.rvm/scripts/rvm' before launching restaurant"
