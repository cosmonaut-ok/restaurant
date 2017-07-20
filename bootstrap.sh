#!/bin/sh

set -e

if [ ! -z $1 ]; then
    RUBY_VERSION=$1
else
    RUBY_VERSION=2.3
fi

if [ ! -z $2 ]; then
    GEMSET_NAME=$2
else
    GEMSET_NAME="restaurant"
fi

SCRIPT_HOME="$(dirname `realpath $0`)"
REQUIRED_PACKAGES="git vagrant markdown"

print_message()
{
    message=$@
    msg_engine=""
    ## check
    if [ -n "$(which zenity)" ]; then
	msg_engine="zenity --error --text"
    elif [ -n "$(which kdialog)" ]; then
	msg_engine="kdialog --error"
    elif [ -n "$(which xdialog)" ]; then
	msg_engine="dialog --error"
    else
	msg_engine="echo"
    fi
    $msg_engine "$message"
}

bootstrap_with_packages ()
{
    os="$(which lsb_release 2>/dev/null >/dev/null && lsb_release -s -i)"
    sudo_cmd="$(which sudo || true)"
    su_cmd="su -c"
        
    case $os in
	Debian|Ubuntu)
	    echo "installing required packages"
	    if [ -n "$sudo_cmd" ]; then
		$sudo_cmd apt-get -y install $REQUIRED_PACKAGES
	    else
		$su_cmd "apt-get -y install $REQUIRED_PACKAGES"
	    fi
	    ;;
	CentOS|RHEL)
	    echo "installing required packages"
	    if [ -n "$sudo_cmd" ]; then
		$sudo_cmd yum -y install $REQUIRED_PACKAGES
	    else
		$su_cmd "yum -y install $REQUIRED_PACKAGES"
	    fi
	    ;;
	*)
	    for i in $REQUIRED_PACKAGES; do
		if [ -z "$(which $i)" ]; then
		    print_message You must install package \"$i\" firstly
		    exit 1
		fi
	    done
	    ;;
    esac
}

rvm_installed_p ()
{
  which rvm 2>/dev/null
}


bootstrap_rvm ()
{
  if rvm_installed_p; then
      echo "RVM alreary installed. Skipping installation. Installing ruby and required gems"
      if [ -z "$(rvm list | grep ${RUBY_VERSION})" ]; then
	  rvm install ${RUBY_VERSION}
      else
	  echo "Ruby ${RUBY_VERSION} already installed. Switching to it"
      fi
      rvm use ${RUBY_VERSION}@${GEMSET_NAME} --create
      gem install bundler
  else
    echo "RVM is not installed. Installing rvm, ruby and required gems"
    # gpg --keyserver ${GNUPG_URL} --recv-keys ${RVM_KEY}
    echo "Importing GPG keys"
    [ -n "$(which gpg)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg --import - 2>/dev/null
    [ -n "$(which gpg2)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg2 --import - 2>/dev/null
    echo "done"
    # bootstrap RVM
    curl -sSL https://get.rvm.io | bash -s stable --ruby=${RUBY_VERSION} --auto-dotfiles --gems=bundler
    echo "RVM Installed"
  fi
  # install required gems
  ## use ``/bin/bash --login`` because rvm is stupid
  /bin/bash --login -c ". ${HOME}/.rvm/scripts/rvm && rvm use ${RUBY_VERSION}@${GEMSET_NAME} --create && cd $SCRIPT_HOME && bundle install"
}

generate_configfile ()
{
    mkdir -p ${SCRIPT_HOME}/etc/
    echo "RUBY_VERSION=${RUBY_VERSION}" > ${SCRIPT_HOME}/etc/restaurant.conf
    echo "GEMSET_NAME=${GEMSET_NAME}" >> ${SCRIPT_HOME}/etc/restaurant.conf
    echo "OPTIONS=\"-fs\"" >> ${SCRIPT_HOME}/etc/restaurant.conf
}

generate_configfile

bootstrap_with_packages

bootstrap_rvm

FIX_PATH="$(echo $SCRIPT_HOME | sed 's/\//\\\//g')"

sed "s/\@HERE\@/$FIX_PATH/g" $SCRIPT_HOME/data/restaurant.desktop.in > $SCRIPT_HOME/Restaurant.desktop
	
printf "\n\nYou must run command 'source ${HOME}/.rvm/scripts/rvm' before launching restaurant, or just restart 'restaurant' if it already launched\n\n"
