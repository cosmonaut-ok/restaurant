#!/bin/sh

set -e
set -a

SCRIPT_HOME="$(dirname `realpath $0`)"
REQUIRED_PACKAGES="git vagrant discount gnupg"
CONF_FILE=${SCRIPT_HOME}/etc/restaurant.conf

if [ -f ${CONF_FILE} ]; then
    . ${CONF_FILE}
else
    RUBY_VERSION=''
    GEMSET_NAME=''
    CHEF_VERSION=''
    OPTIONS=''
fi

if [ ! -z $1 ]; then
    RUBY_VERSION=$1
else
    test -z ${RUBY_VERSION} && RUBY_VERSION=2.3.7
fi

if [ ! -z $2 ]; then
    GEMSET_NAME=$2
else
    test -z ${GEMSET_NAME} && GEMSET_NAME="global"
fi

if [ ! -z $3 ]; then
    CHEF_VERSION=$3
else
    test -z ${CHEF_VERSION} && CHEF_VERSION="13"
fi

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
	    ## optimize libgecode
	    export REQUIRED_PACKAGES="$REQUIRED_PACKAGES libgecode-dev"
	    export USE_SYSTEM_GECODE=1
	    #
	    if [ -n "$sudo_cmd" ]; then
		$sudo_cmd apt-get -y install $REQUIRED_PACKAGES
	    else
		$su_cmd "apt-get -y install $REQUIRED_PACKAGES"
	    fi
	    ;;
	CentOS|RHEL|Fedora)
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

with_ruby ()
{
    ruby=$1
    shift
    gemset=$1
    shift
    args="$@"
    /bin/bash --login -c ". ${HOME}/.rvm/scripts/rvm; rvm use ${ruby}@${gemset} --create && $args"
}

bootstrap_rvm ()
{
  if rvm_installed_p; then
      echo "RVM alreary installed. Skipping installation. Installing ruby and required gems"
      if [ -z "$(rvm list | grep ${RUBY_VERSION})" ]; then
	  rvm install ${RUBY_VERSION}
      else
	  echo "Ruby ${RUBY_VERSION} already installed. Switching to it"
	  RUBY_VERSION="$(rvm list | grep -oE "[a-z].*${RUBY_VERSION}[0-9,.,-]*" | head -n1)"
      fi
      ## use ``/bin/bash --login`` because rvm is stupid
      with_ruby ${RUBY_VERSION} ${GEMSET_NAME} gem install bundler
      # /bin/bash --login -c "rvm use ${RUBY_VERSION}@${GEMSET_NAME} --create && gem install bundler"
  else
    echo "RVM is not installed. Installing rvm, ruby and required gems"
    # gpg --keyserver ${GNUPG_URL} --recv-keys ${RVM_KEY}
    echo "Importing GPG keys"
    [ -n "$(which gpg)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg --import - 2>/dev/null
    [ -n "$(which gpg2)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg2 --import - 2>/dev/null
    echo "done"
    # bootstrap RVM
    curl -sSL https://get.rvm.io | bash -s stable --ruby=${RUBY_VERSION} --auto-dotfiles --gems=bundler || \
	(if [ ! -f ~/.rvm/scripts/version ]; then ln -s ~/.rvm/scripts/functions/version ~/.rvm/scripts/version && curl -sSL https://get.rvm.io | bash -s stable --ruby=${RUBY_VERSION} --auto-dotfiles --gems=bundler; fi) # fix for new RVM convensions
    echo "RVM Installed"
  fi
  # install required gems
  if [ -f ${SCRIPT_HOME}/data/Gemfile.${CHEF_VERSION} ]; then
      cp -f ${SCRIPT_HOME}/data/Gemfile.${CHEF_VERSION} ${SCRIPT_HOME}/Gemfile
      with_ruby ${RUBY_VERSION} ${GEMSET_NAME} "cd $SCRIPT_HOME && bundle install"
      rm -f ${SCRIPT_HOME}/Gemfile
  else
      echo "Chef version ${CHEF_VERSION} is not supported by restaurant. Please, use one of $(find data -name 'Gemfile.*' -exec basename $i {} \; | cut -d'.' -f2)"
      exit 1
  fi
}

generate_configfile ()
{
    if [ ! -f ${CONF_FILE} ]; then
       mkdir -p ${SCRIPT_HOME}/etc/
       echo "RUBY_VERSION=${RUBY_VERSION}" > ${CONF_FILE}
       echo "GEMSET_NAME=${GEMSET_NAME}" >> ${CONF_FILE}
       echo "CHEF_VERSION=${CHEF_VERSION}" >> ${CONF_FILE}
       echo "OPTIONS=\"-fs\"" >> ${CONF_FILE}
    fi
}

generate_configfile

bootstrap_with_packages

bootstrap_rvm

FIX_PATH="$(echo $SCRIPT_HOME | sed 's/\//\\\//g')"

sed "s/\@HERE\@/$FIX_PATH/g" $SCRIPT_HOME/data/restaurant.desktop.in > $SCRIPT_HOME/Restaurant.desktop
	
printf "\n\nYou must run command 'source ${HOME}/.rvm/scripts/rvm' before launching restaurant, or just restart 'restaurant' if it already launched\n\n"
