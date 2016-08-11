
EMACS_URI=http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.gz
BUILD_DIR=/tmp/restaurant-release-tmp.zKdT0u
ARCHIVE_NAME=$(shell basename ${EMACS_URI})
SOURCE_DIR=$(shell basename ${EMACS_URI} .tar.gz)
RUBY=yes
BUNDLE=yes

all: emacs build clean-build clean-local

emacs:
	@echo "Making temporary working directory ${BUILD_DIR}"
	@mkdir -p ${BUILD_DIR}/emacs-build
	@mkdir -p ${BUILD_DIR}/restaurant
	@echo "Getting emacs archive..."
	@curl --progress-bar ${EMACS_URI} > ${BUILD_DIR}/${ARCHIVE_NAME}
	@echo "Unpacking emacs archive..."
	@tar -C ${BUILD_DIR} -xf ${BUILD_DIR}/${ARCHIVE_NAME}
	@echo "Building emacs..."
	cd ${BUILD_DIR}/${SOURCE_DIR} && \
	./configure --prefix=/ --without-pop --with-x-toolkit=gtk  --without-gif --without-png --without-jpeg --without-tiff && \
	make && \
	make install DESTDIR=${BUILD_DIR}/emacs-build
	@cp -r ${BUILD_DIR}/${SOURCE_DIR}/lib-src ${BUILD_DIR}/emacs-build/lib-src
	@mv -f ${BUILD_DIR}/emacs-build/share/emacs/24.5/* ${BUILD_DIR}/emacs-build/
	@echo "Moving to current directory as ''emacs''..."
	@mv ${BUILD_DIR}/emacs-build/ emacs

package:
	@echo "Building package..."
	@for i in bin bootstrap.el bootstrap.sh data init.el LICENSE rc README.md scripts snippets themes el-get elpa emacs; do cp -rp $$i ${BUILD_DIR}/restaurant/; done
	@touch ${BUILD_DIR}/restaurant/build
	@echo "Stripping package from unneded files..."
	@find ${BUILD_DIR}/restaurant/ -type d -name '.git' -exec rm -rf {} +
	@find ${BUILD_DIR}/restaurant/ -type f -name '.gitignore' -delete
	@find ${BUILD_DIR}/restaurant/ -type f -name '*.~' -delete
	@find ${BUILD_DIR}/restaurant/ -type f -name '^#.*' -delete
	@find ${BUILD_DIR}/restaurant/ -type f -name '.*#$$' -delete
	# TODO: add external libraries
	@echo "Packaging..."
	@cd ${BUILD_DIR} && tar -czpf restaurant-0.1-CURRENT.tar.gz restaurant
	@cp ${BUILD_DIR}/restaurant-0.1-CURRENT.tar.gz .

clean-build:
	@echo "Clearing build directory..."
	@[ -d ${BUILD_DIR} ] && rm -rf ${BUILD_DIR} || return 0

clean-local:
	@echo "Clearing working directory..."
	@rm -rf autom4te.cache configure config.log conf18498.dir config.status
	@find . -type f -name '*~' -delete
	@find . -type f -name '*#$$' -delete
	@find . -type f -name '*^#*' -delete
	@find . -type f -name '*.gz$$' -delete

clean: clean-build clean-local
	@echo "Clearing emacs 3rt-party data dirs..."
	@rm -rf build el-get elpa emacs

build:
	@echo "Building restaurant..."
	/usr/bin/emacs -Q --debug-init --script ./bootstrap.el
	@cd scripts && ./build_all
	@chmod 755 bin/restaurant
	@cat bootstrap.el > build

bootstrap: build
	@echo "Chekcing if all needed components are installed..."
ifneq ($(RUBY),yes)
	@echo "ERROR: there is no ruby in system. Exiting" && exit 1
endif
ifneq ($(BUNDLE),yes)
	@echo "ERROR: there is no ruby in system. Exiting" && exit 1
endif
	@echo "Building restaurant starting dependencies..."
	@cd scripts && bundle install
	@cat bootstrap.el > bootstrap

release: build emacs package clean-build

install:
	@echo install

help:
	@echo Options:
	@echo "       $(shell cat Makefile | grep -E '^[a-z].*\:$$' | tr ":" " ")"
	@echo
	@echo "       emacs       - build emacs locally to work with restaurant"
	@echo "       package     - packages prepared emacs into distribution restaurant package"
	@echo "       clean-build - clean remote build directory"
	@echo "       build       - build restaurant (w/o emacs. Just bootstrap)"
	@echo "       install     - install restaurant"
