#!/usr/bin/env sh

# used within an Alpine docker container to do
# a build of the project

set -eux

: "display current environment"

grep ^ -n /etc/*rel*
sudo --version
id
stack --version

: "install tar"

# stack 2.1.3 can't parse current git specification for extra-deps

sudo apk add tar

: "set permissions"

# ensure 'user' user can write to
# .stack-work etc, and read all code

sudo mkdir -p ~user/.stack
sudo chown -R user:user ~user/.stack
sudo mkdir -p ./stack-work
sudo chown user:user ./stack-work
sudo chmod -R a+rw .
sudo chmod -R a+rwx `find -type d`

: "define builder function"

# expects one arg, either "--dry-run" or ""
do_stack_build() {
  sudo mkdir -p ./build_artifacts
  sudo chown user:user ./build_artifacts

  stack --allow-different-user --skip-ghc-check --system-ghc \
      --resolver=lts-13 \
      --local-bin-path ./build_artifacts  \
      build   \
        --copy-bins  \
        --flag b2hs:staticbuild  \
        $1 ;
} 

: "dry-run"

do_stack_build --dry-run

: "build"

do_stack_build ""

: "prepare deploy"

export ARGS="--resolver lts-13 --allow-different-user --skip-ghc-check --system-ghc"
export PKGVER=$(stack $ARGS ls dependencies --depth 0 | sed 's/ /-/')
export SRC_TGZ=$PKGVER.tar.gz

# build source tgz

stack $ARGS sdist;
distdir=`stack $ARGS path --dist-dir`;
cp $distdir/$SRC_TGZ .;
ls -al "$SRC_TGZ"

# executables tgz

tar cf ${PKGVER}_linux_x86_64.tgz --gzip -C build_artifacts .
ls -al ${PKGVER}_linux_x86_64.tgz

set +eux
