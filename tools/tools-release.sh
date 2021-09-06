#!/usr/bin/env sh

if [ -z "${VERSION}" ]; then
  echo "Usage: VERSION=<version> make release"
  exit 1
fi

# check that the working tree is not dirty
if [ -n "$(git diff --stat)" ]; then
  echo "ERROR: 'git diff --stat' reports that the repository contains uncommitted changed; cannot proceed"
  exit 1
fi

# update the version in the DESCRIPTION
sed "s|Version:.*|Version: ${VERSION}|g" DESCRIPTION > DESCRIPTION.tmp
mv DESCRIPTION.tmp DESCRIPTION

# remove UNRELEASED from NEWS
sed "s|.*(UNRELEASED)|# renv ${VERSION}|g" NEWS.md > NEWS.md.tmp
mv NEWS.md.tmp NEWS.md

# build and check the package
cd ..
R CMD build renv
R CMD check "renv_${VERSION}.tar.gz"
cd renv
