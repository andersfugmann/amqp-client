#!/bin/sh
VERSION=$(head -n 1 Changelog | sed 's/://')
REMOTE=$(git config remote.origin.url | sed 's/git@github.com:\(.*\).git/\1/')
URL=https://github.com/${REMOTE}/archive/${VERSION}.tar.gz
CHECKSUM=$(curl -q -L ${URL} | md5sum | cut -f1 -d' ')
BASE="/home/afu/git/opam-repository/packages"


for f in *.opam; do
    NAME=$(basename -s .opam $f)
    DIR=${BASE}/${NAME}/${NAME}.${VERSION}
    echo Create: $NAME $DIR

    echo mkdir -p ${DIR}
    mkdir -p ${DIR}

    cp ${NAME}.opam ${DIR}/opam
    cp ${NAME}.descr ${DIR}/descr
    echo "http: \"$URL\"" > ${DIR}/url
    echo "checksum: \"${CHECKSUM}\"" >> ${DIR}/url
done
