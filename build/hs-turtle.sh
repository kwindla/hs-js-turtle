BUILDIR=$(dirname $0)
SRCDIR=${BUILDIR}/../hs-core
MAIN=${SRCDIR}/Main.hs

#ghc ${BASEDIR}/../hs-core/Main.hs -outputdir $BASEDIR -o turtle
ghc ${MAIN} -i${SRCDIR} -outputdir $BUILDIR/intermediate-files -o turtle
