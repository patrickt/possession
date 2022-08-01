alias b := build
alias r := run

build:
    cabal build --ghc-options="-ferror-spans" exe:possession

run:
    cabal run --ghc-options="-ferror-spans" possession
