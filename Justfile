alias r := run

run:
    cabal run --ghc-options="-ferror-spans" possession
