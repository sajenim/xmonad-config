# List available recipes
default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Run cabal build
build *ARGS:
    cabal build {{ ARGS }}

# Run cabal run
run *ARGS:
    cabal run {{ ARGS }}
