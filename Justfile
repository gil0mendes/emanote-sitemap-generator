default:
	@just --list

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -T :main
