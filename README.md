# Emanote Sitemap Generator

Generate a `sitemap.xml` file based on the export JSON by [Emanote](https://emanote.srid.ca/).

## Development

For development we use [devshell](https://github.com/numtide/devshell) which is a pre project development environment that uses nix under the hood.

To start a shell with all the dependencies needed to run this project, just run:

```sh
nix develop
```

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `nix --accept-flake-config run github:juspay/omnix ci` to build _all_ outputs.
- Run `just fmt` in nix shell to autoformat the project. This uses [treefmt](https://github.com/numtide/treefmt).
- Run `just docs` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:gil0mendes/emanote-sitemap-generator` (or `nix run .` from checkout)
