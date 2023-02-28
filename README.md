# Emanote Sitemap Generator

Generate a `sitemap.xml` file based on the export JSON by [Emanote](https://emanote.srid.ca/).

## Development

For development we use [devshell](https://github.com/numtide/devshell) which is a pre project development environment that uses nix under the hood.

To start a shell with all the dependencies needed to run this project, just run:

```sh
nix develop
```

Now you can use [Stack](https://docs.haskellstack.org/en/stable/) to start an interactive REPL or run the executable:

```sh
stack repl
# - OR -
stack run
```
