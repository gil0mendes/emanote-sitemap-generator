let
  pkgs = import <nixpkgs> { };
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
            --nix \
            --no-nix-pure \
            "
    '';
  };
in
pkgs.mkShell {
  # Do NOT use `stack`, otherwise system dependencies like `zlib` are missing at compilation
  buildInputs = [
    stack-wrapped
    pkgs.haskell-language-server
    pkgs.hlint
  ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
