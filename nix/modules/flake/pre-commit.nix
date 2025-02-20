{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
    inputs.fourmolu-nix.flakeModule
  ];
  perSystem = { config, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        cabal-fmt.enable = true;
        fourmolu = {
          enable = true;
          package = config.fourmolu.wrapper;
        };
        hlint.enable = true;
      };
    };

    fourmolu.settings = {
      column-limit = 120;
      comma-style = "leading";
      extensions = [ "ImportQualifiedPost" ];
      function-arrows = "leading";
      haddock-style = "single-line";
      haddock-style-module = "single-line";
      import-export-style = "leading";
      in-style = "left-align";
      indent-wheres = false;
      indentation = 2;
      let-style = "inline";
      newlines-between-decls = 1;
      record-brace-space = true;
      respectful = true;
      single-constraint-parens = "always";
      unicode = "never";
    };
  };
}
