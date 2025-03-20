{
  description = "OCaml-based Vim plugin: alloc_scan";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        self',
        pkgs,
        ...
      }: let
        inherit (pkgs) ocamlPackages mkShell;
        inherit (ocamlPackages) buildDunePackage;

        name = "alloc_scan";
        version = "0.0.1";

        # Step 1: Build the OCaml package
        ocamlAllocScan = buildDunePackage {
          inherit version;
          pname = name;
          src = ./.;
          buildInputs = with pkgs.ocamlPackages; [
            core
            angstrom
            vcaml
            odoc
          ];
        };

        # Step 2: Build the Vim plugin
        vimPlugin = pkgs.vimUtils.buildVimPlugin {
          pname = "alloc_scan";
          version = version;
          src = ./.; # Use the same source directory
          # Include the OCaml build output as a dependency
          buildInputs = [ocamlAllocScan];
          # Optional: Customize the build process if needed
          buildPhase = ''
            # Copy the OCaml build output into the plugin directory
            mkdir -p $out/bin
            cp ${ocamlAllocScan}/bin/* $out/bin/ || true # Adjust based on actual output

            # If you have Vim-specific files (e.g., plugin/*.vim), ensure they're in src
            # If not, you might need to add them manually to your repo
          '';
        };
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [self'.packages.default];
            buildInputs = with pkgs.ocamlPackages; [
            dune_3
              utop
              ocaml-lsp
              ocamlformat
            ];
          };
        };

        packages = {
          # The OCaml package (for reference or standalone use)
          ocamlAllocScan = ocamlAllocScan;

          # The Vim plugin package
          default = vimPlugin;
        };
      };
    };
}
