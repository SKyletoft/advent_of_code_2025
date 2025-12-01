{
	inputs = {
		nixpkgs.url      = "github:nixos/nixpkgs";
		flake-utils.url  = "github:numtide/flake-utils";
		apl-readline.url = "github:SKyletoft/apl-readline";
	};

	outputs = { self, nixpkgs, flake-utils, apl-readline }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = import nixpkgs {
					inherit system;
					config.allowUnfree = true; # APL
				};
				haskell-packages = [(pkgs.haskell.packages.ghc912.ghc.withPackages(pkgs: with pkgs; [
					QuickCheck
					unordered-containers
					haskell-language-server
				]))] ++ [
					pkgs.fourmolu
				];
				rust-packages = with pkgs; [
					rustc
					cargo
					rust-analyzer
					clippy
					rustfmt
				];
			in {
				devShells.default = pkgs.mkShell {
					nativeBuildInputs =
						[
							(pkgs.dyalog.override { acceptLicense = true; })
							apl-readline.outputs.packages.${system}.default
							pkgs.hyperfine
						]
						++ rust-packages
						++ haskell-packages;
				};
			}
		);
}
