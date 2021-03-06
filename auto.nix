{ nixpkgs, config ? { }, optimize ? false }:

with (import nixpkgs config);

let
  mkDerivation = { srcs ? ./elm-srcs.nix, src, name, srcdir ? "./src"
    , targets ? [ ], registryDat ? ./registry.dat, outputJavaScript ? false }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ]
        ++ lib.optional outputJavaScript nodePackages_10_x.uglify-js;

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = let
        elmfile = module:
          "${srcdir}/${builtins.replaceStrings [ "." ] [ "/" ] module}.elm";
        extension = if outputJavaScript then "js" else "html";
        optimizeOption = if optimize then "--optimize" else "";
      in ''
        mkdir -p $out
        cp $src/src/index.html $out/
        cp -r $src/sounds $out/
        ${lib.concatStrings (map (module: ''
          echo "compiling ${elmfile module}"
          elm make ${
            elmfile module
          } ${optimizeOption} --output $out/${module}.${extension}
          ${lib.optionalString (outputJavaScript && optimize) ''
            echo "minifying ${elmfile module}"
            uglifyjs $out/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                | uglifyjs --mangle --output=$out/${module}.min.${extension}
          ''}
        '') targets)}
        ${lib.optionalString optimize ''
          sed -i 's/Main.js/Main.min.js/' $out/index.html
          rm $out/Main.js
        ''}
      '';
    };
in mkDerivation {
  name = "elm-app-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = [ "Main" ];
  srcdir = "./src";
  outputJavaScript = true;
}
