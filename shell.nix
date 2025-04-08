{ pkgs ? import <nixpkgs> {} }:

let
  # Define library paths for better maintainability
  libraryPaths = with pkgs; [
    openssl
    sqlite
    libffi
    libGL
    libGLU
    SDL2
    SDL2_image
    SDL2_ttf
  ];

  # Create a temporary .sbclrc file for ASDF configuration
  sbclrcFile = pkgs.writeText "temp-sbclrc" ''
    ;; Configure ASDF output translations
    (require :asdf)
    ;(asdf:initialize-output-translations
    ;  '(:output-translations
    ;    (("/nix/store/**/*.*" (:home ".cache/common-lisp/sbcl-**/*.*"))
    ;    (t t))))

    ;; Load Quicklisp if available
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
  '';
in
pkgs.mkShell {
  # Include only the required packages
  buildInputs = with pkgs; [
    # Base SBCL
    sbcl

    # Libraries needed by Sketch
    libraryPaths

    # Common Lisp specific packages
    sbclPackages.cffi-libffi
    sbclPackages.sdl2
  ];

  # Set up the environment
  shellHook = ''
    # Set up library paths
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath libraryPaths}:$LD_LIBRARY_PATH"

    # Set up SBCL_HOME to find contribs
    export SBCL_HOME="${pkgs.sbcl}/lib/sbcl"

    # Create a wrapper for SBCL with proper configuration
    alias sbcl="sbcl --load ${sbclrcFile}"

    echo "SBCL environment ready for Sketch!"
    echo "To load Sketch, start SBCL and run: (ql:quickload :sketch)"
  '';
}
