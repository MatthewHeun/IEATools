## Context
`IEATools` v0.1.15 is the first CRAN release.

## Test environments (7 in total) and R CMD check results
* Local: macOS X install 10.15.2 (Catalina), R3.6.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* TRAVIS-CI: Ubuntu 16.04.6, R3.6.2
    * 0 ERRORs
    * 0 WARNINGs
    * 0 NOTEs
* Windows (on win-builder):
    * `devtools::check_win_release()`, R3.6.3 (2020-02-29)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-01-28 r77738)
        * 0 ERRORs
        * 0 WARNINGs
        * 0 NOTEs
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * 0 ERRORs
            * 0 WARNINGs
            * 0 NOTEs
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * 1 ERROR
              The error is:    
              #> sh: echo: I/O error    
              #> Build step 'Execute shell' marked build as failure    
              This error occurs early in the bootstrapping process and has nothing to do with testing the `matsindf` package.
            * 0 WARNINGs
            * 0 NOTEs
        * Fedora Linux, R-devel, clang, gfortran
            * 1 ERROR:
              The error is:    
              #> sh: echo: I/O error    
              #> Build step 'Execute shell' marked build as failure    
              This error occurs early in the bootstrapping process and has nothing to do with testing the `matsindf` package.
            * 0 WARNINGs
            * 0 NOTEs

## Downstream dependencies
* There are currently no downstream dependencies for `IEATools`.
