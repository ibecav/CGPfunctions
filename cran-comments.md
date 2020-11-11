## Test environments
* local OS X install, R 4.0.3
* rhub() Ubuntu, Fedora, Windows server
  Have to force utf-8 build with 
  rhub::check(
      platform="windows-x86_64-devel",
      env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
  )
* win-builder (devel and release)

## R CMD check results

── R CMD check results ───────────────────────────────── CGPfunctions 0.6.3 ────
Duration: 2m 19.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
