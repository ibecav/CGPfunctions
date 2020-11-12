## Second Resubmission

Dear CRAN maintainers sorry about the extra trouble for some reason my changes to README.rmd didn't propagate to README.md.  Fixed now

## Resubmission

Corrected the following

 Found the following (possibly) invalid URLs:
     URL: http://cranlogs.r-pkg.org/badges/grand-total/CGPfunctions
(moved to https://cranlogs.r-pkg.org:443/badges/grand-total/CGPfunctions)
       From: README.md

     URL: https://ibecav.github.io/CGPfunctions (moved to
https://ibecav.github.io/CGPfunctions/)
       From: README.md

     URL: https://ibecav.netlify.com/tags/chaid/ (moved to
https://ibecav.netlify.app/tags/chaid/)
       From: inst/doc/Using-chaid_table.html

     URL: https://www.electionstudies.org/ (moved to
https://electionstudies.org/)
       From: inst/doc/Using-chaid_table.html

Please change http --> https, add trailing slashes, or follow moved
content as appropriate.

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
