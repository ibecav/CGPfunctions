## Resubmission

You wrote ===
Thanks, we see:

   Found the following (possibly) invalid URLs:
     URL: http://creativecommons.org/licenses/by-sa/4.0/
       From: inst/doc/Using-PlotXTabs.html
             inst/doc/Using-PlotXTabs2.html
             inst/doc/Using-chaid_table.html
             inst/doc/Using-newggslopegraph.html
       Status: 521

Can you simply change to https://...?


In your Description field we see you wrote “new”

but you should not use directed quotes, better use undirected ones such
as in "new".

Please fix and resubmit.

I changed ===

* removed the quotes completely in DESCRIPTION
* changed all http to https in all locations in vignettes

## Test environments
* local OS X install, R 3.6.3
* rhub() Ubuntu, Fedora, Windows server
* win-builder (devel and release)

## R CMD check results

── R CMD check results ───────────────────────────────── CGPfunctions 0.6.0 ────
Duration: 2m 13.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
