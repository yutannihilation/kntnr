## Test environments
* local Windows install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is mainly a maintenance release to fix compatibility issue with dplyr >0.7.
* This release contains one breaking change on the function kntn_file(). But, this function is not widely used,
  the impact is expected to be negligible.
