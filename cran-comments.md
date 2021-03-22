## Resubmission
This is a resubmission. In this version I have:

* Added "\value"" to those files where it was missing

* Changed "\dontrun{}" to "\donttest{}"
  This change results in a new note when running "R CMD check" locally

# R CMD check results
0 errors | 0 warnings | 1 note

N  checking for non-standard things in the check directory
   Found the following files/directories:
     'temp'

* Similar questions on Stackoverflow, indicates these occur when producing PDF documents from Latex:
  https://stackoverflow.com/questions/62456137/r-cran-check-detritus-in-temp-directory


## Test environments
# Through R-hub
Windows Server 2008 R2 SP1, R-devel, 32/64 bit
Ubuntu Linux 20.04.1 LTS, R-release, GCC
Fedora Linux, R-devel, clang, gfortran

## R CMD check results

## R-hub test results
-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-a09a3648744e44839dc43d446cf7877b
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  5m 27.3s ago
  Build time: 5m 8.2s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jason Kaplan <scjase@gmail.com>'
  
  New submission

0 errors √ | 0 warnings √ | 1 note x

-- bridger 0.1.0: CREATED

  Build ID:   bridger_0.1.0.tar.gz-9089e56a661e4a81a549dd1ef197ae19
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  5m 27.4s ago


-- bridger 0.1.0: CREATED

  Build ID:   bridger_0.1.0.tar.gz-017774a542154b0e85c95814a86a4818
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  5m 27.4s ago

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Jason Kaplan <scjase@gmail.com>’

New submission

## Reverse dependencies
None
