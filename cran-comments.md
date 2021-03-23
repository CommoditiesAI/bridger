## Resubmission
This is a second re-submission. 

Notes from the auto-check service:
Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-ix86+x86_64
Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'Jason Kaplan <scjase@gmail.com>'

  New submission

Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-ix86+x86_64
Check: LazyData, Result: NOTE
    'LazyData' is specified without a 'data' directory
    
* I have removed 'LazyData' from the description

Flavor: r-devel-linux-x86_64-debian-gcc
Check: for detritus in the temp directory, Result: NOTE
  Found the following files/directories:
    'RtmpbbllxA\bridgeHands_any_full.pdf'
    
* This temporary file is created from running the examples.  The PDF is the intended output of the package


This is a re-submission. In this version I have:

* Added "\value"" to those files where it was missing

* Changed "\dontrun{}" to "\donttest{}"

# R CMD check results
0 errors | 0 warnings | 0 note

## Test environments
# Through R-hub
Windows Server 2008 R2 SP1, R-devel, 32/64 bit
Ubuntu Linux 20.04.1 LTS, R-release, GCC
Fedora Linux, R-devel, clang, gfortran

# Through check_win_devel()
Your package bridger_0.1.0.tar.gz has been built (if working) and checked for Windows.
Installation time in seconds: 14
Check time in seconds: 129
Status: 1 NOTE
R Under development (unstable) (2021-04-05 r80144)

## R CMD check results
0 errors | 0 warnings | 0 notes

## R-hub test results
-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-5992f302e16b4b10a7899048bdbd2106
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  27m 33.3s ago
  Build time: 8m 0.9s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jason Kaplan <scjase@gmail.com>'
  
  New submission

0 errors | 0 warnings | 1 note

-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-25b79529ee9c47efaa5d4ccde19cc8c1
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  27m 33.3s ago
  Build time: 18m 59.3s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jason Kaplan <scjase@gmail.com>’
  
  New submission

0 errors | 0 warnings | 1 note

-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-d09b5758023648188ae383e15b43e2ab
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  27m 33.4s ago
  Build time: 19m 43.9s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jason Kaplan <scjase@gmail.com>’
  
  New submission

0 errors | 0 warnings | 1 note

## Reverse dependencies
None
