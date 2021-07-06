## Resubmission
This is a re-submission. 

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
    
* This temporary file was created when running the examples. 
* Made examples "\dontrun{}" so they do not generate temporary files that persist after running


# R CMD check results
0 errors | 0 warnings | 0 note

## Test environments
# Through R-hub
Windows Server 2008 R2 SP1, R-release, 32/64 bit
Ubuntu Linux 20.04.1 LTS, R-release, GCC
Fedora Linux, R-devel, clang, gfortran

# Through check_win_devel()
Your package bridger_0.1.0.tar.gz has been built (if working) and checked for Windows.
Installation time in seconds: 14
Check time in seconds: 129
Status: 1 NOTE
Maintainer: 'Jason Kaplan <scjase@gmail.com>'

New submission

R Under development (unstable) (2021-04-05 r80144)

## R CMD check results
0 errors | 0 warnings | 0 notes

## R-hub test results
-- bridger 0.1.0: NOTE

  Build ID: 	bridger_0.1.0.tar.gz-7a733e98da194f2b8f86ee90f2098f92
  Platform: 	Windows Server 2008 R2 SP1, R-release, 32/64 bit
  Submitted:  27m 33.3s ago
  Build time: 8m 0.9s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jason Kaplan <scjase@gmail.com>'
  
  New submission

0 errors | 0 warnings | 1 note

-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-43f48e364c584a2fb9f7fb4c1503074b
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  27m 33.3s ago
  Build time: 18m 59.3s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jason Kaplan <scjase@gmail.com>’
  
  New submission

0 errors | 0 warnings | 1 note

-- bridger 0.1.0: NOTE

  Build ID:   bridger_0.1.0.tar.gz-f4d38c2260ab44798ef962fc3f954e9a
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  27m 33.4s ago
  Build time: 19m 43.9s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jason Kaplan <scjase@gmail.com>’
  
  New submission

0 errors | 0 warnings | 1 note

## Reverse dependencies
None
