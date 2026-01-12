## R CMD check results

0 errors | 0 warnings | 0 note


In addition, `rchk` reports:


```
CRAN Package Check Results for Package readNSx

Last updated on 2026-01-12 17:49:57 CET.


Package readNSx version 0.0.6
Package built using 88970/R 4.6.0; x86_64-pc-linux-gnu; 2025-10-26 17:32:44 UTC; unix   
Checked with rchk version 35618ebbccf3cd0b45a3530e6303970a22a9056b LLVM version 14.0.6
More information at https://github.com/kalibera/cran-checks/blob/master/rchk/PROTECT.md
For rchk in docker image see https://github.com/kalibera/rchk/blob/master/doc/DOCKER.md

Function cpp11::writable::r_vector<SEXPREC*>::r_vector(std::initializer_list<cpp11::named_arg>)::{lambda()#1}::operator()() const
  [UP] unprotected variable names while calling allocating function Rf_mkCharCE cpp11/include/cpp11/list.hpp:91
```

This is an upstream issue from `cpp11` package abd is being discussed at https://github.com/r-lib/cpp11/issues/445
