## R CMD check results

0 errors | 0 warnings | 0 note


Additional comment:

`gcc-UBSAN` error for 0.0.3:

```
readBCI.h:571:31: runtime error: load of misaligned address 0x6310000dc8bf for type 'short int', which requires 2 byte alignment
0x6310000dc8bf: note: pointer points here
 00 00 01 00 80  00 d0 ff 40 ff a0 fc 90  fe 90 fe a0 01 90 ff d0  fe 50 fe 80 fd 20 fd 30  fe e0 fe
```

This was because I cast pointer (`char*`) into (`uint16_t*`). The operation has been replaced by `memcpy` with proper checks to ensure no out-of-bound access.

The new version has been checked with `rhub::check(platform = 'linux-x86_64-rocker-gcc-san')`
