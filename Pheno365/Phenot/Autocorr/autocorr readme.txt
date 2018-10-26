This directory contains a copy of the original IDL program to calculate
the autocorrelation function (a_correlate.pro) and a new version of the
same program to perform the same computation for a series that may include
missing or 'bad' values (autocorr.pro). This latter routine will ignore
all values belo or above user-specified thresholds.

A test program (tstauto.pro) is also included to demonstrate proper usage
and to allow the verification of the numerical results.
