FUNCTION numrec_period, x, y, n, ofac, hifac, px, py, np, ph, f, nout, $
   jmax, prob

;  Purpose:
;     To compute the Lomb normalized periodogram of a time series where the
;     data points are not necessarily equally spaced.

;  Usage:
;     rc = numrec_period (x, y, n, ofac, hifac, px, py, np, ph, f, nout, $
;        jmax, prob)

;  Input arguments:
;     * x {floating point array}: The time indices of the unequally-spaced time
;       series.
;     * y {floating point array}: The corresponding time series values.
;     * n {integer}: The number of points in the unequally-spaced time series.
;     * ofac {floating point}: The oversampling factor (a value of 4 is
;       typically adequate).
;     * hifac {floating point}: The largest frequency to be considered,
;       expressed as a multiplicative factor of the Nyquist frequency (a value
;       of 1 is typically adequate).

;  Output arguments:
;     * px {floating point array}: The suite of frequencies (up to hifac times
;       the Nyquist frequency) for which the Lomb periodogram is generated.
;     * py {floating point array}: The power associated with the corresponding
;       frequencies.
;     * np {integer}: The number of frequencies included in the periodogram. In
;       the original NR version, these arrays (and thus np) are defined before
;       calling this routine, hence the test on nout at the start of the
;       routine to ensure these arrays are large enough to hold the results. In
;       this implementation, nout is estimated first, then np is set to nout
;       and the output arrays are generated dynamically with that dimension.
;     * ph {floating point array}: The phase array associated with x.
;     * f {floating point array}: The complex Pseudo-Fourier spectrum.
;     * nout {integer}: A diagnostic value used to ensure that the time series
;       is long enough to proceed with the analysis.
;     * jmax {integer}: The index into the array px where py is maximum.
;     * prob {double precision}: The significance level that a periodic signal is
;       present in the time series at the frequency px (jmax).

;  Dependencies:
;     * Routine numrec_avevar

;  History:
;     This routine is an IDL implementation of the Numerical Recipes subroutine
;     period (See p. 572 and 573 of Numerical Recipes book). It appears to have
;     been upgraded or complemented by Hocke, as some additional code lines are
;     very similar or identical to those found in the MathLab function 'lspr'
;     available as part of supplementary materials to the paper by Hocke and
;     Kampfer (2008) in Atmospheric Chemistry and Physics, Vol 8, p. 4603-4623.
;     All materials available from
;     http://www.atmos-chem-phys-discuss.net/8/issue2.html

;  Initialize the return code:
retcode = 0

;  Set the maximum expected value of n. The value defined in the original
;  Numerical Recipes routine (2000) is sufficient to handle 55 years of
;  decadal data or 5 years of dily data. This value is used to dimension
;  temporary arrays:
nmax = 2000

;  Initialize the values of local double precision scalar variables:
arg = 0.0D0
twopid = 6.2831853071795865D0
wtemp = 0.0D0

;  The following two lines are ported from the version of Hocke but are not
;  part of the original Numerical Recipes version:
xstart = x[0]
x = x - xstart

;  Define local double prescision arrays:
wi = DBLARR(nmax)
wpi = DBLARR(nmax)
wpr = DBLARR(nmax)
wr = DBLARR(nmax)

;  Compute nout and set np to equal nout:
nout = 0.5 * ofac * hifac * FLOAT(n)
np = CEIL(nout)

;  Define the output arrays:
px = FLTARR(np)
py = FLTARR(np)
ph = FLTARR(np)
ph1 = FLTARR(np)

;  Compute the mean and variance of the data array:
rc = numrec_avevar(y, n, ave, var)

;  Estimate the minimum and maximum of the data array:
;  Numerical Recipes version:
;xmax = x[0]
;xmin = x[0]
;FOR j = 1, n - 1 DO BEGIN
;   IF (x [j] GT xmax) THEN xmax = x [j]
;   IF (x [j] LT xmin) THEN xmin = x [j]
;ENDFOR
;  IDL version:
xmax = MAX(x, MIN = xmin)

;  Compute the range and the mid-range of the array:
xdif = xmax - xmin
xave = 0.5 * (xmax + xmin)

pymax = 0.0
pnow = 1.0 / (xdif * ofac)
;  Numerical Recipes version:
;FOR j = 0, n - 1 DO BEGIN
;   arg = twopid * ((x [j] - xave) * pnow)
;   wpr [j] = -2.D0 * SIN(0.5D0 * arg)^2
;   wpi [j] = SIN(arg)
;   wr [j] = COS(arg)
;   wi [j] = wpi [j]
;ENDFOR
;  IDL version (which defines arg  and wpr as arrays instead of scalars):
arg = twopid * ((x - xave) * pnow)
wpr = -2.D0 * SIN(0.5D0 * arg)^2
wpi = SIN(arg)
wr = COS(arg)
wi = wpi

;  IDL version of removing the average from the time series:
yy = y - ave

FOR i = 0, nout - 1 DO BEGIN
   px [i] = pnow
;  Numerical Recipes version:
;   sumsh = 0.0
;   sumc = 0.0
;   FOR j = 0, n - 1 DO BEGIN
;      c = wr [j]
;      s = wi [j]
;      sumsh = sumsh + s * c
;      sumc = sumc + (c - s) * (c + s)
;   ENDFOR
;  IDL version:
   sumsh = TOTAL(wr * wi)
   sumc = TOTAL((wr - wi) * (wr + wi))
   wtau = 0.5 * ATAN(2.0 * sumsh, sumc)
   swtau = SIN(wtau)
   cwtau = COS(wtau)
;  Numerical Recipes version:
;   sums = 0.0
;   sumc = 0.0
;   sumsy = 0.0
;   sumcy = 0.0
;   FOR j = 0, n - 1 DO BEGIN
;      s = wi [j]
;      c = wr [j]
;      ss = s * cwtau - c * swtau
;      cc = c * cwtau + s * swtau
;      sums = sums + ss^2
;      sumc = sumc + cc^2
;      yy = y [j] - ave
;      sumsy = sumsy + yy * ss
;      sumcy = sumcy + yy * cc
;      wtemp = wr [j]
;      wr [j] = (wr [j] * wpr [j] - wi [j] * wpi [j]) + wr [j]
;      wi [j] = (wi [j] * wpr [j] + wtemp * wpi [j]) + wi [j]
;   ENDFOR
;  IDL version:
   ss = wi * cwtau - wr * swtau
   cc = wr * cwtau + wi * swtau
   sums = TOTAL(ss^2)
   sumc = TOTAL(cc^2)
   sumsy = TOTAL(yy * ss)
   sumcy = TOTAL(yy * cc)
   wtemp = wr
   wr = wr * wpr - wi * wpi + wr
   wi = wi * wpr + wtemp * wpi + wi

;  Computation of the imaginary and real parts of he Lomb-Scargle spectral
;  components (needed to compute the phase):
   iy = sumsy / SQRT(sums)
   ry = sumcy / SQRT(sumc)

;  Numerical Recipes version:
;   py [i] = 0.5 * (sumcy^2 / sumc + sumsy^2 / sums) / var
;  IDL version (to match the codes of Hocke):
   py [i] = 0.5 * (ry^2 + iy^2) / var

;  Assess the frequency and power at maximum power:
   IF (py [i] GE pymax) THEN BEGIN
      pymax = py [i]
      jmax = i
   ENDIF

;  Compute the phase of the Lomb-Scargle spectrum:
   phls = ATAN(iy, ry)
   arg0 = twopid * (xave + xstart) * pnow + wtau
   arg1 = twopid * xave * pnow + wtau
   ph [i] = phls + arg0 MOD twopid
   ph1 [i] = phls + arg1 MOD twopid

   pnow = pnow + 1.0 / (ofac * xdif)
ENDFOR

;  Compute the statistical significance of the maximum (in double precision
;  because variable pymax can take on large values (> 80.0) and generate an
;  underflow exception when such computations are run in single precision):
expy = EXP(DOUBLE(-pymax))

effm = 2.0 * nout / ofac
prob = effm * expy
IF (prob GT 0.01D) THEN prob = 1.0D - (1.0D - expy)^effm

;  Additional code from Hocke to compute FFT.
;  Set the dimension of the FFT spetrum:
dim = 2 * nout + 1
fac = SQRT(var * dim / 2)
;  Compute the amplitude vector for FFT:
a = fac * SQRT(py)
;  Compute the real and imaginary parts of the FFT spectrum:
fx = a * COS(ph1)
fy = a * SIN(ph1)
ph = (ph + 5 * twopid) MOD twopid
wk1 = px
wk2 = py

fxr = REVERSE(fx)
fyr = REVERSE(fy)

;f = [TRANSPOSE(COMPLEX(ave, 0)), TRANSPOSE(COMPLEX(fx, fy)), TRANSPOSE(COMPLEX(fxr, -fyr))]
f = [COMPLEX(ave, 0), COMPLEX(fx, fy), COMPLEX(fxr, -fyr)]

RETURN, retcode

END