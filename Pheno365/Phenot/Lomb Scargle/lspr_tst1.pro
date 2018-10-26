PRO lspr_tst1


;  Start of the IDL implementation:
;  WARNING: Matlab keeps track of case when handling identifiers.
;  Unfortunately, the original code uses both f to designate a set of
;  frequencies and F to stand for the power spectrum. IDL does not
;  care for case, so these variables have been relabeled f_lc and f_uc,
;  respectively.

;  Set the number of days:
days = 60

;  Calculate the number of data points per day for a sampling of every
;  20 minutes:
ndp = 24 * 3          ;   72

;  Calculate the total number of data points in the time series:
n = days * ndp        ; 4320

;  Set the starting point of the time series:
t1 = 5 * ndp          ;  360

;  Set the time series offset, a value added later to y to raise the
;  whole time series to positive values between 0 and 40:
offset = 20

;  Generate the array of sequential time values and the array that will
;  contain the time series:
x = t1 + FINDGEN(n) + 1   ; [360:4679] ; x = t1 + FINDGEN(n) Janek: following the matlab code the first element is 361
y = FLTARR(n)
;MMV: It is not clear why this assignment is made here in the Matlab code,
;  as the variable dim is never used until it is reassigned another value
;  later on... Statement commented out.
;dim = n               ; 4320 when originally set.

;  Set the amplitudes of the frequencies used to generate the clean
;  signal:
a = [3, 2, 3, 2, 3]
;a = [3,0,0,0,0]
;  Set the fixed delays applied to each of the frequencies used to
;  generate the clean signal:
del = [0.25, 0.5, 0.75, 0.1, 0.1] * 2.0 * !PI

;  Set the phases of the frequencies used to generate the clean signal:
ome = days * [0.3, 0.7, 1.2, 2.0, 3.0]
om = 2.0 * !PI * ome / n

;  Set the number of frequencies used to generate the clean signal:
nn = N_ELEMENTS(a)

;  Build the clean time series as a superposition of basic frequencies
;  of amplitudes a, frequency om and phase or time delay del:
FOR j = 0, nn - 1 DO BEGIN
   y = y + a [j] * COS(om [j] * x + del [j])
ENDFOR


;  Save this clean time series in arrays xc and yc:
xc = x 
yc = y  

;  Add Normally distributed pseudo random noise to the original time
;  series:
y = y + 2.5 * MEAN(a) * IMSL_RANDOM(n, /NORMAL) ; JANEK: Why scaling factor 2.5??

;  Set the flag to indicate whether to incorporate data gaps or not:
igap = 1

IF (igap) THEN BEGIN
;MMV: Artificially create two data gaps. The Matlab variables ig1 and ig2 are renamed ig1a
;  and ig1b for the first gap a and iga1b and ig2b for the second gap,
;  to be able to check their numerical values:

   ig1a = FIX(n / 3.0)           ; 1440
   ig2a = ig1a + 450            ; 1890
   ig1b = FIX(2.0 * n / 4.0) + 451   ; 2160
   ig2b = ig1b + 150          ; 2310
   
   x = [x[0:ig1a-2],x[ig2a:ig1b-2],x[ig2b:N_ELEMENTS(x)-1]] ;Janek: it was y [ig1a - 1:ig2a - 1] = -1.0
                                             ;Janek: coresponding do Matlab 452 points are removed (including 1440 and 1890)
                                             ;and 152 points (including 2160 and 2310)
                                             ;so [x[0:1398],x[1890:2158],x[2310:4680]] 
    
   y = [y[0:ig1a-2],y[ig2a:ig1b-2],y[ig2b:N_ELEMENTS(y)-1]] ;Janek: it was y [ig1a - 1:ig2a - 1] = -1.0


ENDIF

;  Save this noisy and gappy time series in xo and yo:
xo = x
yo = y

;  Remove the average value of the main time series so that it is truly
;  centered on 0:
y = y - MEAN(y)
;----------------------------------------------------------------------------------------------------------------------------
;MMV: The role of variable fa is unclear in Matlab code. This changes
;  the overall amplitude of the final signal, depending on whether or
;  not the Hamming window algorithm is applied to the series (fa = 2)
;  or not (fa = 1)...
fa = 1

;  Set the flag to decide whether to apply the Hamming window algorithm
;  or not:
win = 1

;MMV: The Hann (Hanning) and Hamming window filters are implemented as
;  separate functions in Matlab (actually in the Spectral Processing
;  Toolbox). In IDL, the Hamming window is an option of the Hann
;  (Hanning) algorithm.
IF (win) THEN BEGIN
   window = HANNING(N_ELEMENTS(y), ALPHA = 0.54)
   y = y * window
   fa = 2
ENDIF

;  By construction, the time series built so far are centered on 0
;  (sums of cosine functions). Add a standard offset to all of them
;  so that they range between 0 and 40:
y = y + offset     ; Noisy and gappy time series, mean removed,
                   ; possibly subjected to Hamming window; working
                   ; version
yo = yo + offset   ; Noisy and gappy time series
yc = yc + offset   ; Clean time series

;  Compute the Lomb-Scargle Normalized Periodogram, using the IDL
;  translation of the Matlab routine. Prior to calling this routine,
;  x = array of time values
;  y = array of observational data corresponding to the times provided
;     in x

matlab_lspr, x, y, wk1, wk2, ph, vari, hifac, ofac, f_uc

;rc = numminmax(wk2, 20.0, 200.0, nseqs, nmins, nmaxs, mins, maxs, imins, imaxs) ;Janek: Why lower treshold set to 20??
;print, 'Finding maxs in wk2...'
;print, 'nseqs = ', nseqs
;print, 'nmaxs = ', nmaxs
;print, 'maxs = ', maxs
;print, 'imaxs = ', imaxs
;print, 'wk1[imaxs] = ', wk1[imaxs]
;window, /free

;MMV: Upon returning from lspr,
;  wk1 = array of increasing frequencies, from 1.0 / (range(x) * ofac)
;     to ofac times the nyquist frequency f_c = 1/(2 * sampling interval),
;     in increments of 1.0 / (range(x) * ofac), where range(x) = MAX(x) - MIN(x).
;  wk2 = array of Lomb-Scargle Normalized power values for the
;     frequencies provided in wk1
;  ph = array of phases, in radians, for each of the frequencies
;     provided in wk1 (arguments of the COS function at x = 0)
;  vari = sigma^2, variance of y, used to derive the amplitude from
;     the normalized power wk2
;  hifac = an integer indicating up to which maximum frequency the
;     analysis proceeds, in units of Nyquist frequency (typically 1 or 2)
;  ofac = oversampling factor, typically 4
;  f_uc = complex Pseudo-Fourier spectrum

;  Compute the amplitude of the Lomb-Scargle Normalized periodogram
;  (this variable is not used later on):
am = fa * SQRT(wk2 * 2.0 * vari * 2.0 / n)

;  Save the original complex Pseudo-Fourier spectrum so that it can be
;  modified to retain only the desired frequencies:
fo_uc = f_uc

;MMV: The Matlab code appears to implement two approaches to select the
;  frequencies that should be kept for the purpose of reconstructing the
;  missing data. The first is based on an a priori value of the
;  significance level, the second on an arbitrary selection of the lower
;  frequency components. Both approaches are 'active' and the second
;  appears to override the first, so only the second is activated here.

;  First approach:
;  Set the confidence level to evaluate the significance of the power
;  spectrum peaks:
;conf = 98
;alpha = (100.0 - conf) / 200.0

;MMV: The Matlab call to norminv appears to compute at once the 2 values
;  of the Normal distribution corresponding to the two confidence levels
;  provided, but only the second one is used. The IMSL function works with
;  only one input, so it is called only once:
;level = IMSL_NORMALCDF(1.0 - alpha, /DOUBLE, /INVERSE)
;nf = N_ELEMENTS(f_uc)
;spec = f_uc [1:FIX(nf / 4.0) - 1]
;noise_limit = SQRT(level * MEAN(spec * CONJ(spec)))

;  Second approach:
;  Set the threshold at 1/3 of the highest peak in the the power spectrum:
level = MAX(ABS(f_uc[1:N_ELEMENTS(f_uc) - 1])) / 2.5
noise_limit = level

;  Zero the power spectrum for those frequencies where the power is less
;  than the threshold, but keep the 0-frequency value:
ind = WHERE(ABS(f_uc) LT noise_limit)
f_uc [ind] = 0.0          ;removal of the noise
f_uc [0] = fo_uc [0]      ;

dim = N_ELEMENTS(wk1)
f_lc = wk1 * N_ELEMENTS(xc) / days
ao_spec = ABS(fo_uc [1:dim]) * 2 / N_ELEMENTS(f_uc) ;Janek: what is this??
a_spec = ABS(f_uc [1:dim]) * 2 / N_ELEMENTS(f_uc) ;Janek: what is this??

thresh = noise_limit * 2.0 / N_ELEMENTS(f_uc)

;  Find the maxima in this truncated power spectrum:
;rc = numminmax(a_spec, 0.0, 1.0e6, nseqs, nmins, nmaxs, mins, maxs, imins, imaxs)
;print, 'Finding maxs in a_spec...'
;print, 'nseqs = ', nseqs
;print, 'nmaxs = ', nmaxs
;print, 'maxs = ', maxs
;print, 'imaxs = ', imaxs
;print, 'wk1[imaxs] = ', wk1[imaxs]

WINDOW, /FREE, TITLE = 'Lomb-Scargle periodogram'
!P.MULTI = [0, 0, 0, 0, 0]
xli = [0.1, 10]
yli = [0.0, MAX(ao_spec) * 1.2]
PLOT, $
   [f_lc [0], f_lc [N_ELEMENTS(f_lc) - 1]], $
   [thresh, thresh], $
   /XLOG, $
   XRANGE = xli, $
   XSTYLE = 1, $
   YRANGE = yli, $
   YSTYLE = 1, $
   XTITLE = 'Frequency [cycles/day]', $
   YTITLE = 'Amplitude [arbitrary units]', $
   TITLE = 'Lomb-Scargle periodogram', $
   BACKGROUND = 16777215, $
   COLOR = 0, $
   LINESTYLE = 2

OPLOT, $
   f_lc, $
   ao_spec, $
   COLOR = 1000, $
   THICK = 3
   
OPLOT, $
   f_lc, $
   a_spec, $
   PSYM = 2, $
   COLOR = 16711680, $
   SYMSIZE  = 1.5

fb_uc = FFT(f_uc, /INVERSE)

yf = REAL_PART(fb_uc)
yf = yf/n_elements(fb_uc) ; Janek: This statment is written because the Matlab variable yf is devided by number of element of array fb_uc
xf = x[0] + FINDGEN(N_ELEMENTS(f_uc)) / (wk1 [1] - wk1 [0]) / N_ELEMENTS(f_uc)


ind = WHERE(xf LE x [N_ELEMENTS(x) - 1])
xf = xf [ind]
yf = yf [ind]
IF (win) THEN BEGIN
   yf = yf / HANNING(N_ELEMENTS(yf), ALPHA = 0.54)
ENDIF
sc = 1
;sc=mean(abs(yo-mean(yo)))/mean(abs(yf))
yf = sc * yf + REAL_PART(f_uc [0])

WINDOW, XSIZE = 1200, YSIZE = 1000, /FREE, TITLE = 'Time series'
!P.MULTI = [0, 0, 4, 0, 0]
xli = [20, 48]
yli = [0, 40]
PLOT, $
   xc / ndp, $
   yc, $
   XRANGE = xli, $
   XSTYLE = 1, $
   YRANGE = yli, $
   YSTYLE = 1, $
   MIN_VALUE = yli [0], $
   MAX_VALUE = yli [1], $
   YMINOR = 4, $
   CHARSIZE = 2.0, $
   BACKGROUND = 16777215, $
   COLOR = 0, $
   THICK = 2, $
   TITLE = 'Original time series'
PLOT, $
   [0], $
   [0], $
   XRANGE = xli, $
   XSTYLE = 1, $
   YRANGE = yli, $
   YSTYLE = 1, $
   MIN_VALUE = yli [0], $
   MAX_VALUE = yli [1], $
   YMINOR = 4, $
   CHARSIZE = 2.0, $
   COLOR = 0, $
   TITLE = 'Original time series with noise and data gaps'
OPLOT, $
   x / ndp, $
   y, $
   COLOR = 1000, $
   THICK = 1
   
   
PLOT, $
   [0], $
   [0], $
   XRANGE = xli, $
   XSTYLE = 1, $
   YRANGE = yli, $
   YSTYLE = 1, $
   MIN_VALUE = yli [0], $
   MAX_VALUE = yli [1], $
   YMINOR = 4, $
   CHARSIZE = 2.0, $
   COLOR = 0, $
   TITLE = 'Reconstracted time series'
OPLOT, $
   x / ndp, $
   y, $
   COLOR = 1000, $
   THICK = 1

OPLOT, $
   xc / ndp, $
   yc, $
   COLOR = 0, $
   THICK = 3

   
OPLOT, $
   xf / ndp, $
   yf, $
   Linestyle = 1, $
   COLOR = 65280, $
   THICK = 2   
  
xlim = [10, 80]
;xlim = [23, 33]
ylim = yli

PLOT, $
   [0], $
   [0], $
   XRANGE = xlim, $
   XSTYLE = 1, $
   YRANGE = ylim, $
   YSTYLE = 1, $
   MIN_VALUE = yli [0], $
   MAX_VALUE = yli [1], $
   YMINOR = 4, $
   CHARSIZE = 2.0, $
   COLOR = 0, $
   TITLE = 'Reconstracted time series details'
OPLOT, $
   x, $ ; x / ndp, $
   y, $
   COLOR = 1000, $
   THICK = 1

OPLOT, $
   xc / ndp, $
   yc, $
   COLOR = 0, $
   THICK = 3

   
OPLOT, $
   xf / ndp, $
   yf, $
   Linestyle = 1, $
   COLOR = 65280, $
   THICK = 2   

!P.MULTI = [0,0,0,0,0]

WINDOW, XSIZE = 1200, YSIZE = 1000, /FREE, TITLE = 'fo_uc'
plot, ABS(fo_uc[0:1000])
oplot, [0,100000],[noise_limit,noise_limit], linestyle=1,color=1000
 
print, 'koniec'
;stop

;  Purpose:
;     To test the IDL implementation of the matlab_lspr routine.

;  Origin:
;     This is an IDL translation of the matlab_lspr routine provided
;     by Hocke as supplementary materials to his 2008 paper
;     (file 'supplement2.txt').

;  Original source file:

;% ****************************************************
;% main program for demonstration how to call the 
;% procedure lnpr (Lomb-Scargle Periodogram for Reconstruction)  
;% ****************************************************
;
;% preparation of artificial time serie y(x):
;% --------------------------------------------------
;days=60 ;
;ndp=24*3;   %points per day (sampling time 20 min)
;n=days*ndp;  
;t1=5*ndp     ; %start point    
;offset=20;
;
;x=t1+(1:n);
;y=zeros(n,1);
;dim=n;
;
;a=[3,2,3,2,3];                    %input amplitude 
;del=[.25,.5,.75,.1,.1]*2*pi;       %;      phase
;
;ome=days*[.3,.7,1.2,2,3]    ;        %;   periods [cyc/day]
;om=2*pi*ome/n;
;nn=length(a);
;
;
;for j=1:nn         %; superposition of the waves
;  y=y+a(j)*cos(om(j)*x'+del(j));
;end
;
;xc=x;  %saving of the clear series (signal curve without noise and gaps)
;yc=y;
;  
;  
;y=y+2.5*mean(a)*randn(n,1) ;   %  plus  noise 
;
;%generation of gaps in y(x):
;igap=1
;if igap
;  ig1=fix(n/3)
;  ig2=ig1+450;
;  x(ig1:ig2)=[];
;  y(ig1:ig2)=[];
;  ig1=fix(2*n/4)
;  ig2=ig1+150;
;  x(ig1:ig2)=[];
;  y(ig1:ig2)=[];
;end
;%;---------------------------------------------------
;xo=x;    %  saving of series yo(xo) with gaps and noise
;yo=y;
;
;y=y-mean(y);
;fa=1;
;win=1;   
;if win
;  window=hamming(length(y)); 
;  y=y.*window;     % reconstruction is a bit better with windowing
;  fa=2;     
;end
;
;y=y+offset;     % general case: series is not centered at 0  
;yo=yo+offset;
;yc=yc+offset; 
;
;
;% end of preparation of artificial time serie y(x)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;
;%!!!!!!!!!!!!!!  call of procedure  !!!!!!!!!!!!!!!
;
;[wk1,wk2,ph,vari,hifac,ofac,F]=lspr(x,y);
;
;%;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
;
;%amplitude
;am=fa*sqrt(wk2*2*vari*2/n); % not considered here 
;
;
;
;
;%manipulation of the Fourier spectrum F:
;Fo=F;
;conf=98;   %confidence level 
;alpha=(100-conf)/2/100;
;a=norminv([alpha,1-alpha],0,1);   %
;level=a(2) ; 
;nf=length(F);
;spec=F(2:fix(nf/4));  %consider only a part of the spectrum for confidence estimation
;noise_limit=sqrt(level*mean(spec.*conj(spec))); %  level*mean power of spectra
;
;% or just a free choice in case of synthetic data:
;level=max(abs(F(2:end)))/3;
;noise_limit=level;
;
;
;ind=find(abs(F) < noise_limit) ;
;F(ind)=0; F(1)=Fo(1);           %removal of noise
;%-------------------
;%spectrum plot
;figure(2)
;fs=15
;lw=2
;clf
;%;frequency axis in cycles per day
;dim=length(wk1);
;f=wk1*length(xc)/days;
;ao_spec=abs(Fo(2:dim+1))*2/length(F);
;plot(f,ao_spec,'r','linewidth',1.2)
;hold on
;a_spec=abs(F(2:dim+1))*2/length(F);
;plot(f,a_spec,'*','markersize',8)
;set(gca,'xscale','log')
;xli=[1e-1,10];
;xlim(xli);
;ylim([0,max(ao_spec)*1.2])
;yli=get(gca,'ylim');
;plot(xli,noise_limit*2/length(F)*[1,1],'k--','linewidth',2)
;ome1=ome/days;
;for i=1:nn
;plot(ome1(i)*[1,1],yli,'k--','linewidth',1.2)
;end
;leg=legend('original spectrum','manipulated',2)
;set(leg,'fontsize',13)
;xlabel('frequency [cycles/day]','fontsize',fs)
;ylabel('amplitude [arbitrary units]','fontsize',fs)
;
;set(gca,'xminortick','on','fontsize',fs,'linewidth',2,'ticklength',[0.04 0.04])
;
;
;filenam='/export/scratch/picture/eps/lspr_plot_spec'
;%print('-depsc2','-painters',filenam );
;%print('-djpeg','-painters',filenam );
;%--------------------------------------------------
;figure(1)
;
;Fb=ifft(F);
;yf=real(Fb);     % inverse Fourier transform    
;% generation of 'time vector' xf belonging to 'time series' yf:
;xf=x(1)+[0 (1:length(F))/(wk1(2)-wk1(1))/length(F)];
;ind=find(xf <= x(end));
;xf=xf(ind);
;yf=yf(ind);
;
;% window correction (rescaling to the state without windowing)
;if win 
;yf=yf./hamming(length(yf))';
;end
;
;% adjustment/scaling of yf by using the input series yo:    
;%sc=mean(abs(yo-mean(yo)))/mean(abs(yf));  %not necessary
;sc=1;
;yf=sc*yf + real(F(1));
;
;
;
;%begin of plotting of time series:
;
;clf
;xli=[20,48]
;%yli=[min(yc)-20, max(yc)+20];
;yli=[0,40];
;lw=1.2
;fs=13
;subplot(4,1,1)
;plot(xc/ndp,yc,'k','linewidth',lw)
;text(xli(1)+1,yli(2)-6,'a)','fontsize',fs)
;xlim(xli)
;ylim(yli)
;ylabel('y','fontsize',fs)
;
;set(gca,'yminortick','on','xminortick','on','fontsize',fs,'linewidth',2,'ticklength',[0.03 0.03])
;
;
;subplot(4,1,2)
;plot(x/ndp,y,'r')
;text(xli(1)+1,yli(2)-6,'b)','fontsize',fs)
;xlim(xli)
;ylim(yli)
;ylabel('y','fontsize',fs)
;
;set(gca,'yminortick','on','xminortick','on','fontsize',fs,'linewidth',2,'ticklength',[0.03 0.03])
;
;
;subplot(4,1,3)
;
;plot(xo/ndp,yo,'r')
;hold on
;
;plot(xc/ndp,yc,'k','linewidth',lw)
;plot(xf/ndp,yf,'linewidth',lw)
;text(xli(1)+1,yli(2)-6,'c)','fontsize',fs)
;ylabel('y','fontsize',fs)
;xlim(xli)
;ylim(yli)
;
;set(gca,'yminortick','on','xminortick','on','fontsize',fs,'linewidth',2,'ticklength',[0.03 0.03])
;
;subplot(4,1,4)
;
;plot(xo/ndp,yo,'r')
;hold on
;plot(xc/ndp,yc,'k','linewidth',3)
;plot(xf/ndp,yf,'linewidth',lw)
;
;ylabel('y','fontsize',fs)
;
;xlabel('t [day]','fontsize',fs)
;xlim([23,33])
;ylim(yli)
;text(23+.3,yli(2)-6,'d)','fontsize',fs)
;set(gca,'yminortick','on','xminortick','on','fontsize',fs,'linewidth',2,'ticklength',[0.03 0.03])
;
;
;filenam='/export/scratch/picture/eps/lspr_plot'
;%print('-depsc2','-painters',filenam );
;%print('-djpeg','-painters',filenam );
;
END
