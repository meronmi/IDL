# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 23:44:06 2015

@author: raul
"""
from numba import jit

def SAILH(LAI,LIDF,ts,to,psi,HotSpot,leaf_r,leaf_t,soil_r,fdirect):
    import numpy as np
    import math
    #Original code written in Fortran 77
    #translated by R. Lopez, Ispra December 2015
    #Computation of frecuencies based on leaf inclination distribution function
    #LIDF (integer): 1-Planophile, 2-Erectophile, 3-Plagiophyle, 4-Extremphile, 5-Spherophile
    leaf_incl=np.array([0,10,20,30,40,50,60,70,80,82,84,86,88,90]) #leaf inclination classes (in degrees)
    freq=np.zeros(len(leaf_incl))
    l_classes=np.arange(1,len(leaf_incl))
    for c_class in l_classes:
        var1=leaf_incl[c_class-1]*math.pi/180
        var2=leaf_incl[c_class]*math.pi/180
        if LIDF==1:
            freq[c_class]=((2/math.pi)*(var2+(0.5*math.sin(2*var2))))-((2/math.pi)*(var1+(0.5*math.sin(2*var1))))
        elif LIDF==2:
            freq[c_class]=((2/math.pi)*(var2-(0.5*math.sin(2*var2))))-((2/math.pi)*(var1-(0.5*math.sin(2*var1))))
        elif LIDF==3:
            freq[c_class]=((2/math.pi)*(var2-(0.25*math.sin(4*var2))))-((2/math.pi)*(var1-(0.25*math.sin(4*var1))))
        elif LIDF==4:
            freq[c_class]=((2/math.pi)*(var2+(0.25*math.sin(4*var2))))-((2/math.pi)*(var1+(0.25*math.sin(4*var1))))
        elif LIDF==5:
            freq[c_class]=(-math.cos(var2))-(-math.cos(var1))
    leaf_incl=[5, 15, 25, 35, 45, 55,65,75,81,83,85,87,89]
    freq=freq*100
    freq=freq[1:len(freq)] #final vector with frequencies
    leaf_r=leaf_r*100 #spectral
    leaf_t=leaf_t*100 #spectral
    #Conversion degrees to radians
    rad_ts=ts*math.pi/180
    rad_to=to*math.pi/180
    psi=psi*math.pi/180
    #Previous computations
    rtp=(leaf_r+leaf_t)*0.005 #spectral
    rtm=(leaf_r-leaf_t)*0.005 #spectral
    tants=math.tan(rad_ts)
    tanto=math.tan(rad_to)
    cspsi=math.cos(psi)
    dso=math.sqrt(math.pow(tants,2)+math.pow(tanto,2)-2*tants*tanto*cspsi)
    alf=1e6
    if HotSpot > 0:
        alf=dso/HotSpot
    psir=psi
    #Initiating sums
    sbf=0
    sks=0
    sko=0
    sw1=0
    sw2=0
    #Computation of leaf area distribution
    l_classes=np.arange(0,len(leaf_incl))
    for c_class in l_classes:
        ttl=leaf_incl[c_class]
        cstl=math.cos(ttl*math.pi/180)
        cs2tl=math.pow(cstl,2)
        sn2tl=1-cs2tl
        sntl=math.sqrt(sn2tl)
        sbf=sbf+cs2tl*freq[c_class]
        bts=math.pi
        if (ttl+ts) > 90:
            bts=math.acos(-cstl/(sntl*tants))
        sks=sks+((bts-math.pi*0.5)*cstl+math.sin(bts)*tants*sntl)*freq[c_class]
        bto=math.pi
        if (ttl+to) > 90:
            bto=math.acos(-cstl/(sntl*tanto))
        sko=sko+((bto-math.pi*0.5)*cstl+math.sin(bto)*tanto*sntl)*freq[c_class]
        btran1=math.fabs(bts-bto)
        btran2=2*math.pi-bts-bto
        if psir <= btran1:
            bt1=psir
            bt2=btran1
            bt3=btran2
        else:
            bt1=btran1
            if psir<=btran2:
                bt2=psir
                bt3=btran2
            else:
                bt2=btran2
                bt3=psir
        tsin=0.5*sn2tl*tants*tanto
        t1=cs2tl+tsin*cspsi
        sw1=sw1+t1*freq[c_class]
        if bt2>0:
            t3=tsin*2
            if (bts==math.pi) or (bto==math.pi):
                t3=cs2tl/(math.cos(bts)*math.cos(bto))
            t2=-bt2*t1+math.sin(bt2)*(t3+math.cos(bt1)*math.cos(bt3)*tsin)
            sw2=sw2+t2*freq[c_class]
    #Computation of extinction and scattering coefficients
    sbf=sbf*rtm*LAI*0.01 #spectral
    att=LAI*(1-rtp)+sbf #spectral
    sig=LAI*rtp+sbf #spectral
    ks=sks*0.02*LAI/math.pi
    sb=ks*rtp+sbf #spectral
    sf=ks*rtp-sbf #spectral
    ko=sko*0.02*LAI/math.pi
    uf=ko*rtp-sbf #spectral
    ub=ko*rtp+sbf #spectral
    w=0.01*LAI*(leaf_r*0.01*sw1+2*rtp*sw2/math.pi) #spectral
    #Computation of layer optical properties
    m=np.sqrt(np.power(att,2)-np.power(sig,2))
    h1=(att+m)/sig
    h2=1/h1
    e1=np.exp(m)
    e2=1/e1
    dns=np.power(ks,2)-np.power(m,2)
    dno=np.power(ko,2)-np.power(m,2)
    cs=(sb*(ks-att)-sf*sig)/dns
    ds=(-sf*(ks+att)-sb*sig)/dns
    co=(ub*(ko-att)-uf*sig)/dno
    do=(-uf*(ko+att)-ub*sig)/dno
    tss=math.exp(-ks)
    too=math.exp(-ko)
    #Computation of the Hot Spot effect
    if alf==0:
        #In the Hot Spot
        tsstoo=tss
        sumint=(1-tss)/ks
    else:
        #Outsode the Hot Spot
        fhot=math.sqrt(ko*ks)
        #Integrated by the exponential Simpson method in 20 steps
        #steps are defined in equal parts of the slope
        #of the joint probability function
        x1=0
        y1=0
        f1=1
        fint=(1-math.exp(-alf))*0.05
        sumint=0
        hs_steps=np.arange(1,21)
        for st in hs_steps:
            if st<20:
                x2=-math.log(1-st*fint)/alf
            else:
                x2=1
            y2=-(ko+ks)*x2+fhot*(1-math.exp(-alf*x2))/alf
            f2=math.exp(y2)
            sumint=sumint+(f2-f1)*(x2-x1)/(y2-y1)
            x1=x2
            y1=y2
            f1=f2
        tsstoo=f1
    rsos=w*sumint
    #Computation of other reflectances and transmittances
    dnd=h1*e1-(h2*e2)
    rdd=(e1-e2)/dnd
    tdd=(h1-h2)/dnd
    rsd=cs*(1-tss*tdd)-ds*rdd
    tsd=ds*(tss-tdd)-cs*tss*rdd
    rdo=co*(1-too*tdd)-do*rdd
    tdo=do*(too-tdd)-co*too*rdd
    ho=(sf*co+sb*do)/(ko+ks)
    rsod=ho*(1-tss*too)-co*tsd*too-do*rsd
    rso=rsos+rsod
    rn=1-soil_r*rdd
    fdiff=1-fdirect
    rsot=rso+soil_r*(tsstoo+(tdo*(tss+tsd)+too*(tsd+tss*soil_r*rdd))/rn)
    rdot=rdo+tdd*soil_r*(tdo+too)/rn
    canopy_r=rsot*fdirect+rdot*fdiff
    return(canopy_r*100)

def getPROSPECTcoef():
    import shelve
    pr=shelve.open('X:/Radiative_Transfer/PROSPECT_coef_v5',flag='r')
    data=pr['data']
    pr.close()
    return data

def PROSPECT_5(N, Cab, Car, Cw, Cm,data):
    # ***********************************************************************
    # Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G.,
    # Hosgood B. (1996), Estimating leaf biochemistry using the PROSPECT
    # leaf optical properties model, Remote Sens. Environ., 56:194-202.
    # Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical
    # properties spectra, Remote Sens. Environ., 34:75-91.
    # Féret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical
    # Properties Model Separating Photosynthetic Pigments, Remote Sensing of
    # Environment
    # ***********************************************************************
    # Plant leaf reflectance and transmittance are calculated from 400 nm to
    # 2500 nm (1 nm step) with the following parameters:
    #
    #       - N   = leaf structure parameter
    #       - Cab = chlorophyll a+b content in µg/cm²
    #       - Car = carotenoids content in µg/cm²
    #       - Cw  = equivalent water thickness in g/cm² or cm
    #       - Cm  = dry matter content in g/cm²
    #
    # Here are some examples observed during the LOPEX'93 experiment on
    # fresh (F) and dry (D) leaves :
    #
    # ---------------------------------------------
    #                N     Cab     Cw        Cm
    # ---------------------------------------------
    # min          1.000    0.0  0.004000  0.001900
    # max          3.000  100.0  0.040000  0.016500
    # corn (F)     1.518   58.0  0.013100  0.003662
    # rice (F)     2.275   23.7  0.007500  0.005811
    # clover (F)   1.875   46.7  0.010000  0.003014
    # laurel (F)   2.660   74.1  0.019900  0.013520
    # ---------------------------------------------
    # min          1.500    0.0  0.000063  0.0019
    # max          3.600  100.0  0.000900  0.0165
    # bamboo (D)   2.698   70.8  0.000117  0.009327
    # lettuce (D)  2.107   35.2  0.000244  0.002250
    # walnut (D)   2.656   62.8  0.000263  0.006573
    # chestnut (D) 1.826   47.7  0.000307  0.004305
    # ---------------------------------------------
    # _______________________________________________________________________

    import numpy as np
    from scipy.special import exp1
    #data=np.genfromtxt('X:/Radiative_Transfer/PROSPECT_coef_v5.csv',delimiter=';')
    #Now given as input parameter
    l = data[:, 0]
    n = data[:, 1]
    k = (Cab*data[:,2]+Car*data[:, 3]+Cw*data[:, 4]+Cm*data[:, 5])/N
    k[k == 0] = np.spacing(1)
    trans = (1-k)*np.exp(-k)+np.power(k, 2)*exp1(k)
    # ***********************************************************************
    # reflectance and transmittance of one layer
    # ***********************************************************************
    # Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969),
    # Interaction of isotropic ligth with a compact plant leaf, J. Opt.
    # Soc. Am., 59(10):1376-1379.
    # ***********************************************************************
    #
    # reflectivity and transmissivity at the interface
    #--------------------------------------------------
    alpha=40
    t12=tav(alpha,n)
    t21=tav(90,n)/np.power(n,2)
    r12=1-t12
    r21=1-t21
    x=tav(alpha,n)/tav(90,n)
    y=x*(tav(90,n)-1)+1-tav(alpha,n)
    #
    #reflectance and transmittance of the elementary layer N=1
    #---------------------------------------------------------
    ra=r12+(t12*t21*r21*np.power(trans,2))/(1-np.power(r21,2)*np.power(trans,2))
    ta=(t12*t21*trans)/(1-np.power(r21,2)*np.power(trans,2))
    r90=(ra-y)/x
    t90=ta/x
    #
    #---------------------------------------------------------------------------
    #reflectance and transmittance of N layers
    #---------------------------------------------------------------------------
    # Stokes G.G. (1862), On the intensity of the light reflected from
    # or transmitted through a pile of plates, Proc. Roy. Soc. Lond.,
    # 11:545-556.
    # ***********************************************************************
    delta=np.sqrt(np.power((np.power(t90,2)-np.power(r90,2)-1),2)-4*np.power(r90,2))
    beta=(1+np.power(r90,2)-np.power(t90,2)-delta)/(2*r90)
    va=(1+np.power(r90,2)-np.power(t90,2)+delta)/(2*r90)
    denvb = va*(beta-r90)  #This is mine (adapted from original PROSPECT -5
    denvb[denvb<1e-14] = 1e-14  #This is mine (adapted from original PROSPECT - 5
    vb=np.sqrt(beta*(va-r90)/denvb)  #This is mine (adapted from original PROSPECT - 5
    vbNN = np.power(vb,N-1)
    vbNNinv = 1/vbNN
    vainv = 1/va
    s1=ta*t90*(vbNN-vbNNinv)
    s2=ta*(va-vainv)
    s3=va*vbNN-vainv*vbNNinv-r90*(vbNN-vbNNinv)
    RN=ra+s1/s3
    TN=s2/s3
    return l,RN,TN


def tav(tetain, ref):
    # ***********************************************************************
    # Stern F. (1964), Transmission of isotropic radiation across an
    # interface between two dielectrics, Appl. Opt., 3(1):111-113.
    # Allen W.A. (1973), Transmission of isotropic light across a
    # dielectric surface in two and three dimensions, J. Opt. Soc. Am.,
    # 63(6):664-666.
    # ***********************************************************************
    # Féret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical
    # Properties Model Separating Photosynthetic Pigments, Remote Sensing of
    # Environment
    # ***********************************************************************
    import math
    import numpy as np
    s=len(ref)
    teta=tetain*math.pi/180
    r2=np.power(ref,2)
    rp=r2+1
    rm=r2-1
    a=np.power((ref+1),2)/2
    k=-np.power((r2-1),2)/4
    ds=math.sin(teta)
    k2=np.power(k,2)
    rm2=np.power(rm,2)
    if teta == 0:
        f=4*ref/np.power((ref+1),2)
    else:
        if teta==math.pi/2:
            b1=np.zeros(s)
        else:
            b1=np.sqrt(np.power((np.power(ds,2)-rp/2),2)+k)
        b2=np.power(ds,2)-rp/2
        b=b1-b2
        ts=(k2/(6*np.power(b,3))+k/b-b/2)-(k2/(6*np.power(a,3))+k/a-a/2)
        tp1=-2*r2*(b-a)/np.power(rp,2)
        tp2=-2*r2*rp*np.log(b/a)/rm2
        tp3=r2*(np.power(b,-1)-np.power(a,-1))/2
        tp4=16*np.power(r2,2)*(np.power(r2,2)+1)*np.log((2*rp*b-rm2)/(2*rp*a-rm2))/(np.power(rp,3)*rm2)
        tp5=16*np.power(r2,3)*(np.power((2*rp*b-rm2),-1)-np.power((2*rp*a-rm2),-1))/np.power(rp,3)
        tp=tp1+tp2+tp3+tp4+tp5
        f=(ts+tp)/(2*np.power(ds,2))
    return(f)

def soilspec(wvlength,rsl1,rsl2,rsl3,rsl4):
    #
    #    rsl=soilspec(wvlength,rsl1,rsl2,rsl3,rsl4);
    #
    #     function soilspec calculates soil reflectance spectra using
    #     four weights for four basis functions
    #     basis functions for soil spectral reflectance phis1, phis2,
    #     phis3 and phis4 (Price, 1990)
    #     wvlength, wavelenght (nm), single value or vector betwen [400-2500]
    #
    #     rsl1 = [0.05, 0.4],
    #     rsl2 = [ - 0.1, 0.1],
    #     rsl3 = [ - 0.05, 0.05],
    #     rsl4 = [ - 0.04, 0.04],
    import numpy as np
    rlambda=np.arange(400,2501,5)
    phis10=np.array([0.081,0.089,0.098,0.107,0.116,0.125,0.133,0.142,0.151,0.159,0.168,0.176,0.185,0.194,0.202,0.211,0.219,0.228,0.236,0.245,0.254,0.262,0.271,0.280,0.289,0.298,0.308,0.318,0.328,0.338,0.349,0.361,0.373,0.385,0.398,0.410,0.421,0.431,0.441,0.450,0.459,0.466,0.474,0.482,0.489,0.497,0.504,0.512,0.519,0.527,0.534,0.542,0.549,0.557,0.565,0.573,0.581,0.589,0.598,0.606,0.615,0.624,0.633,0.642,0.651,0.661,0.670,0.679,0.688,0.697,0.706,0.714,0.723,0.731,0.739,0.747,0.754,0.762,0.769,0.776,0.782,0.789,0.794,0.800,0.805,0.810,0.814,0.819,0.822,0.826,0.829,0.833,0.836,0.840,0.844,0.847,0.851,0.855,0.859,0.863,0.867,0.871,0.876,0.880,0.884,0.889,0.894,0.898,0.903,0.908,0.913,0.918,0.924,0.929,0.935,0.940,0.946,0.952,0.958,0.963,0.969,0.975,0.980,0.986,0.992,0.997,1.003,1.008,1.014,1.019,1.025,1.030,1.035,1.041,1.046,1.051,1.056,1.061,1.065,1.069,1.074,1.078,1.081,1.085,1.088,1.092,1.095,1.098,1.100,1.103,1.106,1.108,1.111,1.114,1.116,1.119,1.122,1.125,1.128,1.131,1.133,1.136,1.139,1.142,1.146,1.149,1.153,1.156,1.160,1.164,1.168,1.173,1.177,1.181,1.184,1.187,1.189,1.191,1.191,1.191,1.191,1.190,1.188,1.186,1.183,1.179,1.174,1.170,1.164,1.158,1.151,1.143,1.136,1.127,1.117,1.101,1.081,1.055,1.020,0.979,0.935,0.889,0.845,0.811,0.788,0.773,0.762,0.754,0.748,0.744,0.743,0.743,0.746,0.750,0.757,0.766,0.777,0.789,0.803,0.817,0.832,0.848,0.864,0.881,0.898,0.914,0.930,0.945,0.960,0.974,0.988,1.001,1.014,1.026,1.037,1.048,1.058,1.068,1.077,1.085,1.093,1.100,1.107,1.113,1.119,1.125,1.130,1.135,1.139,1.143,1.147,1.150,1.153,1.155,1.157,1.159,1.161,1.162,1.162,1.163,1.163,1.162,1.161,1.159,1.157,1.154,1.151,1.148,1.144,1.139,1.134,1.129,1.122,1.116,1.109,1.101,1.094,1.088,1.082,1.078,1.074,1.071,1.066,1.061,1.056,1.050,1.044,1.038,1.030,1.019,1.004,0.985,0.962,0.934,0.895,0.843,0.780,0.707,0.625,0.551,0.495,0.456,0.422,0.389,0.360,0.338,0.326,0.323,0.323,0.325,0.329,0.334,0.340,0.347,0.355,0.364,0.374,0.385,0.397,0.408,0.420,0.433,0.446,0.459,0.472,0.486,0.500,0.514,0.528,0.542,0.555,0.569,0.582,0.595,0.608,0.621,0.633,0.646,0.659,0.671,0.683,0.695,0.707,0.719,0.731,0.741,0.750,0.759,0.766,0.773,0.778,0.783,0.786,0.789,0.791,0.793,0.795,0.797,0.798,0.799,0.800,0.801,0.801,0.801,0.801,0.801,0.800,0.799,0.798,0.796,0.794,0.792,0.789,0.785,0.782,0.778,0.773,0.768,0.763,0.757,0.751,0.745,0.738,0.731,0.723,0.715,0.707,0.698,0.688,0.678,0.668,0.657,0.645,0.632,0.620,0.606,0.592,0.577,0.562,0.546,0.530,0.515,0.499,0.484,0.469,0.454,0.439,0.424,0.410,0.396,0.381,0.367,0.353,0.339,0.324,0.310,0.295,0.280,0.265,0.250,0.235])
    phis20=np.array([0.253,0.248,0.243,0.238,0.232,0.226,0.219,0.212,0.205,0.197,0.189,0.180,0.171,0.161,0.151,0.141,0.130,0.119,0.107,0.095,0.082,0.069,0.055,0.041,0.026,0.011,-.005,-.021,-.038,-.055,-.073,-.091,-.109,-.127,-.146,-.164,-.183,-.201,-.220,-.239,-.256,-.271,-.284,-.296,-.305,-.313,-.320,-.324,-.327,-.329,-.332,-.335,-.338,-.341,-.344,-.348,-.351,-.355,-.359,-.363,-.367,-.371,-.374,-.377,-.380,-.381,-.383,-.383,-.383,-.383,-.381,-.380,-.377,-.374,-.371,-.367,-.362,-.357,-.351,-.344,-.337,-.330,-.322,-.315,-.307,-.300,-.292,-.285,-.278,-.270,-.263,-.255,-.248,-.241,-.233,-.226,-.218,-.211,-.204,-.196,-.189,-.182,-.175,-.168,-.161,-.154,-.147,-.140,-.133,-.126,-.119,-.113,-.106,-.099,-.093,-.086,-.080,-.073,-.067,-.060,-.053,-.046,-.039,-.032,-.024,-.017,-.009,-.001,0.007,0.015,0.023,0.032,0.040,0.049,0.058,0.067,0.076,0.085,0.095,0.104,0.114,0.124,0.134,0.144,0.154,0.164,0.175,0.185,0.195,0.206,0.216,0.227,0.237,0.247,0.258,0.268,0.279,0.289,0.300,0.310,0.321,0.332,0.342,0.353,0.363,0.374,0.385,0.395,0.406,0.417,0.428,0.438,0.449,0.460,0.471,0.483,0.494,0.506,0.518,0.531,0.544,0.556,0.570,0.583,0.597,0.611,0.625,0.639,0.654,0.669,0.684,0.699,0.715,0.730,0.745,0.759,0.772,0.784,0.796,0.807,0.817,0.826,0.834,0.842,0.849,0.855,0.860,0.865,0.868,0.871,0.873,0.876,0.879,0.882,0.885,0.889,0.893,0.898,0.903,0.908,0.914,0.920,0.926,0.933,0.940,0.948,0.956,0.964,0.973,0.982,0.990,0.999,1.007,1.014,1.022,1.029,1.036,1.042,1.048,1.054,1.060,1.065,1.070,1.074,1.079,1.083,1.086,1.090,1.093,1.095,1.098,1.100,1.103,1.105,1.108,1.111,1.114,1.117,1.120,1.123,1.127,1.130,1.134,1.137,1.141,1.145,1.149,1.153,1.157,1.161,1.165,1.169,1.172,1.175,1.177,1.178,1.179,1.180,1.180,1.180,1.179,1.178,1.176,1.174,1.171,1.169,1.166,1.163,1.160,1.157,1.153,1.146,1.133,1.115,1.089,1.052,1.002,0.940,0.874,0.807,0.740,0.677,0.624,0.582,0.550,0.527,0.510,0.500,0.497,0.501,0.507,0.514,0.523,0.533,0.544,0.557,0.570,0.584,0.598,0.613,0.629,0.646,0.663,0.681,0.700,0.719,0.738,0.757,0.775,0.792,0.809,0.826,0.842,0.857,0.872,0.887,0.901,0.914,0.927,0.940,0.952,0.963,0.974,0.985,0.995,1.004,1.013,1.022,1.030,1.037,1.044,1.051,1.057,1.063,1.068,1.073,1.077,1.081,1.084,1.087,1.089,1.091,1.092,1.093,1.093,1.093,1.092,1.091,1.089,1.087,1.085,1.082,1.078,1.074,1.070,1.065,1.059,1.053,1.047,1.040,1.033,1.025,1.017,1.009,0.999,0.990,0.980,0.970,0.959,0.947,0.935,0.923,0.910,0.897,0.883,0.869,0.855,0.840,0.824,0.808,0.792,0.775,0.757,0.740,0.721,0.703,0.683,0.664,0.644,0.623,0.602,0.581,0.559,0.536,0.513,0.490,0.466,0.442,0.417,0.392,0.366])
    phis30=np.array([-.455,-.412,-.369,-.327,-.286,-.245,-.204,-.164,-.124,-.085,-.046,-.008,0.030,0.067,0.104,0.140,0.176,0.211,0.246,0.280,0.314,0.348,0.381,0.413,0.445,0.477,0.507,0.538,0.567,0.597,0.625,0.653,0.681,0.708,0.735,0.761,0.785,0.809,0.831,0.852,0.871,0.889,0.906,0.921,0.935,0.948,0.959,0.969,0.978,0.986,0.994,1.000,1.006,1.011,1.014,1.017,1.019,1.020,1.020,1.019,1.018,1.015,1.012,1.008,1.004,0.998,0.992,0.985,0.978,0.969,0.960,0.950,0.940,0.929,0.916,0.904,0.890,0.876,0.861,0.845,0.828,0.811,0.794,0.775,0.756,0.736,0.716,0.695,0.673,0.650,0.627,0.605,0.583,0.561,0.541,0.520,0.501,0.481,0.463,0.444,0.427,0.409,0.392,0.375,0.358,0.341,0.324,0.307,0.290,0.273,0.257,0.240,0.224,0.207,0.191,0.174,0.158,0.142,0.125,0.109,0.093,0.076,0.060,0.043,0.027,0.011,-.006,-.023,-.039,-.056,-.072,-.089,-.105,-.120,-.135,-.150,-.165,-.179,-.192,-.205,-.218,-.231,-.243,-.255,-.266,-.277,-.288,-.298,-.308,-.318,-.328,-.337,-.346,-.355,-.363,-.372,-.379,-.387,-.394,-.402,-.409,-.417,-.425,-.433,-.442,-.450,-.459,-.469,-.478,-.488,-.498,-.507,-.516,-.525,-.533,-.540,-.548,-.555,-.561,-.567,-.572,-.576,-.578,-.579,-.578,-.576,-.573,-.568,-.562,-.555,-.546,-.537,-.527,-.516,-.503,-.484,-.456,-.420,-.375,-.323,-.264,-.202,-.138,-.086,-.051,-.030,-.015,-.005,-.001,0.001,0.002,0.001,-.001,-.004,-.010,-.018,-.028,-.040,-.054,-.070,-.089,-.108,-.128,-.147,-.167,-.186,-.205,-.225,-.244,-.263,-.281,-.299,-.316,-.333,-.349,-.365,-.380,-.394,-.408,-.422,-.435,-.447,-.459,-.470,-.481,-.490,-.499,-.507,-.515,-.522,-.528,-.533,-.538,-.542,-.545,-.547,-.549,-.550,-.550,-.550,-.549,-.547,-.544,-.541,-.537,-.531,-.526,-.519,-.511,-.503,-.494,-.485,-.476,-.468,-.460,-.452,-.445,-.439,-.433,-.427,-.422,-.418,-.416,-.415,-.415,-.416,-.413,-.406,-.395,-.381,-.363,-.343,-.321,-.289,-.231,-.147,-.071,-.013,0.032,0.077,0.122,0.160,0.185,0.199,0.203,0.205,0.207,0.208,0.209,0.209,0.210,0.210,0.209,0.209,0.208,0.206,0.204,0.202,0.200,0.197,0.194,0.191,0.187,0.183,0.178,0.174,0.168,0.163,0.157,0.150,0.143,0.136,0.129,0.121,0.112,0.104,0.095,0.086,0.077,0.068,0.060,0.051,0.043,0.034,0.026,0.018,0.009,0.001,-.007,-.015,-.022,-.030,-.037,-.043,-.049,-.054,-.059,-.063,-.067,-.071,-.074,-.076,-.078,-.079,-.080,-.081,-.081,-.080,-.079,-.077,-.075,-.073,-.070,-.067,-.064,-.060,-.056,-.052,-.047,-.042,-.037,-.031,-.025,-.019,-.012,-.005,0.003,0.010,0.019,0.027,0.036,0.045,0.055,0.065,0.076,0.087,0.098,0.110,0.122,0.134,0.144,0.153,0.160,0.165,0.168,0.170,0.171,0.169,0.166,0.162,0.157,0.151,0.144,0.137,0.128,0.119,0.109,0.098,0.086,0.074,0.061])
    phis40=np.array([0.058,0.070,0.081,0.092,0.103,0.112,0.122,0.131,0.139,0.148,0.155,0.162,0.169,0.175,0.181,0.186,0.191,0.196,0.200,0.203,0.206,0.209,0.211,0.213,0.214,0.215,0.215,0.215,0.214,0.213,0.212,0.210,0.207,0.204,0.201,0.197,0.192,0.187,0.182,0.176,0.169,0.162,0.154,0.146,0.138,0.129,0.119,0.109,0.099,0.089,0.079,0.069,0.059,0.049,0.039,0.029,0.019,0.009,-.001,-.011,-.021,-.031,-.041,-.051,-.060,-.069,-.077,-.085,-.092,-.099,-.105,-.111,-.116,-.121,-.125,-.129,-.133,-.136,-.138,-.140,-.142,-.143,-.145,-.147,-.148,-.150,-.151,-.153,-.154,-.156,-.157,-.159,-.160,-.162,-.163,-.164,-.165,-.165,-.165,-.165,-.165,-.164,-.163,-.162,-.161,-.159,-.157,-.155,-.152,-.149,-.146,-.142,-.138,-.133,-.128,-.122,-.116,-.109,-.102,-.094,-.085,-.076,-.067,-.057,-.046,-.035,-.024,-.012,0.000,0.012,0.024,0.036,0.049,0.061,0.074,0.087,0.100,0.113,0.126,0.140,0.153,0.167,0.181,0.195,0.210,0.224,0.238,0.253,0.267,0.282,0.297,0.311,0.326,0.341,0.356,0.371,0.385,0.400,0.415,0.430,0.446,0.461,0.477,0.493,0.510,0.527,0.544,0.562,0.579,0.597,0.614,0.631,0.648,0.664,0.680,0.695,0.710,0.725,0.738,0.752,0.764,0.776,0.787,0.797,0.805,0.811,0.815,0.816,0.816,0.816,0.818,0.821,0.820,0.811,0.793,0.765,0.720,0.658,0.579,0.494,0.408,0.322,0.242,0.181,0.141,0.119,0.108,0.106,0.113,0.124,0.134,0.142,0.149,0.157,0.169,0.184,0.204,0.228,0.255,0.283,0.313,0.344,0.377,0.411,0.446,0.480,0.514,0.547,0.579,0.610,0.640,0.669,0.698,0.726,0.752,0.777,0.801,0.824,0.846,0.867,0.886,0.904,0.921,0.937,0.952,0.967,0.980,0.992,1.004,1.015,1.025,1.034,1.042,1.050,1.056,1.062,1.067,1.071,1.074,1.077,1.078,1.079,1.077,1.074,1.069,1.063,1.054,1.044,1.033,1.019,1.004,0.988,0.971,0.954,0.937,0.921,0.906,0.893,0.882,0.872,0.865,0.858,0.851,0.844,0.838,0.830,0.823,0.814,0.805,0.790,0.761,0.716,0.655,0.579,0.491,0.394,0.287,0.171,0.045,-.078,-.177,-.252,-.303,-.330,-.333,-.331,-.329,-.327,-.326,-.324,-.323,-.321,-.319,-.316,-.313,-.309,-.304,-.298,-.293,-.288,-.282,-.277,-.271,-.265,-.257,-.247,-.236,-.222,-.207,-.190,-.172,-.154,-.137,-.122,-.107,-.093,-.080,-.068,-.055,-.042,-.028,-.013,0.004,0.021,0.039,0.058,0.078,0.095,0.109,0.119,0.127,0.130,0.131,0.128,0.121,0.112,0.102,0.092,0.084,0.075,0.067,0.060,0.053,0.047,0.042,0.037,0.032,0.028,0.025,0.021,0.018,0.015,0.011,0.008,0.004,0.001,-.003,-.006,-.010,-.014,-.017,-.021,-.025,-.028,-.032,-.036,-.040,-.044,-.047,-.051,-.054,-.056,-.059,-.060,-.062,-.063,-.063,-.063,-.063,-.063,-.062,-.060,-.058,-.056,-.053,-.050,-.047,-.045,-.042,-.040,-.039,-.037,-.035,-.034,-.033,-.033,-.032,-.032,-.032,-.032,-.033])
    phis1=np.interp(wvlength,rlambda,phis10)
    phis2=np.interp(wvlength,rlambda,phis20)
    phis3=np.interp(wvlength,rlambda,phis30)
    phis4=np.interp(wvlength,rlambda,phis40)
    rsl=rsl1*phis1+rsl2*phis2+rsl3*phis3+rsl4*phis4
    rsl[rsl<0]=0
    return(rsl)


def skylspec(wvl,bangs):
    import numpy as np
    in_wvl=np.arange(400,2501,5)
    gamml0=np.append(np.array([.426, .438, .450, .461, .472, .483, .493, .503, .513, .523,.532, .541, .550, .559, .567, .575, .583, .591, .598, .606,.613, .620, .626, .633, .640, .647, .654, .661, .668, .675,.683, .690, .697, .705, .712, .719, .726, .733, .739, .744,.748, .750, .752, .752, .751, .749, .746, .743, .740, .736,.732, .728, .724, .720, .716, .712, .707, .703, .698, .693,.689, .684, .679, .674, .668, .663, .657, .652, .646, .640,.634, .627, .617, .605, .590, .573, .553, .531, .505, .478,.448]),np.ones(340)*0.4)
    alpl0=np.append(np.array([.394, .381, .369, .357, .345, .334, .323, .312, .302, .293,.283, .275, .266, .258, .250, .243, .236, .230, .224, .218,.213, .208, .202, .197, .192, .187, .182, .176, .171, .166,.160, .155, .150, .144, .139, .133, .128, .122, .117, .112,.107, .102, .097, .092, .087, .083, .078, .074, .070, .066,.062, .058, .054, .050, .047, .043, .040, .037, .034, .031,.028, .025, .023, .020, .017, .015, .012, .010, .008, .006,.004, .003, .002, .001]), np.zeros(347))
    gamml=np.interp(wvl,in_wvl,gamml0)
    alpl=np.interp(wvl,in_wvl,alpl0)
    gammlc = -1.4
    tausk = np.power((wvl/1000),gammlc)*bangs
    skyl = tausk*gamml+alpl
    difuse_f= skyl/(1+skyl)
    direct_f=1-difuse_f
    return direct_f

def KuuskMCCRM(LAI, clump,hotspot, theta_l, elip_l, sun_theta,view_theta, phi_sun_view ,leaf_r,leaf_t,soil_r,f_direct):
    #
    #    Markov Chain Canopy Reflectance Model by Kuusk, 1995
    #    Transcribed from 2-layer MCCRM by R.Lopez. Ispra, Feb 2016
    #    Transcription removing the second layer (understorey vegetation) to mimic the 1-layer model by Kuusk
    #
    #     LAI: leaf area Index
    #     clump: canopy clumping index
    #     hotspot: hotspot parameter
    #     theta_l: average leaf inclination angle (degrees)
    #     elip_l: eccentricity of leaf inclination distribution
    #     n_ratio: leaf refractive index [0.7-1.2]
    #     sun_theta: sun zenith angle (degrees)
    #     view_theta: view azimuth angle (degrees)
    #     phi_sun_view: relative azimuth sun-view (degrees)
    #     leaf_r: leaf reflectance
    #     leaf_t: leaf transmittance
    #     soil_r: soil reflectance
    #     f_direct= fraction of direct soybean
    #     Kuusk, A. (1995): A Markov chain model of canopy reflectance.
    #                       Agricultural and Forest Meteorology 76(3):221-236
    import numpy as np
    #Conversion degrees to radians
    sun_theta = sun_theta*np.pi/180
    view_theta = view_theta*np.pi/180
    phi_sun_view = phi_sun_view*np.pi/180
    theta_l=theta_l*np.pi/180
    n_ratio=1 #leaf refraction index, fixed
    #Parameters for gamma function
    ellmy = abs(0.059e0*elip_l*(theta_l-1.02e0)+0.02e0)
    ellsi = abs(0.0107e0 - 0.0216e0 * elip_l*(theta_l-0.265e0))
    ee = 1.0e0 - np.exp(-elip_l)
    rrl=leaf_r-np.power((1-n_ratio)/(1+n_ratio),2)
    f_diff=1-f_direct
    #
    gg1,gg2,gamm = KuuskGamma(view_theta,sun_theta,phi_sun_view,theta_l, ee,rrl,leaf_t, n_ratio, hotspot, clump, ellmy, ellsi)
    bc1,bs1,bs_sunflake = KuuskBiz(view_theta, sun_theta, phi_sun_view, LAI, hotspot, gg1, gg2, gamm, soil_r)
    rrl=leaf_r
    #Calculating diffuse reflectance
    bdplants,bdsoil=KuuskDif(view_theta,sun_theta, gg1, gg2, LAI, rrl, leaf_t, ellmy, ellsi, theta_l, f_diff, soil_r)
    #Calculating overall bidirectiona reflectance
    bq=f_direct*(bc1+bs1)+bdplants+bdsoil
    return bq

def KuuskGamma(view_theta,sun_theta,phi_sun_view, theta_l, ee,rrl,ttl, n_ratio, hotspot, clump, ellmy, ellsi):
    #*********************************************************************
    #
    #  Phase function and G-function for a Markov chain structure
    #  J. Ross,  The Radiation Regime and Architecture of Plant Stands
    #  Junk,  The Hague,  1981.
    #  Kuusk, A. A Markov chain model of canopy reflectance,
    #  Agricult. Forest Meteorol., 1995, 76(3-4): 221-236.
    #  A. Kuusk    22.03.1988 & 16.06.1992 & 20.01.2000
    #  0 <= t11 < t22 <= pi/2, 0 <= phi <= pi
    #
    #*********************************************************************
    import numpy as np
    import math
    fgamm = np.zeros((5-1+1,1))
    eps3 = .01
    eps5 = .1e-2
    eps = .1e-5
    pi21 = np.pi*0.5
    pi4 = np.pi*2
    pi12 = 1/pi4
    pi13 = pi12/1.5e0
    pi14 = pi12*4
    sth1 = np.sin(sun_theta)
    sth2 = np.sin(view_theta)
    cth1 = np.cos(sun_theta)
    cth2 = np.cos(view_theta)
    calph = sth1*sth2*np.cos(phi_sun_view)+cth1*cth2
    alph = math.acos(calph)
    salph = np.sin(alph)
    calp2 = np.cos(alph/2)
    # Phase function of specular reflection
    sa2 = 1-np.power(calp2,2)
    aag= np.sqrt(np.power(n_ratio,2)-sa2)
    gmf = (np.power((aag-calp2)/(aag+calp2),2)+np.power((np.power(n_ratio,2)*calp2-aag)/(np.power(n_ratio,2)*calp2+aag),2))/2 #Fresnel reflection
    if ee<=eps3:
        gl=1
    else:
        y4 = abs(cth2+cth1)*.5e0/calp2
        thp= math.acos(y4)
        gl = KuuskGammaLeaf(theta_l,ee,thp)
    gammf = gmf * gl * .125e0
    gr0 = (salph+(np.pi-alph)*calph)*pi13
    gt0 = (salph - alph*calph)*pi13
    if ee<.4e0:
        gr = gr0
        gt = gt0
        gg2 = .5e0
        gg1 = .5e0
        gammd = gr * rrl + gt * ttl
        clx = .1e0/hotspot
        xy = clx*np.tan(sun_theta)
        if (xy<eps):
            clmp1 = clump
        else:
            clmp1 = (1-(1-clump)*eint(xy))
        xy = clx*np.tan(view_theta)
        if (xy<eps):
            clmp2 = clump
        else:
            clmp2 = (1-(1-clump)*eint(xy))
        gg1 = gg1*clmp1
        gg2 = gg2*clmp2
        gamm = gammd+gammf
    else:
        if (sun_theta<=view_theta):
           t11 = sun_theta
           t22 = view_theta
           jj=1
        else:
           t11 = view_theta
           t22 = sun_theta
           jj=0
        sg2 = 0
        sg1 = 0
        sgmr = 0
        sgmt = 0
        yg = 0
        i1=0
        thmc = pi21-theta_l
        if (abs(thmc)<eps):
           thmc = abs(thmc)
        st1 = np.sin(t11)
        ct1 = np.cos(t11)
        st2 = np.sin(t22)
        ct2 = np.cos(t22)
        sthm = np.sin(theta_l)
        cthm = np.cos(theta_l)
        sp = np.sin(phi_sun_view)
        cp = np.cos(phi_sun_view)
        stt1 = st1*st2
        ctt1 = ct1*ct2
        e1 = st1*ct2
        e2 = ct1*st2
        s2 = e1*cp+e2
        s3 = e1*sp
        ctg2 = 1.0e30
        ctg1 = 1.0e30
        if (st1!=0):
           ctg2 = ct1/st1
        if (st2!=0):
           ctg1 = ct2/st2

        #now if con not (thcm<t22)
        #now if con not(thcm<t11)
        if not(thmc<t22):
            xg = sthm*sthm
            pp = calph*xg+ctt1*(2-3.*xg)
            pp = pp*np.pi
            yg = pp
            if (yg>0):
                sgmr = sgmr+yg
            if (yg<0):
                sgmt = sgmt-yg
            y1 = ct2*cthm
            sg1 = sg1+np.abs(y1)
            y1 = ct1*cthm
            sg2 = sg2+np.abs(y1)
            gr1 = sgmr*pi12
            gt1 = sgmt*pi12
            gr = gr0-.0102e0+(1.742e0*ellmy-.4557e0*ellsi)*(gr1-gr0)
            gt = gt0+1.14e-4+(.2693e0*ellmy+5.821e0*ellsi)*(gt1-gt0)
            gg2 = (2.653e0*ellmy+1.432e0*ellsi)*(sg2-.5e0)+.50072e0
            gg1 = (2.653e0*ellmy+1.432e0*ellsi)*(sg1-.5e0)+.50072e0
            if (jj==1):
                ggx = gg1
                gg1 = gg2
                gg2 = ggx
            gammd = gr*rrl+gt*ttl
            clx = .1e0/hotspot
            xy = clx*np.tan(sun_theta)
            if (xy<eps):
                clmp1 = clump
            else:
                clmp1 = (1-(1-clump)*eint(xy))
            xy = clx*np.tan(view_theta)
            if (xy<eps):
                clmp2 = clump
            else:
                clmp2 = (1-(1-clump)*eint(xy));
            gg1 = gg1*clmp1
            gg2 = gg2*clmp2
            gamm = gammd+gammf
        else:
            if not(thmc<t11):
                x2 = cthm/sthm
                xg = -ctg1*x2
                x1 = np.sqrt(1-xg*xg)
                fa = math.atan2(x1,xg)
                fb = pi4-fa
                xg = fb-fa
                if (xg>eps5):
                    if ((pi4-xg) < eps5):
                        xg = sthm*sthm
                        pp = calph*xg+ctt1*(2-3.*xg)
                    sfa = np.sin(fa)
                    sfb = np.sin(fb)
                    cfa = np.cos(fa)
                    cfb = np.cos(fb)
                    pp = xg*ctt1*cthm*cthm
                    y1 = xg+sfb*cfb-sfa*cfa
                    xg = cfa-cfb
                    y1 = y1*cp+sp*xg*(cfa+cfb)
                    pp = pp+stt1*.5e0*y1*sthm*sthm
                    y1 = s2*(sfb-sfa)+s3*xg
                    pp = pp+y1*sthm*cthm
                else:
                    pp = 0
                yg = pp
                if (yg>0):
                    sgmr = sgmr+yg
                if (yg<0):
                    sgmt = sgmt-yg
                xg = sthm*sthm
                pp = calph*xg+ctt1*(2-3.*xg)
                pp = pp*np.pi
                yg = pp-yg
                if (yg>0):
                    sgmr = sgmr+yg
                if (yg<0):
                    sgmt = sgmt-yg
                x2 = ct2*cthm
                x1 = st2*sthm/x2
                x1 = np.sqrt(x1*x1-1)
                xg = math.atan2(1,x1)
                xg = (xg+x1)*x2
                yg = xg*pi14
                sg1 = sg1+abs(yg)
                y1 = ct1*cthm
                sg2 = sg2+abs(y1)
                gr1 = sgmr*pi12
                gt1 = sgmt*pi12
                gr = gr0-.0102e0+(1.742e0*ellmy-.4557e0*ellsi)*(gr1-gr0)
                gt = gt0+1.14e-4+(.2693e0*ellmy+5.821e0*ellsi)*(gt1-gt0)
                gg2 = (2.653e0*ellmy+1.432e0*ellsi)*(sg2-.5e0)+.50072e0
                gg1 = (2.653e0*ellmy+1.432e0*ellsi)*(sg1-.5e0)+.50072e0
                if (jj==1):
                    ggx = gg1
                    gg1 = gg2
                    gg2 = ggx
                gammd = gr*rrl+gt*ttl
                clx = .1e0/hotspot
                xy = clx*np.tan(sun_theta)
                if (xy<eps):
                  clmp1 = clump
                else:
                  clmp1 = (1-(1-clump)*eint(xy))
                xy = clx*np.tan(view_theta)

                if (xy<eps):
                    clmp2 = clump
                else:
                    clmp2 = (1-(1-clump)*eint(xy))
                gg1 = gg1*clmp1
                gg2 = gg2*clmp2
                gamm = gammd+gammf
            else:
                x2 = cthm/sthm
                xg = -ctg1*x2
                x1 = np.sqrt(1-xg*xg)
                fa = math.atan2(x1,xg)
                fgamm[1] = fa
                fgamm[2] = pi4-fa
                xg = -ctg2*x2
                x1 = np.sqrt(1-xg*xg)
                fa = math.atan2(x1,xg)
                fb = phi_sun_view-fa

                if (fb<0):
                   fb = fb+pi4
                fgamm[3] = fb
                fgamm[4] = phi_sun_view+fa
                fgamm[0] = fgamm[4]-pi4
                i1=1
                ii = i1
                i1 = ii+1
                fa = fgamm[ii-1]
                fb = fgamm[i1-1]
                xg = fb-fa
                if (xg>eps5):
                   if ((pi4-xg)<eps5):
                      xg = sthm*sthm
                      #pp = calph*xg+ctt1*(2-3.*xg) commented out from SCRM2 (by Raul)
                      #pp = pp*np.pi
                   sfa = np.sin(fa)
                   sfb = np.sin(fb)
                   cfa = np.cos(fa)
                   cfb = np.cos(fb)
                   pp = xg*ctt1*cthm*cthm
                   y1 = xg+sfb*cfb-sfa*cfa
                   xg = cfa-cfb
                   y1 = y1*cp+sp*xg*(cfa+cfb)
                   pp = pp+stt1*.5e0*y1*sthm*sthm
                   y1 = s2*(sfb-sfa)+s3*xg
                   pp = pp+y1*sthm*cthm
                else:
                   pp = 0
                yg = pp
                if (yg>0):
                   sgmr = sgmr+yg
                if (yg<0):
                   sgmt = sgmt-yg
                x2 = ct1*cthm
                x1 = st1*sthm/x2
                x1 = np.sqrt(x1*x1-1)
                xg = math.atan2(1,x1)
                xg = (xg+x1)*x2
                yg = xg*pi14
                sg2 = sg2+abs(yg)
                x2 = ct2*cthm
                x1 = st2*sthm/x2
                x1 = np.sqrt(x1*x1-1)
                xg = math.atan2(1,x1)
                xg = (xg+x1)*x2
                yg = xg*pi14
                sg1 = sg1+abs(yg)
                gr1 = sgmr*pi12
                gt1 = sgmt*pi12
                gr = gr0-.0102e0+(1.742e0*ellmy-.4557e0*ellsi)*(gr1-gr0)
                gt = gt0+1.14e-4+(.2693e0*ellmy+5.821e0*ellsi)*(gt1-gt0)
                gg2 = (2.653e0*ellmy+1.432e0*ellsi)*(sg2-.5e0)+.50072e0
                gg1 = (2.653e0*ellmy+1.432e0*ellsi)*(sg1-.5e0)+.50072e0
                if (jj==1):
                   ggx = gg1
                   gg1 = gg2
                   gg2 = ggx
                gammd = gr*rrl+gt*ttl
                clx = .1e0/hotspot
                xy = clx*np.tan(sun_theta)
                if (xy<eps):
                   clmp1 = clump
                else:
                   clmp1 = (1-(1-clump)*eint(xy))
                xy = clx*np.tan(view_theta)
                if (xy<eps):
                   clmp2 = clump
                else:
                   clmp2 = (1-(1-clump)*eint(xy))
                gg1 = gg1*clmp1
                gg2 = gg2*clmp2
                gamm = gammd+gammf
    return gg1,gg2,gamm


def KuuskBiz(view_theta, sun_theta, phi_sun_view, LAI, hotspot, gg1, gg2, gamm, soil_r):
    import numpy as np
    import math

    eps=1.e-4
    sun_theta = sun_theta+eps
    sth2=np.sin(view_theta)
    cth2=np.cos(view_theta)
    sth1=np.sin(sun_theta)
    cth1=np.cos(sun_theta)
    cp=np.cos(phi_sun_view)
    if LAI < eps:
        bc1=0
        bs1 = soil_r
        rc1=0
        bs_sunflake = soil_r
    else:
        calph= sth2*sth1*cp+cth2*cth1
        catmp=calph
        alph=math.acos(catmp)
        delt2 = 1.0e0/np.power(cth1,2)+1.0e0/np.power(cth2,2)-2.0e0*calph/cth1/cth2
        if (abs(delt2)<eps):
            delt2 = abs(delt2)
        delta = np.sqrt(delt2)
        xyc1 = delta/hotspot
        chsh1 = np.exp(np.sqrt(gg1/cth1*gg2/cth2)*LAI*eint(xyc1))
        qq1 = np.exp(-LAI*(gg1/cth1+gg2/cth2))*chsh1
        bs1=soil_r*qq1
        bs_sunflake=soil_r*np.sqrt(qq1)
        rc1 = KuuskRhoc1(cth2,cth1,alph,LAI, hotspot, gg1, gg2, gamm)
        bc1=rc1 #This is mine (Raul)
    return bc1, bs1, bs_sunflake

def KuuskDif(view_theta,sun_theta,gg1,gg2,LAI,rrl,ttl,ellmy,ellsi,thm, f_diff, soil_r):
    import numpy as np
    if LAI<=0:
        rdd=0
        rdo=0
        rsd=0
        rso=0
        tdd=1
        tdo=0
        tsd=0
    else:
        cth2 = np.cos(view_theta)
        cth1 = np.cos(sun_theta)
        cthm = np.cos(thm)
        rtp = (rrl+ttl)/2
        rtm = (rrl-ttl)/2
        rk1 = gg1/cth1
        rk2 = gg2/cth2
        sailj = (1.289e0*ellmy-1.816e0*ellsi)*(np.power(cthm,2)-.33333333333e0)+.31823e0
        bf = rtm*sailj
        att = 1-rtp+bf
        sig = rtp+bf
        ssb = rk1*rtp+bf
        ssf = rk1*rtp-bf
        vv = rk2*rtp+bf
        uu = rk2*rtp-bf
        rmm = np.sqrt(att*att-sig*sig)
        h1 = (att+rmm)/sig
        h2 = 1.0e0/h1
        cc = (ssf*sig-ssb*(rk1-att))/(np.power(rmm,2)-np.power(rk1,2))
        dd = (ssb*sig+ssf*(rk1+att))/(np.power(rmm,2)-np.power(rk1,2))
        exmu1 = np.exp(rmm*LAI)
        exmu2 = 1./exmu1
        exku1 = np.exp(-rk1*LAI)
        delt = h1*exmu1-h2*exmu2
        rdd = (exmu1-exmu2)/delt
        tdd = (h1-h2)/delt
        aa1 = (h2*cc*exku1-dd*exmu1)/delt
        bb1 = (dd*exmu2-h1*cc*exku1)/delt
        tsd = h1*aa1*exmu2+h2*bb1*exmu1+dd*exku1
        rsd = aa1+bb1+cc
        tdo = LAI*((uu*h1+vv)*eint((rk2-rmm)*LAI)-(uu*h2+vv)*eint((rk2+rmm)*LAI))/delt
        rdo = LAI*(exmu1*(vv*h1+uu)*eint((rk2+rmm)*LAI)-exmu2*(vv*h2+uu)*eint((rk2-rmm)*LAI))/delt
        rso = LAI*((vv*dd+uu*cc)*eint((rk2+rk1)*LAI)+(vv*h1+uu)*aa1*eint((rk2+rmm)*LAI)+(vv*h2+uu)*bb1*eint((rk2-rmm)*LAI))
    sq = 1-f_diff
    ts1 = np.exp(-LAI*gg1/np.cos(sun_theta))
    ts2 = np.exp(-LAI*gg2/np.cos(view_theta))
    bdplants = sq*rso+f_diff*rdo+(sq*ts1*soil_r*tdo+sq*tsd*soil_r*tdo+f_diff*tdd*soil_r*tdo)/(1.0e0-rdd*soil_r);
    bdsoil = (sq*tsd+f_diff*tdd)*soil_r*ts2
    return bdplants, bdsoil

def KuuskRhoc1(cth2,cth1,alph,LAI, hotspot, gg1, gg2, gamm):
    #Single scattering from leaves
    import numpy as np
    xy1 = LAI*(gg1/cth1+gg2/cth2)
    ss1 = LAI/cth1/cth2*eint(xy1)
    xy2 = LAI/2.0e0*(gg1/cth1+gg2/cth2+alph/(hotspot*LAI*np.sqrt(cth1*cth2)))
    ss2 = LAI/cth1/cth2*eint(xy2)
    xy3 = LAI*(gg1/cth1+gg2/cth2+alph/(2.0e0*hotspot*LAI*np.sqrt(cth1*cth2)))
    ss3 = LAI/cth1/cth2*eint(xy3)
    rc1 = gamm*(ss1+ss2-ss3)
    return rc1

def KuuskGammaLeaf(thm,ee,thl):
    import numpy as np
    import math
    bb = 1.e0
    es = 0.e0
    tms = 0.e0
    eps = .1e0
    sthm = 0.e0
    cthm = 1.e0
    if ee < eps:
        gl = 1.0e0
    else:
        if ee == 1.0e0:
            ee = .999999e0
        if (ee != es) or (thm != tms):
            sthm = np.sin(thm)
            cthm = np.cos(thm)
            u1 = ee * cthm
            u3 = ee * sthm
            u2 = np.sqrt(1.0e0 - u1 * u1)
            u4 = np.sqrt(1.0e0 - u3 * u3)
            xg = np.log((u4+u1)/(u2-u3))
            x1 = math.atan2(u3, u4) - math.atan2(u1, u2)
            x2 = sthm * xg - cthm * x1
            bb = ee / x2
            es = ee
            tms = thm
        gl = bb/np.sqrt(1.0e0-np.power(ee*np.cos(thm-thl),2))
    return gl

def eint(xy):
    #Exponential integral for Kuusk model
    import numpy as np
    eps = .1e-6
    if (np.all(abs(xy))<eps):
        e = 1-xy*(0.5-xy/6*(1-xy*0.25))
    else:
        e = (1-np.exp(-xy))/(xy+eps)
    return e

