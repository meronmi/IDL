PRO Print_rum_hdr, lun, sensor_ID, var_ID
PRINTF, lun, 'RUM-DATABASE, created with IDL'
PRINTF, lun, '=============================================================================='
PRINTF, lun, 'SPU-File  : fake rum built from ALRMP data'
PRINTF, lun, '-----------'
PRINTF, lun, 'REGIONS'
PRINTF, lun, '-REGimg   : fake rum'
PRINTF, lun, '-REGtxt   :
PRINTF, lun, '-REGs     : 224 (1 - 224) / Used=224
PRINTF, lun, '----------
PRINTF, lun, 'LU-CLASSES
PRINTF, lun, '-SELmet   : 1
PRINTF, lun, '-Nclass   : 2
PRINTF, lun, '-SELhard  : fake rum'
PRINTF, lun, '-M3Tfil   : none  (0 optimized thresholds)'
PRINTF, lun, '----------'
PRINTF, lun, 'IN-IMG(s)'
PRINTF, lun, '-IMGmet   : 1'
PRINTF, lun, '-Nimg     : 1'
PRINTF, lun, '-IN-IMG   : fake rum'
PRINTF, lun, '-IDsensor : ' + STRTRIM(sensor_ID,2)
PRINTF, lun, '-IDvar    : ' + STRTRIM(var_ID,2)
PRINTF, lun, '-Event    : Not used => File contains regional MEANS (no frequencies).'
PRINTF, lun, '----------'
PRINTF, lun, 'OUT-FILE(s)'
PRINTF, lun, '-OUTmet   : 1'
PRINTF, lun, '-OUTkeep  : 1 (Keep missing values except for empty regions)'
PRINTF, lun, '=============================================================================='
PRINTF, lun, 'Some explanations::'
PRINTF, lun, '-Unmixing Methods (UMmet):'
PRINTF, lun, '. 0 = Overall regional MEAN (no unmixing)'
PRINTF, lun, '. 1 = Unweighted MEAN of retained pixels'
PRINTF, lun, '. 2 =   Weighted MEAN of retained pixels (C-Indicators)'
PRINTF, lun, '. 3 =   Idem but with thresholds read from file M3Tfil'
PRINTF, lun, '(One optimal threshold for each REGION x CLASS)'
PRINTF, lun, '-Thresholds to withdraw pixels if the AF of considered class is too low:'
PRINTF, lun, '. SELmet=0: No land use info             => No thresholds (always   0%)'
PRINTF, lun, '. SELmet=1: Hard LU-Classification       => No thresholds (always 100%)'
PRINTF, lun, '. SELmet=2: Area Fraction IMGs per class => Variable thresholds (see SPU)'
PRINTF, lun, '-Relative areas RA1, RA2(both in %)'
PRINTF, lun, '. RA1 = 100 * Nr. of retained pixels         / Total pixels in Region'
PRINTF, lun, '. RA2 = 100 * Sum of retained area fractions / Total pixels in Region'
PRINTF, lun, '=============================================================================='
PRINTF, lun, 'File-Layout:'
PRINTF, lun, '-All data in one big table'
PRINTF, lun, '-12 comma-separated values per line (sorted in same way)'
PRINTF, lun, '. Region_ID'
PRINTF, lun, '. Class_ID, UnmixingMethod_ID, Threshold%'
PRINTF, lun, '. Sensor_ID, Var_ID'
PRINTF, lun, '. Periodicity [days], Date'
PRINTF, lun, '. RA1, RA2, MEAN, StDev'
PRINTF, lun, '=============================================================================='
PRINTF, lun, 'DATA:'

END