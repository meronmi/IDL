FUNCTION C_start_up
dlmtr = '\'
disk_letter = 'D' ;'E'
;Common
stack_path = disk_letter + ':\TEST_COPERNICUS\STACKS'
out_path = disk_letter + ':\TEST_COPERNICUS\OUTPUTS'

;all thinned, [Reference, Candidate 1, Candidate 2]
ts_name = ['BOKU NDVI', 'Cop_FAPAR_GEOV2','Cop_NDVI3s']
ts_name_short = ['bk_ND', 'c_FAP','c_ND3s']  
in_path  = disk_letter + [':\TEST_COPERNICUS\BOKU_NDVI_v1\THINNED', $
                          ':\TEST_COPERNICUS\GEOV2_FAPAR\GEOV2_FAPAR_THINNED', $
                          ':\TEST_COPERNICUS\NDVIv30beta\ENVI_format_resized_thinned_smoothed']
meta_fn =  in_path + dlmtr + ['AAA_meta_bk2003-2018NDVI.mta', $
                           'AAA_meta_c2003-2018FAPAR.mta', $
                           'AAA_meta_c2003-2018NDVI3s.MTA']
bil_fn =  stack_path + dlmtr + ['bk2003-2018NDVI.bil', $
                          'c2003-2018FAPAR.bil', $
                          'c2003-2018NDVI3s.bil']
z_fn = stack_path +  dlmtr + ['bk2003-2018zNDVI_bsqZSCORE.img', $
                        'c2003-2018FAPAR_bsqZSCORE.img', $
                        'c2003-2018zNDVI3s_bsqZSCORE.img']
;scaling issues ref c1 c2 , bk fapar ndvi3
minval = [0, 0, 0]    
maxval = [250, 200, 250]
gain = [0.0048, 0.005, 0.004]
offset = [-0.2, 0, -0.08]                    


startup = CREATE_STRUCT('ts_name', ts_name, 'ts_name_short', ts_name_short, 'in_path', in_path, $
                        'meta_fn', meta_fn, 'bil_fn', bil_fn, 'z_fn', z_fn, 'stack_path', stack_path, 'out_path', out_path, $
                        'minval',minval, 'maxval',maxval, 'gain',gain, 'offset',offset)
RETURN, startup
END