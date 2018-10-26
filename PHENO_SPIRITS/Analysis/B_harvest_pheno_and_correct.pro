PRO B_harvest_pheno_and_correct
;work only on normal
;take the length files of all runs 
;find GSL GE GSL_Threshold an check if they were excluded in the max and range runs. If so, it excluded them.

GSL_Threshold = 15 ;18 until 20 mar 2018

dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion
dir_normal = phenoDirOut.normal
;dir_normal = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\pheno_SOS025'
;dir_normal = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV1\pheno_SOS015'
fn_len_normalRun = dir_normal + '\' + prefix + 'L1.img'
;fn_len_maxRun = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\pheno_SOS025max03\vtL1.img'
;fn_len_rangeRun = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\pheno_SOS025range018\vtL1.img'
dir_max = phenoDirOut.max
fn_len_maxRun = dir_max + '\' + prefix + 'L1.img'
;fn_len_maxRun = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV1\pheno_SOS025max03\vtL1.img'
dir_range = phenoDirOut.range
fn_len_rangeRun = dir_range + '\' + prefix + 'L1.img'
;fn_len_rangeRun = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV1\pheno_SOS025range018\vtL1.img'
;dir_sen = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\pheno_SOS025_sen'
dir_sen = phenoDirOut.sen
;dir_out = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\merged_fixed'
dir_out =  merged_fixed_dir
;dir_out = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV1\merged_fixedV1'

ns = 40320
nl = 14673


lenNormRunMat = BYTARR(ns, nl)
lenMaxRunMat = BYTARR(ns, nl)
lenRangeRunMat = BYTARR(ns, nl)
suppressedMat = BYTARR(ns, nl) * 0B

OPENR, lun, fn_len_normalRun, /GET_LUN
READU, lun, lenNormRunMat
FREE_LUN, lun
OPENR, lun, fn_len_maxRun, /GET_LUN
READU, lun, lenMaxRunMat
FREE_LUN, lun
OPENR, lun, fn_len_rangeRun, /GET_LUN
READU, lun, lenRangeRunMat
FREE_LUN, lun

;find where the length id GE GSL_Threshold
ind = WHERE((lenNormRunMat GE GSL_Threshold) AND (lenNormRunMat LE 36), count)
;for these pixels check if the were excluded in the max and range runs, if so get
;these indexes that are to be escluded from the normal and sen runs
indb = WHERE((lenMaxRunMat[ind] GT 250) AND (lenRangeRunMat[ind] GT 250), countb)
PRINT, '% of exclude (of those having a season length GE '+ STRTRIM(GSL_Threshold,2) +' ) = ' + STRTRIM(countb/float(count) * 100,2)
ind2exclude = ind[indb]
suppressedMat[ind2exclude] = 1B
;now copy the norm and relevant of sen (changing name) into a merged_fixed dir
FILE_MKDIR, dir_out
FILE_COPY, dir_normal + '\*', dir_out, /OVERWRITE
FILE_COPY, dir_sen +  '\' + prefix + 'e1.img', dir_out + '\' + prefix + 'sen1.img', /OVERWRITE
FILE_COPY, dir_sen +  '\' + prefix + 'e1.hdr', dir_out + '\' + prefix + 'sen1.hdr', /OVERWRITE
FILE_COPY, dir_sen +  '\' + prefix + 'e2.img', dir_out + '\' + prefix + 'sen2.img', /OVERWRITE
FILE_COPY, dir_sen +  '\' + prefix + 'e2.hdr', dir_out + '\' + prefix + 'sen2.hdr', /OVERWRITE
fn_list = FILE_SEARCH(dir_out, '*.img')
;make a file  with ones where suprressed

res = write_envi_img(suppressedMat, dir_out + '\suppressed')
res = write_envi_hdr(dir_out + '\suppressed', ns, nl, 1)
FOR i = 0, N_ELEMENTS(fn_list) - 1 DO BEGIN
  ;suppress the pixels
  tmp = ReadEnviWithHdr(fn_list[i])
  tmp[ind2exclude] = 252
  res = write_envi_img(tmp, fn_list[i])
END
END