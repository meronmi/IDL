FUNCTION load_envi_bsq_file, image_filename
;open and return the data of an envi file (the filename of the image is provided)
;the file must be with no extention of img extension and must have a hdr file
dir = FILE_DIRNAME(image_filename)
img_fn = FILE_BASENAME(image_filename)
hdr_fn = FILE_BASENAME(image_filename, '.img') +'.hdr'
ns = read_info('samples', dir + '\' + hdr_fn)
nl = read_info('lines', dir + '\' + hdr_fn)
nb = read_info('bands', dir + '\' + hdr_fn)
dt = read_info('data type', dir + '\' + hdr_fn)
data = MAKE_ARRAY([ns,nl,nb], TYPE=dt, /NOZERO)
OPENR, lun, image_filename, /GET_LUN
READU, lun, data
FREE_LUN, lun
RETURN, data
END