FUNCTION get_date_tif_from_GEE, dir, fn
;return the date of acquisition of a downloaded csv with properties of GEE tif file
mtdata = READ_CSV(dir + '\' + FILE_BASENAME(fn, '.tif') + '.csv', HEADER=hdr)
RETURN, mtdata.(WHERE(hdr EQ 'DATE_ACQUIRED'))
END