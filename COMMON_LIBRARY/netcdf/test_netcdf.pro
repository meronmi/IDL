PRO test_netcdf

fn = '\\ies\d5\asap\BEC_soil_moisture\data\D\BEC_BIN_SM_D_20100113T001244_20100114T002702_NOMINAL_025_002.nc'
;fn = '\\ies\d5\asap\users_data\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\netcdfFromEcoCast\'
ncdfObj = Obj_New('NCDF_DATA', fn, /NO_READ_ON_PARSE)
; METHODS:
;
;     The following methods can be used directly.
;
;     ncdfObject -> Browse                             ; Use GUI to browse file data and metadata.
;     ncdfObject -> OpenFile, filename                 ; Opens a new netCDF or HDF file.
;     globalAttr = ncdfObject -> ReadGlobalAttr()      ; Return a structure containing global attributes.
;     attribute = ncdfObject -> ReadAttribute(attrname); Return an attribute, identified by name.
;     dim = ncdfObject -> ReadDimension(dimName)        ; Return a dimension, identified by name.
;     variable = ncdfObject -> ReadVariable(varname)   ; Return a variable, identified by name.
;     varstruct = ncdfObject -> ReadVariableWithAttr(varname)   ; Return a variable, identified by
;                                                               ; name, along with its attributes.
;     allData = ncdfObject -> ReadFile(filename)        ; Read all the data in the file, into structures.
IF Obj_Valid(ncdfObj) THEN BEGIN
  ncdfObj -> Browse, NO_NEW_FILE=Keyword_Set(no_new_file), $
    XOFFSET=xoffset, YOFFSET=yoffset, SUCCESS=success, TITLE=title
  GlobAttr = ncdfObj-> ReadGlobalAttr()
  PRINT, GlobAttr
  x = ncdfObj -> ReadVariable('SM')
  lon = ncdfObj -> ReadVariable('lon')
ENDIF

  
Obj_Destroy, ncdfObj
PRINT, 'Here
END