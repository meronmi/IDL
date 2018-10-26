PRO netcdfCopernicus

  fn = 'E:\TEST_COPERNICUS\COP_FAPAR_MISSING_WEBSITE\FAPAR_RT6_201708310000_GLOBE_PROBAV_V2.0.1\c_gls_FAPAR-RT6_201708310000_GLOBE_PROBAV_V2.0.1.nc'
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
    x = ncdfObj -> ReadVariable('FAPAR')
  ENDIF


  Obj_Destroy, ncdfObj
  PRINT, 'Here
END