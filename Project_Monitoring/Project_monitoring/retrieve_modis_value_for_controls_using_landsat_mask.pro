FUNCTION retrieve_Landsat_controls_coordinates_in_modis_samples_and_lines, $
modis_dir, modis_template, landsat_controls_dir, landsat_controls_fn, target_id, n_controls, out_dir

; target_id *1000 + 10, 12,..20 are 10 controls

compile_opt idl2


;first reproject an then make polygons out of the ls_ctrls
;e = ENVI(/HEADLESS)
e = ENVI()


ls_ctrls = e.OpenRaster(landsat_controls_dir + '\' + landsat_controls_fn)
CoordSys = ENVICoordSys(COORD_SYS_CODE=4326)
reprj_ls_ctrls = ENVIReprojectRaster(ls_ctrls, COORD_SYS=CoordSys, RESAMPLING='Nearest Neighbor')
rep_fn = out_dir + '\' + FILE_BASENAME(landsat_controls_fn, '.img')+'_LATLON.img'
res = FILE_SEARCH(landsat_controls_dir, FILE_BASENAME(landsat_controls_fn, '.img') + '_LATLON.*')
IF (res[0] NE '') THEN FILE_DELETE, res
reprj_ls_ctrls.Export, rep_fn, 'ENVI'
ls_ctrls.Close
;data = reprj_ls_ctrls.GetData()
;newRaster = ENVIRaster(data, $
;  URI=landsat_controls_dir + '\' + FILE_BASENAME(landsat_controls_fn, '.img')+'_LATLON.img')
;reprj_ls_ctrls.Save

view = e.GetView()
layer = view.CreateLayer(reprj_ls_ctrls)
roi_obj_array = OBJARR(n_controls+1)
; Define a new ROI with a threshold and display it
c = 0
;define controls
FOR i = target_id*1000+10+1, target_id*1000+10+n_controls DO BEGIN
  ;roi = EnviRoi(NAME = 'Threshold', COLOR='Blue')
  roi_obj_array[c] = EnviRoi(NAME='C'+STRTRIM(i,2), COLOR='Red')
  ;rois = [rois, EnviRoi(NAME='Control_'+STRTRIM(i,2), COLOR='Red')]
  ;roi_obj_array[c].AddThreshold, ls_ctrls, 0, MIN_VALUE=i, MAX_VALUE=i
  roi_obj_array[c].AddThreshold, reprj_ls_ctrls, 0, MIN_VALUE=i, MAX_VALUE=i
  roiLayer = layer.AddRoi(roi_obj_array[c])
  c = c +1 
ENDFOR
;define project
roi_obj_array[c] = EnviRoi(NAME='Project'+STRTRIM(target_id,2), COLOR='Green')
;roi_obj_array[c].AddThreshold, ls_ctrls, 0, MIN_VALUE=target_id, MAX_VALUE=target_id
roi_obj_array[c].AddThreshold, reprj_ls_ctrls, 0, MIN_VALUE=target_id, MAX_VALUE=target_id
roiLayer = layer.AddRoi(roi_obj_array[c])

fn_vector = out_dir + '\' + FILE_BASENAME(landsat_controls_fn, '.img')+'_LATLON.shp'
reprj_ls_ctrls.ExportRois, fn_vector, roi_obj_array, 'SHAPEFILE'

;then use this polygons to make rois on MODIS
;use the rois to get pixel values
modis =  e.OpenRaster(modis_dir + '\' + modis_template)
view = e.GetView()
layer = view.CreateLayer(modis)
;open the vector and transform it into a roi
vector = e.OpenVector(fn_vector)
;view = e.GetView()
layer2 = view.CreateLayer(vector)
Task = ENVITask('VectorAttributeToROIs')
Task.ATTRIBUTE_NAME = 'CLASS_NAME'
Task.INPUT_VECTOR = Vector
res = FILE_SEARCH(out_dir, 'roi_modis.xml')
IF (res[0] NE '') THEN FILE_DELETE, res
Task.OUTPUT_ROI_URI = out_dir + '\roi_modis.xml'
Task.Execute
VisRois = !NULL
first = 1
Foreach Roi, Task.OUTPUT_ROI do BEGIN
  VisRois = [VisRois, layer.AddRoi(Roi)]
  ;then use   ENVIROI::PixelAddresses to get where they are and extract the values
  ;PRINT, 'addresses = '
  IF first THEN BEGIN
    myList = LIST(roi.PixelAddresses(modis))
    first = 0
  ENDIF ELSE myList.Add, roi.PixelAddresses(modis)
END
;e.Close
RETURN, myList

END

PRO retrieve_modis_value_for_controls_using_landsat_mask
;this procedure use the landsat ls_ctrls of the controls to extract from a modis image
modis_dir = 'S:\Actions\FOODSEC\projects\GGW_monitoring\AutomatedReferenceSiteSelection_eMODIS\eMODIS vs Landsat'
modis_template =  'wa1527i.img'
modis_before_list = ['wa0726i.img']
modis_after_list =['wa1527i.img']
landsat_controls_dir = 'S:\Actions\FOODSEC\projects\GGW_monitoring\AutomatedReferenceSiteSelection_eMODIS\eMODIS vs Landsat'
target_id = 18
n_controls = 10
landsat_controls_fn = 'control_mask_project_18.img'
out_dir = 'S:\Actions\FOODSEC\projects\GGW_monitoring\AutomatedReferenceSiteSelection_eMODIS\eMODIS vs Landsat\OUTDIR'
;END OF USER PART

;get the list
roipos = retrieve_Landsat_controls_coordinates_in_modis_samples_and_lines($
  modis_dir, modis_template, landsat_controls_dir, landsat_controls_fn, target_id, n_controls, out_dir)
nrois =  roipos.Count() 
dlmtr = ','
OPENW, lun, out_dir + '\ndvi_control_and_project' + STRTRIM(target_id,2)+'_ANNE_FORMAT.csv', /GET_LUN
PRINTF, lun, 'Year'+dlmtr+'SiteClass'+dlmtr+'Site'+dlmtr+'Period'+dlmtr+'Pixel'+dlmtr+'RS-var'
e = ENVI()
;loop on rois (be care, the last is the project)
FOR r = 0, nrois-1 DO BEGIN
  ;loop on images before
  n_before = N_ELEMENTS(modis_before_list)
  n_after = N_ELEMENTS(modis_after_list)
  image_list = [modis_before_list, modis_after_list]
  FOR b= 0, N_ELEMENTS(image_list)-1 DO BEGIN
    IF (b LE n_before-1) THEN period = 'before' ELSE period = 'after'
    ;open the file and get the data
    raster = e.OpenRaster(modis_dir + '\' + image_list[b])
    data = raster.GetData()
    ;loop on pixels
    roi = roipos[r]
    FOR p = 0, N_ELEMENTS(roi[0,*])-1 DO BEGIN
      val = data[roi[0,p], roi[1,p]]
      date = read_info('date', modis_dir + '\' + remove_ext_from_fn(image_list[b]) + '.hdr')
      IF (r EQ nrois-1) THEN BEGIN
        PRINTF, lun, STRTRIM(date,2) + dlmtr +$
                     STRTRIM('project',2) + dlmtr +$
                     'C'+STRTRIM(r+1,2) + dlmtr +$
                     STRTRIM(period,2) + dlmtr +$
                     'P'+STRTRIM(p+1,2) + dlmtr +$
                     STRTRIM(FIX(val),2) + dlmtr
      ENDIF ELSE BEGIN
        PRINTF, lun, STRTRIM(date,2) + dlmtr +$
                     STRTRIM('control',2) + dlmtr +$
                     'C'+STRTRIM(r+1,2) + dlmtr +$
                     STRTRIM(period,2) + dlmtr +$
                     'P'+STRTRIM(p+1,2) + dlmtr +$
                     STRTRIM(FIX(val),2)+ dlmtr 
      ENDELSE
    ENDFOR ;p
  ENDFOR ;b
ENDFOR; r

FREE_LUN, lun
PRINT, 'Finished'
END