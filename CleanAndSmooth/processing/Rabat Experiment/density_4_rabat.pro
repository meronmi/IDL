PRO density_4_rabat_profiles
  fn = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\DOCS\PROFILES stats\export_DB_matlab.csv'
  dirout = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\DOCS'
  tmp =  READ_CSV(fn, HEADER=hdr, MISSING_VALUE=-9999)
  class = tmp.field1
  sensor = tmp.field2
  ndvi = tmp.field4
  ind = WHERE(class EQ 1)
  sensor = sensor[ind]
  ndvi = ndvi[ind]
  ind = WHERE(sensor EQ 1)
  pv = ndvi[ind]
  ind = WHERE(sensor EQ 2)
  vt = ndvi[ind]
  ngrid=35 ;200                 ;resolution of the plot
  frec_max= 75
  scatter_plotDataXY_rabat, vt, pv, 'VGT NDVI', 'PV NDVI', dirout, [0.0,1.0], [0.0,1.0], ngrid, frec_max
  
END

PRO density_4_rabat
fn = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\DOCS\paired_anomal.csv'
dirout = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\DOCS'
tmp =  READ_CSV(fn, HEADER=hdr, MISSING_VALUE=-9999)
pv = tmp.field1
vt = tmp.field2
ngrid=50 ;200                 ;resolution of the plot
frec_max= 50000
scatter_plotDataXY_rabat, vt, pv, 'VGT diff. anomaly', 'PV diff. anomaly', dirout, [-1.0,1.0], [-1.0,1.0], ngrid, frec_max

END