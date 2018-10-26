PRO load__spirits_db
inpath = 'E:\ILRI\SP_PROJECTS\SP_Shape_Oct2014\SpiritsProjectData'
workpath = 'd:\Users\meronmi\Documents\IDL\ALRMP kenya\workdir' 
fn = 'SpiritsDB.csv'
tmp =  READ_CSV(inpath + '\' + fn, HEADER=hdr, MISSING_VALUE=-99999.999)
PRINT, hdr
data = rename_tags(tmp, TAG_NAMES(tmp),  hdr)
help, data, /STRUCT
SAVE, data, FILENAME = workpath + '\' + 'spiritsDB.sav'
END


