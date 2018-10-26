PRO B_main_read_mxd09_a_and_q
path = 'E:\SimMod_data\MODIS_data\JOSH_DATA\250m\8day ref products\';'X:\Active Projects\GEE\pixel extraction'
fnameMODA = 'aabMOD09A1_c5_profiles.csv'
MODA = B_load_MxD09A1_into_structure(path, fnameMODA)
fnameMODQ = 'aaaMOD09Q1_c5_profiles.csv'
MODQ = B_load_MxD09q1_into_structure(path, fnameMODQ)
MODQA = B_matchQA(MODA,MODQ)
PRINT, 'FINISHED'
END