FUNCTION par_settings_GRID
@cb_parallel.comm
;number of segments in which to split the job. It should be slighly GT the number of processors if some segments are desertic 
n = '100'                                            ;MatlabServer2 has 12 processors                                
;root of remote directory for the decomposition output
;out_path = 'M:\sahel';'M:\HoAandUganda'                           ;MatlabServer 2 disk mapped as M:\
out_path = 'Y:\projects\grid\io\FOR_UNIX';''Z:\michele';'M:\HoAandUganda'                           ;MatlabServer 2 disk mapped as M:\
rem_out_path = 'X:\michele';'E:\Foodsec\michele';'V:\Michele\HoAandUganda'       ;Same directory but seen from the remote machine                   
;IDL idlrt directory on remote computer
;rem_idl_path='C:\Program Files\ITT\IDL\IDL81\bin\bin.x86_64';'C:\Program Files\ITT\IDL\IDL80\bin\bin.x86_64'
rem_idl_path='C:\Program Files\Exelis\IDL84\bin\bin.x86_64';'C:\Program Files\Exelis\IDL85\bin\bin.x86_64';'C:\Program Files\Exelis\IDL84\bin\bin.x86_64';'C:\Program Files\ITT\IDL\IDL80\bin\bin.x86_64'
;rem_idl_path='C:\Program Files\Exelis\IDL82\bin\bin.x86_64
;Batch file name
bat_fn = 'decomposer.bat'

;name and path of sav file to be use for running the code on the remote machine
sav_fn = 'phenot_command_line.sav'
sav_path = 'd:\Users\meronmi\Documents\IDL\Pheno365\Phenot'
;##############################################################################################
RETURN, 0
END