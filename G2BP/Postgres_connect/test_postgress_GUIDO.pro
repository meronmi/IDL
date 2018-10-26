;Postrgres installation
;Databse = postgres
;Server = localhost
;User = postgres
;Pass = solita
;port = 5432


;Prove per fargli vedere Postgres
;Installato driver odbc psqlodbc_09_03_0210-x64.zip da http://www.postgresql.org/ftp/odbc/versions/msi/
;in Control Panel\System and Security\Administrative Tools\odbc data sources 64, ho aggiunto POSTgeSQL Unicode
;Lo stesso per aggiungere un db su un altro server remoto, prima lo devo definire nel ODBC Dta Source Administrator (64-bit)
;come descritto sopra (posso anche usare pgAdmin III per vedere cosa c'è nel db):

;Host: 139.191.142.63
;Port: 5432
;Database: globalrs
;User: viewer
;Password: viewer


;EXAMPLE CODE FOR READING ECMWF

;check that dataminer exists
print, DB_EXISTS()
;% Loaded DLM: DATAMINER.
;1
objDB = OBJ_NEW('IDLdbDatabase')
sources = objDB->GetDatasources()
;help, sources
;SOURCES         STRUCT    = -> DB_DRIVER_INFO Array[2]
print, sources
;{ Excel Files Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)}{ MS Access Database Microsoft Access Driver (*.mdb, *.accdb)}{ PostgreSQL35W PostgreSQL Unicode(x64)}
;print, sources[2]
mysource = sources[4].datasource
;{ PostgreSQL35W PostgreSQL Unicode(x64)}
;status = DIALOG_DBCONNECT(objDB)  ;per connessione con dialog, sotto per connessione diretta
objDB->CONNECT, DATASOURCE = mysource
tables = objDB->GetTables()
;exmaple find the table 'tendaily_mother' and explore it
;ind = WHERE(tables.name EQ 'tendaily_mother', count)
;print, tables.name[ind]

;example queries
;query = "SELECT year,longitude,latitude,rad FROM meteodatafine.tendaily_mother WHERE year = 2013 AND month = 10 AND dekad = 2 AND longitude >= -10 AND longitude <= 10 AND latitude >= -10 AND latitude <= 10"
;query for getting a profile (extremely slow!!!)
;SELECT year,month,dekad,longitude,latitude,rad
;FROM meteodatafine.tendaily_mother
;WHERE longitude = 10
;AND latitude =  10
;AND year >=  1999
;AND year <=  2014
;order by year,month,dekad
STARTTIME = SYSTIME(1)
query = "select year, dekad, st_nearestvalue(rast, 1, ST_SetSRID(ST_MakePoint(38.07, 9.81),4326)) from spotvgt_fapar svf, dekads where st_intersects(rast, ST_SetSRID(ST_MakePoint(38.07, 9.81),4326)) and svf.filename = dekads.filename order by year, dekad "
res = queryDb(objDB, query)
ELAPSED_TIME = SYSTIME(1) - STARTTIME
PRINT, 'QUERY PROCESSING TOOK seconds: ' + STRTRIM(ELAPSED_TIME,2)
;close
OBJ_DESTROY, objDB

END

