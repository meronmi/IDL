FUNCTION extract_timerange, t_array, t_start, t_end, y_array
;given a time series y_array over the time support t_array
;it extracts the period from t_start to t_end
;if the input starts later than t_start or end before t_end, the output vector is filled with Nan

indFin = WHERE(FINITE(t_array))  
ind = WHERE((t_array[indFin] GE t_start) AND (t_array[indFin] LE t_end), count)
IF (count EQ 0) THEN RETURN, !VALUES.F_NAN

;sind = WHERE(t_array[indFin] GE t_start)
;eind = WHERE(t_array[indFin] LE t_end)
;sind = indFin[sind[0]]
;eind = indFin[eind[N_ELEMENTS(eind)-1]]


RETURN, y_array[indFin[ind]]

;indFin = WHERE(FINITE(t_array))
;indInterval = WHERE((t_array[indFin] GE t_start) AND (t_array[indFin] LE t_end))
;
;RETURN, y_array[indFin[indInterval[0]]:indFin[indInterval[-1]]]
END