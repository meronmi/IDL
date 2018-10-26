FUNCTION duplicateElementsInArray, data
sorteddata = data[Sort(data)]
dataenum = sorteddata[Uniq(sorteddata)]
mappeddata = Value_Locate(dataenum, data)
h = histogram(mappeddata, min=0)
ind = WHERE(h GT 1, count)
IF (count GT 0) THEN RETURN, dataenum[Where(h gt 1)] ELSE RETURN, -1
END