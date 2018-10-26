FUNCTION uniqlist, array
;Purpose:
;     To form a list of unique array values from an array
;     print, uniqlist([2,3,2,2,4,1])
;     >1 2 3 4

mlist = array(SORT(array)) 
mlist = mlist[UNIQ(mlist)] 
mlist = mlist(SORT(mlist))

RETURN, mlist
END