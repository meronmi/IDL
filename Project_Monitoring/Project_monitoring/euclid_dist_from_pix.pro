FUNCTION euclid_dist_from_pix, mask, ind_trgt_project
;this function takes an image (mask) and a scalar subscript of a target pixel
;ind_trgt_project, result of WHERE) and returns a matrix containing 
;for each pixel, the disance (in pix units) to the target point
sz = SIZE(mask)
IF sz[0] NE 2 THEN STOP
ns = sz[1]
nl = sz[2]

posCR = ARRAY_INDICES(mask, ind_trgt_project)
C_centroid = ROUND(MEAN(FLOAT(posCR[0,*])))
R_centroid = ROUND(MEAN(FLOAT(posCR[1,*])))
Cmat = mask*0
Rmat = Cmat
FOR c=0, nl-1 DO Cmat[*,c] = INDGEN(ns)
FOR c=0, ns-1 DO Rmat[c,*] = INDGEN(nl)
;apply Pitagora

RETURN, SQRT((Cmat-C_centroid)^2+(Rmat-R_centroid)^2)
END