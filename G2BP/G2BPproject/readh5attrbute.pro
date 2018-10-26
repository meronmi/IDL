FUNCTION readH5attrbute, id, attr_name

attrib_id = H5A_OPEN_NAME(id, attr_name)
attr_val = H5A_READ(attrib_id)
H5A_CLOSE, attrib_id
RETURN, attr_val
END