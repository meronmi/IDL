function isnumeric, input
  on_ioerror, false
  test = double(input)
  return, 1
  false: return, 0
end