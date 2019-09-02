FUNCTION raphael_crop_select, WINTER = winter, SUMMER = summer
;select the crops to be analysed (Critera: crops (non fodder/grassland), n polygons > 100
IF KEYWORD_SET(winter) THEN RETURN, ['Common wheat - B11', 'Barley - B13', 'Durum wheat - B12', 'Rape and turnip rape - B32', 'Oats - B15']
IF KEYWORD_SET(summer) THEN RETURN, ['Maize - B16',  'Sugar beet - B22', 'Sunflower - B31', 'Dry pulses - B41']
allCrops = ['Common wheat - B11', 'Barley - B13', 'Maize - B16', 'Durum wheat - B12', $
                'Rape and turnip rape - B32', 'Oats - B15', 'Sugar beet - B22', 'Sunflower - B31', 'Dry pulses - B41']
RETURN, allCrops

END