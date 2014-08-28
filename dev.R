# get metadata all stations
meta <- stat_codes()
 
# get metadata for single station
stat_code('acebbnut')

# get all parameters for number of records back from current date
tmp <- all_params('acebbnut', 100)

