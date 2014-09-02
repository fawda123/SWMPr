# get metadata all stations
meta <- stat_codes()
 
# get metadata for single station
stat_code('acebbnut')

# get all parameters for number of records back from current date
# current max is 100
tmp <- all_params('tjrbrwq', 100)

# get all records within a date range, current max is 1000
tmp <- all_params_dtrng('apaesnut', c('01/01/2013', '08/10/2013'))

# get single parameter for number of records back from current date
tmp <- single_param('delllwq', 100, 'DO_mgl')
