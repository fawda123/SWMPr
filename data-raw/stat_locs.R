
stat_locs <- readr::read_csv('data-raw/sampling_stations.csv') |> 
  dplyr::filter(Status == 'Active') |> 
  dplyr::select(
    station_code = `Station Code`, 
    latitude = Latitude,
    longitude = Longitude,
    gmt_off = `GMT Offset`
  ) |> 
  dplyr::mutate(
    station_code = substr(station_code, 1, 5), 
    latitude = as.numeric(latitude), 
    longitude = -1 * as.numeric(longitude)
  ) |> 
  dplyr::distinct()

usethis::use_data(stat_locs, overwrite = TRUE)
