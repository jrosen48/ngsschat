# geocode-locs

# usethis::edit_r_environ()
# add API key for Google Maps geocoding API: https://developers.google.com/maps/documentation/geocoding/overview
# to MAPS_API_KEY <- '<your-key-here>'

geocode_locs <- function(users) {
  # users$geocoded_location <- mapsapi::mp_geocode(users$location, key = MAPS_API_KEY)
  # write_rds(users, 'geocoded-locations.rds')
  read_rds("data/geocoded-locations.rds")
}

