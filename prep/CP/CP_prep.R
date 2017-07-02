dir_layers <- file.path('~/github/mhi/region2017/layers')


#naming convention of the data file: it is "goalcode_layername_assessmentYEAR.csv".
data_file  <- file.path(dir_layers, 'cp_hab_extent_mhi2017.csv')
cp_extent <- readr::read_csv(data_file)

habitat.rank <- c('reef'            = 4,
                  'wetland'        = 3,
                  'beach' = 4)

cp_extent <- cp_extent %>%
  filter(habitat %in% names(habitat.rank)) %>%
  mutate(
    rank = habitat.rank[habitat],
    extent_rank = rank*km2) %>%
  select(rgn_id, habitat, extent_rank)

readr::write_csv(cp_extent, file.path(dir_layers, "element_wts_cp_km2_x_protection_mhi2017.csv"))



#CP prep
data_file  <- file.path(dir_layers, 'cp_hab_extent_mhi2017.csv')
cs_extent <- readr::read_csv(data_file)

habitat.rank <- c('wetland' = 210)

cs_extent <- cs_extent %>%
  filter(habitat %in% names(habitat.rank)) %>%
  mutate(
    rank = habitat.rank[habitat],
    extent_rank = rank*km2) %>%
  select(rgn_id, habitat, extent_rank)



readr::write_csv(cs_extent, file.path(dir_layers, "element_wts_cs_km2_x_storage_mhi2017.csv"))
