#' This function produces annual amounts of water use and profits.
#' @param subsidy_amount                 is the amount of subsidy per acre-inch of groundwater extracted. Defaults to 1.
#' @param subsidy_threshold              is the threshold of subsidy, i.e., the amount of groundwater extraction above which subsidy is zero. Defaults to 400.
#' @param well_soil_file                 is the file that contains the soil types for each well in the region. Defaults to "C:/Users/manirad/Dropbox/DSSAT subregions work pre-2018 annual meeting/subregion KS files/outputs_for_econ/Well_Soil Type.csv".
#' @param well_capacity_files            is the directory where well capacity files are located. Defaults to "C:/Users/manirad/Downloads/test/Well Capacity".
#' @param econ_output_folder             is the name of the folder that contains irrigated acres, irrigation, and profits for each well. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/KS_DSSAT_output.csv",
#' @param well_capacity_file_year        is the name of the output file that contains irrigation for each well which will be used by MODFLOW. Defaults to "C:/Users/manirad/Downloads/test/KS_DSSAT_output.csv",
#' @param subregion_file                 is the subset of wells that the subsidy is applied to. The rest of the wells adjust their water use only due to changes in aquifer levels.
#' @param look_up_table_inside           is the lookup table for wells that are inside  the selected policy area or are   affected by the policy.
#' @param look_up_table_outside          is the lookup table for wells that are outside the selected policy area or are unaffected by the policy.
#' @param first_year_of_simulation       is the first year that the hydro-economic simulation starts. Defaults to 2000.
#' @param default_well_capacity_col_name is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.
#' @param missing_soil_types             is the soil type that is assigned to missing soil types for wells. Defaults to "KS00000007".
#' @param minimum_well_capacity          is the minimum well capacity in the model. If well capacity falls below this capacity, it is set to this minimum. Defaults to 100  gallons per minute.
#' @param maximum_well_capacity          is the maximum well capacity in the model. If well capacity falls above this capacity, it is set to this maximum. Defaults to 3000 gallons per minute.
#' @param first_year_of_GW               is the first year that the GW model exists. This may be different than the first year of simulation. Defaults to 1997.
#' @param last_year_of_GW                is the last  year that the GW model exists. This may be different than the last  year of simulation. Defaults to 2007.
#' @param irrigation_season_days         Number of days in an irrigation season. Defaults to 70.
#' @return                               returns the output table.
#' @examples
#' \dontrun{
#' gen_lookup_subsidy(subsidy_amount = 2)
#' }
#' @export
annual_model_subsidy_subregion_annual = function(subsidy_amount = 21,
                                          subsidy_threshold = 1500,
                                          well_soil_file = "./input_files/Well_Soil Type.csv",
                                          well_capacity_files = "./Well Capacity",
                                          econ_output_folder = "./Econ_output/results_with_subsidy/annual_results/",
                                          well_capacity_file_year = "./KS_DSSAT_output.csv",
                                          subregion_file = "./input_files/all_well_IDs_small.csv",
                                          look_up_table_inside  = "lookup_table_all_years_2.rds",
                                          look_up_table_outside = "lookup_table_all_years_2_0.rds",
                                          first_year_of_simulation = 2000,
                                          default_well_capacity_col_name = "Well_Capacity(gpm)",
                                          missing_soil_types = "KS00000007",
                                          minimum_well_capacity = 0,
                                          maximum_well_capacity = 1000,
                                          first_year_of_GW = 1997,
                                          last_year_of_GW = 2007,
                                          irrigation_season_days = 70)
{
  subsidy_amount = (subsidy_amount - 1)/10
  soil_type = fread(well_soil_file)
  soil_type[, `:=`(Soil_Type, gsub("KSFC00000", "KS0000000",
                                   Soil_Type))]
  soil_type = soil_type[complete.cases(Well_ID)]
  filenames = list.files(path = well_capacity_files, pattern = "*.csv",
                         full.names = TRUE)
  ldf <- lapply(filenames, fread, fill = T)
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  year_dt = rbindlist(ldf)
  foo = data.table::data.table(Well_ID = 1, V1 = 1, file_name = paste0(well_capacity_files,
                                                                       "/", first_year_of_simulation, "_Capacity.csv"))
  setnames(foo, old = "V1", new = default_well_capacity_col_name)
  year_dt = rbind(year_dt, foo)
  year_dt[, `:=`(file_name, substr(file_name, nchar(file_name) -
                                     16, nchar(file_name) - 13))]
  year_dt[, `:=`(file_name, as.integer(file_name))]
  setkey(year_dt, file_name)
  year_dt = year_dt[nrow(year_dt)]
  year_dt = year_dt$file_name
  year_2  = year_dt
  well_capacity = data.table(Well_ID = NA, V1 = NA)
  setnames(well_capacity, old = "V1", new = default_well_capacity_col_name)
  well_capacity = ifelse(year_dt == first_year_of_simulation,
                         list(rbind(well_capacity, fread(paste0(first_year_of_simulation,
                                                                "_Well_Capacity.csv")))), list(rbind(well_capacity,
                                                                                                     fread(paste0("./Well Capacity/", year_dt, "_Capacity.csv")))))
  well_capacity = data.table(well_capacity[[1]])
  well_capacity = well_capacity[complete.cases(Well_ID)]
  setkey(soil_type, Well_ID)
  setkey(well_capacity, Well_ID)
  well_capacity_data = soil_type[well_capacity]
  setnames(well_capacity_data, default_well_capacity_col_name,
           "Well_capacity")
  well_capacity_data[, `:=`(Well_capacity, mean(Well_capacity)),
                     by = "Well_ID"]
  well_capacity_data = unique(well_capacity_data, by = "Well_ID")
  setkey(well_capacity_data, Well_ID)
  well_capacity_data[is.na(Soil_Type), `:=`(Soil_Type, missing_soil_types)]
  well_capacity_data = well_capacity_data[!is.na(Soil_Type)]
  well_capacity_data[, `:=`(Well_capacity, round(Well_capacity))]
  soil_type = fread(well_soil_file)
  soil_type[, `:=`(Soil_Type, gsub("KSFC00000", "KS0000000",
                                   Soil_Type))]
  soil_type = soil_type[complete.cases(Well_ID)]
  soil_type = unique(soil_type, by = "Well_ID")
  well_capacity_data[, Well_capacity := ifelse(Well_capacity <= minimum_well_capacity, minimum_well_capacity, Well_capacity)]
  well_capacity_data[, Well_capacity := ifelse(Well_capacity >= maximum_well_capacity, maximum_well_capacity, Well_capacity)]
  lookup_table_all_years_2   = readRDS(look_up_table_inside)
  lookup_table_all_years_2_0 = readRDS(look_up_table_outside)

  filenames = list.files(path = well_capacity_files, pattern = "*.csv",
                         full.names = TRUE)
  ldf <- lapply(filenames, fread, fill = T)
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  year_dt = rbindlist(ldf)
  foo = data.table(Well_ID = 1, V1 = 1, file_name = paste0(well_capacity_files,
                                                           "/", first_year_of_simulation, "_Capacity.csv"))
  setnames(foo, old = "V1", new = default_well_capacity_col_name)
  year_dt = rbind(year_dt, foo)
  year_dt[, `:=`(file_name, substr(file_name, nchar(file_name) -
                                     16, nchar(file_name) - 13))]
  year_dt[, `:=`(file_name, as.integer(file_name))]
  setkey(year_dt, file_name)
  year_dt = year_dt[nrow(year_dt)]
  year_dt[, `:=`(file_name, ifelse(file_name <= (last_year_of_GW-1), file_name +
                                     1, ifelse(file_name > (last_year_of_GW-1) & file_name <= (last_year_of_GW-1 + last_year_of_GW - first_year_of_GW+1), file_name -
                                                 (-1 + last_year_of_GW - first_year_of_GW+1), ifelse(file_name > (last_year_of_GW-1 + last_year_of_GW - first_year_of_GW+1) & file_name <= (last_year_of_GW-1 + 2*(last_year_of_GW - first_year_of_GW+1)), file_name -
                                                                                                       (-1 + 2*(last_year_of_GW - first_year_of_GW+1)), ifelse(file_name > (last_year_of_GW-1 + 2*(last_year_of_GW - first_year_of_GW+1)) & file_name <= (last_year_of_GW-1 + 3*(last_year_of_GW - first_year_of_GW+1)), file_name -
                                                                                                                                                                 (-1 + 3*(last_year_of_GW - first_year_of_GW+1)), file_name - (-1 + 4*(last_year_of_GW - first_year_of_GW+1)))))))]
  year_dt = year_dt$file_name
  lookup_table_all_years_2   = lookup_table_all_years_2[SDAT ==
                                                          year_dt]
  lookup_table_all_years_2_0 = lookup_table_all_years_2_0[SDAT ==
                                                            year_dt]

  setkey(lookup_table_all_years_2,   SOIL_ID, Well_capacity)
  setkey(lookup_table_all_years_2_0, SOIL_ID, Well_capacity)
  setkey(well_capacity_data, Soil_Type, Well_capacity)
  lookup_table_all_years_2   = lookup_table_all_years_2[well_capacity_data]
  lookup_table_all_years_2_0 = lookup_table_all_years_2_0[well_capacity_data]

  wells_subregion = fread(subregion_file)
  wells_subregion[, id := 1]
  setkey(lookup_table_all_years_2, Well_ID)
  setkey(lookup_table_all_years_2_0, Well_ID)
  setkey(wells_subregion, V1)
  lookup_table_all_years_2 = lookup_table_all_years_2[wells_subregion]
  lookup_table_all_years_2_0 = wells_subregion[lookup_table_all_years_2_0]
  lookup_table_all_years_2_0 = lookup_table_all_years_2_0[is.na(id)]

  lookup_table_all_years_2   = rbind(lookup_table_all_years_2[, .(Well_ID, Well_capacity, tot_acres, irr_tot_acres, irr_below, profit_Well_ID, profit_Well_ID_sub)],
                                     lookup_table_all_years_2_0[, .(Well_ID = V1, Well_capacity, tot_acres, irr_tot_acres, irr_below, profit_Well_ID, profit_Well_ID_sub)])
  lookup_table_all_years_2[, `:=`(output_rate_acin_day, irr_tot_acres/irrigation_season_days)]

  setkey(lookup_table_all_years_2, Well_ID)
  econ_output = lookup_table_all_years_2[, .(Well_ID, Well_capacity,
                                             tot_acres, irr_tot_acres, irr_below, profit_Well_ID, profit_Well_ID_sub, output_rate_acin_day)]

  econ_output[, `:=`(row, 1:.N)]
  econ_output[, `:=`(subsidy_amnt, subsidy_amount)]
  econ_output[, `:=`(subsidy_thshld, subsidy_threshold)]
  econ_output[, year := year_2]
  # econ_output_in = fread("./Econ_output/KS_DSSAT_output.csv")
  # econ_output = rbind(econ_output_in, econ_output)
  econ_output[is.na(output_rate_acin_day), `:=`(output_rate_acin_day,
                                                0)]
  well_capacity_data = lookup_table_all_years_2[, .(Well_ID,
                                                    output_rate_acin_day)]
  write.csv(econ_output, paste0(econ_output_folder, "Econ_output_", subsidy_amount, "_", subsidy_threshold, "_", year_2, ".csv"), row.names = FALSE)
  write.csv(well_capacity_data, well_capacity_file_year, row.names = FALSE)
}

