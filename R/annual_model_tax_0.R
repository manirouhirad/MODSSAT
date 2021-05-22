#' This function produces annual amounts of water use and profits.
#' @param tax_amount                     is the amount of tax on the unit of groundwater extracted. Defaults to 1.1.
#' @param well_soil_file                 is the file that contains the soil types for each well in the region. Defaults to "C:/Users/manirad/Dropbox/DSSAT subregions work pre-2018 annual meeting/subregion KS files/outputs_for_econ/Well_Soil Type.csv".
#' @param well_capacity_files            is the directory where well capacity files are located. Defaults to "C:/Users/manirad/Downloads/test/Well Capacity".
#' @param econ_output_file               is the name of the output file that contains irrigated acres, irrigation, and profits for each well. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/KS_DSSAT_output.csv",
#' @param well_capacity_file_year        is the name of the output file that contains irrigation for each well which will be used by MODFLOW. Defaults to "C:/Users/manirad/Downloads/test/KS_DSSAT_output.csv",
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
#' gen_lookup_tax(tax_amount = 2)
#' }
#' @export
annual_model_tax = function (tax_amount = 1,
                             well_soil_file = "./input_files/Well_Soil Type.csv",
                             well_capacity_files = "./Well Capacity",
                             econ_output_file = "./Econ_output/KS_DSSAT_output.csv",
                             well_capacity_file_year = "./KS_DSSAT_output.csv",
                             first_year_of_simulation = 2000,
                             default_well_capacity_col_name = "Well_Capacity(gpm)",
                             missing_soil_types = "KS00000007",
                             minimum_well_capacity = 100,
                             maximum_well_capacity = 3000,
                             first_year_of_GW = 1997,
                             last_year_of_GW = 2008,
                             irrigation_season_days = 70)
{
  tax_amount = (tax_amount - 1)/10
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
  well_capacity_data[Well_capacity <= minimum_well_capacity,
                     `:=`(Well_capacity, minimum_well_capacity)]
  well_capacity_data[Well_capacity >= maximum_well_capacity,
                     `:=`(Well_capacity, maximum_well_capacity)]

  lookup_table_well_2 = fread("lookup_table_well_2.csv")
  lookup_table_all_years_2 = readRDS("lookup_table_all_years_2.rds")

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
  # year_dt[, `:=`(file_name, ifelse(file_name < last_year_of_GW +
  #                                    1, file_name, ifelse(file_name > last_year_of_GW & file_name <=
  #                                                           last_year_of_GW + (last_year_of_GW - first_year_of_GW +
  #                                                                                1), file_name - (last_year_of_GW - first_year_of_GW +
  #                                                                                                   1), ifelse(file_name > last_year_of_GW + (last_year_of_GW -
  #                                                                                                                                               first_year_of_GW + 1) & file_name <= last_year_of_GW +
  #                                                                                                                2 * (last_year_of_GW - first_year_of_GW + 1), file_name -
  #                                                                                                                2 * (last_year_of_GW - first_year_of_GW), ifelse(file_name >
  #                                                                                                                                                                   last_year_of_GW + 2 * (last_year_of_GW - first_year_of_GW +
  #                                                                                                                                                                                            1) & file_name < last_year_of_GW + 3 * (last_year_of_GW -
  #                                                                                                                                                                                                                                      first_year_of_GW + 1), file_name - 3 * (last_year_of_GW -
  #                                                                                                                                                                                                                                                                                first_year_of_GW), file_name - 4 * (last_year_of_GW -
  #                                                                                                                                                                                                                                                                                                                      first_year_of_GW))))))]

  # year_dt[, `:=`(file_name, ifelse(file_name < 2010, file_name, ifelse(file_name > 2009 & file_name <= 2022, file_name - 13,
  #                                                                      ifelse(file_name > 2022 & file_name <= 2035, file_name - 26,
  #                                                                             ifelse(file_name > 2035 & file_name < 2049, file_name - 39, file_name - 52)))))]

  year_dt[, `:=`(file_name, ifelse(file_name <= 2006, file_name+1,
                                   ifelse(file_name > 2006 & file_name <= 2017, file_name - 10, ifelse(file_name > 2017 & file_name <= 2028,
                                                                                                       file_name - 21, ifelse(file_name > 2028 & file_name <= 2039, file_name - 32, file_name - 43)))))]

  year_dt = year_dt$file_name
  lookup_table_all_years_2 = lookup_table_all_years_2[SDAT == year_dt]
  lookup_table_all_years_2[, `:=`(Well_ID, NULL)]
  setkey(lookup_table_all_years_2, SOIL_ID, Well_capacity)
  setkey(lookup_table_well_2, SOIL_ID, Well_capacity)
  setkey(well_capacity_data, Soil_Type, Well_capacity)
  lookup_table_all_years_2 = lookup_table_all_years_2[well_capacity_data]
  lookup_table_well_2 = lookup_table_well_2[well_capacity_data]
  lookup_table_all_years_2[, `:=`(irr_tot_acres, sum(irrigation)),
                           by = "Well_ID"]
  lookup_table_all_years_2[, `:=`(profit_Well_ID, sum(profit)),
                           by = "Well_ID"]
  lookup_table_all_years_2 = unique(lookup_table_all_years_2,
                                    by = "Well_ID")
  lookup_table_all_years_2[, `:=`(output_rate_acin_day, irr_tot_acres/irrigation_season_days)]
  econ_output = lookup_table_all_years_2[, .(Well_ID, Well_capacity,
                                             tot_acres, irr_tot_acres, profit_Well_ID, output_rate_acin_day)]
  econ_output[, `:=`(row, 1:.N)]
  econ_output[, `:=`(tax, tax_amount)]
  econ_output_in = fread("./Econ_output/KS_DSSAT_output.csv")

  econ_output = rbind(econ_output_in, econ_output)
  # econ_output[is.na(output_rate_acin_day), `:=`(output_rate_acin_day, 0)]
  # well_capacity_data = lookup_table_all_years_2[, .(Well_ID, output_rate_acin_day)]

  econ_output[, `:=`(output_rate_acin_day, 0)]
  well_capacity_data = lookup_table_all_years_2[, .(Well_ID, output_rate_acin_day=0)]

  write.csv(econ_output, econ_output_file, row.names = FALSE)
  write.csv(well_capacity_data, well_capacity_file_year, row.names = FALSE)
}
