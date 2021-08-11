#' This function produces annual amounts of water use and profits.
#' @param tax_amount                     is the amount of tax on the unit of groundwater extracted. Defaults to 1.1.
#' @param soil_weather_file              is the file that contains the soil types for each well in the region. Defaults to "C:/Users/manirad/Dropbox/DSSAT subregions work pre-2018 annual meeting/subregion KS files/outputs_for_econ/Well_Soil Type.csv".
#' @param well_capacity_files            is the directory where well capacity files are located. Defaults to "C:/Users/manirad/Downloads/test/Well Capacity".
#' @param authorized_gpm              is the the maximum pump rate that each well can pump. It defaults to initial well capacity. However, a separate maximum pump rate can be provided. Column name should be Well_Capacity(gpm).
#' @param econ_output_file               is the name of the output file that contains irrigated acres, irrigation, and profits for each well. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/KS_DSSAT_output.csv",
#' @param well_capacity_file_year        is the name of the output file that contains irrigation for each well which will be used by MODFLOW. Defaults to "C:/Users/manirad/Downloads/test/KS_DSSAT_output.csv",
#' @param first_year_of_simulation       is the first year that the hydro-economic simulation starts. Defaults to 2000.
#' @param default_well_capacity_col_name is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.
#' @param missing_soil_types             is the soil type that is assigned to missing soil types for wells. Defaults to "KS00000007".
#' @param minimum_well_capacity          is the minimum well capacity in the model. If well capacity falls below this capacity, it is set to this minimum. Defaults to 100  gallons per minute.
#' @param maximum_well_capacity          is the maximum well capacity in the model. If well capacity falls above this capacity, it is set to this maximum. Defaults to 3000 gallons per minute.
#' @param first_year_of_GW               is the first year that the GW model exists. This may be different than the first year of simulation. Defaults to 1997.
#' @param last_year_of_GW                is the last  year that the GW model exists. This may be different than the last  year of simulation. Defaults to 2007.
#' @param well_capacity_intervals        is the intervals in well capacities. Defaults to 20.
#' @return                               returns the output table.
#' @examples
#' \dontrun{
#' gen_lookup_tax(tax_amount = 2)
#' }
#' @export
annual_model_tax_CO_2 = function (tax_amount = 1,
                                  soil_weather_file   = "./input_files/Well_SoilType_WeatherStation_CO.csv",
                                  well_capacity_files = "./Well Capacity",
                                  authorized_gpm =   "./2000_Well_Capacity.csv",
                                  econ_output_file =    "./Econ_output/CO_DSSAT_output.csv",
                                  well_capacity_file_year = "./CO_DSSAT_output.csv",
                                  first_year_of_simulation = 2000,
                                  default_well_capacity_col_name = "Well_Capacity(gpm)",
                                  missing_soil_types = "KS00000007",
                                  minimum_well_capacity = 1,
                                  maximum_well_capacity = 1000,
                                  first_year_of_GW = 2000,
                                  last_year_of_GW = 2018,
                                  well_capacity_intervals = 20)
{
  tax_amount = (tax_amount - 1)/10
  soil_type = fread(soil_weather_file)
  soil_type[, `:=`(Soil_Type, gsub("KSFC00000", "KS0000000",
                                   Soil_Type))]
  soil_type = soil_type[complete.cases(Well_ID)]
  filenames = list.files(path = well_capacity_files, pattern = "*.csv",
                         full.names = TRUE)
  ldf <- lapply(filenames, fread, fill = T)
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  year_dt = rbindlist(ldf)
  foo = data.table::data.table(Well_ID = 1, V1 = 1, file_name = paste0(well_capacity_files,
                                                                       "/", "Well_Capacity_",
                                                                       first_year_of_simulation, ".csv"))
  setnames(foo, old = "V1", new = default_well_capacity_col_name)
  year_dt = rbind(year_dt, foo)
  
  year_dt[, file_name := substring(file_name, regexpr("Capacity_", file_name) + 1, nchar(file_name))]
  year_dt[, file_name := substring(file_name, regexpr("_", file_name) + 1, nchar(file_name))]
  year_dt[, file_name := gsub(".csv", "", file_name)]
  
  year_dt[, `:=`(file_name, as.integer(file_name))]
  setkey(year_dt, file_name)
  year_dt = year_dt[nrow(year_dt)]
  year_dt = year_dt$file_name
  year_2 = year_dt
  well_capacity = data.table(Well_ID = NA, V1 = NA)
  setnames(well_capacity, old = "V1", new = default_well_capacity_col_name)
  well_capacity = ifelse(year_dt == first_year_of_simulation,
                         list(rbind(well_capacity, fread(paste0("Well_Capacity.csv")))), 
                         list(rbind(well_capacity, fread(paste0("./Well Capacity/", "Well_Capacity_", year_dt, ".csv")))))
  
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
  well_capacity_data[is.na(weather_station), `:=`(weather_station,
                                                  well_capacity_data[1, weather_station])]
  well_capacity_data = well_capacity_data[!is.na(weather_station)]
  well_capacity_data[, `:=`(Well_capacity, round(Well_capacity))]
  authorized_gpm_dt = fread(authorized_gpm)
  authorized_gpm_dt[authorized_rate == -999, authorized_rate := authorized_gpm_dt[,max(authorized_rate)]]
  setkey(well_capacity_data, Well_ID)
  setkey(authorized_gpm_dt, Well_ID)
  well_capacity_data = well_capacity_data[authorized_gpm_dt]
  well_capacity_data[, `:=`(authorized_rate, round(authorized_rate))]
  well_capacity_data[Well_capacity > authorized_rate,
                     `:=`(authorized_rate, Well_capacity)]
  well_capacity_data[, `:=`(well_capacity_org, Well_capacity)]
  well_capacity_data[Well_capacity <= minimum_well_capacity,
                     `:=`(Well_capacity, minimum_well_capacity)]
  well_capacity_data[Well_capacity >= maximum_well_capacity,
                     `:=`(Well_capacity, maximum_well_capacity)]
  well_capacity_data[, Well_capacity := floor(Well_capacity/well_capacity_intervals)*well_capacity_intervals]
  well_capacity_data[Well_capacity !=0, Well_capacity := Well_capacity + 1]
  
  lookup_table_all_years_2 = readRDS("lookup_table_all_years_2.rds")
  lookup_table_all_years_2 = unique(lookup_table_all_years_2, by=colnames(lookup_table_all_years_2))
  filenames = list.files(path = well_capacity_files, pattern = "*.csv",
                         full.names = TRUE)
  ldf <- lapply(filenames, fread, fill = T)
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  year_dt = rbindlist(ldf)
  foo = data.table(Well_ID = 1, V1 = 1, file_name = paste0(well_capacity_files,
                                                           "/", "Well_Capacity_",
                                                           first_year_of_simulation, ".csv"))
  
  
  setnames(foo, old = "V1", new = default_well_capacity_col_name)
  year_dt = rbind(year_dt, foo)
  
  year_dt[, file_name := substring(file_name, regexpr("Capacity_", file_name) + 1, nchar(file_name))]
  year_dt[, file_name := substring(file_name, regexpr("_", file_name) + 1, nchar(file_name))]
  year_dt[, file_name := gsub(".csv", "", file_name)]
  
  year_dt[, `:=`(file_name, as.integer(file_name))]
  setkey(year_dt, file_name)
  year_dt = year_dt[nrow(year_dt)]
  # year_dt[, `:=`(file_name, ifelse(file_name <= (last_year_of_GW - 1), file_name + 1,
  #                                  ifelse(file_name > (last_year_of_GW - 1) & file_name <= (last_year_of_GW - 1 + last_year_of_GW -first_year_of_GW + 1), file_name - (-1 + last_year_of_GW - first_year_of_GW + 1),
  #                                         ifelse(file_name > (last_year_of_GW - 1 + last_year_of_GW - first_year_of_GW + 1) & file_name <= (last_year_of_GW - 1 + 2 * (last_year_of_GW - first_year_of_GW + 1)), file_name - (-1 + 2 * (last_year_of_GW - first_year_of_GW + 1)),
  #                                                ifelse(file_name > (last_year_of_GW - 1 + 2 * (last_year_of_GW - first_year_of_GW + 1)) & file_name <= (last_year_of_GW - 1 + 3 * (last_year_of_GW - first_year_of_GW + 1)), file_name - (-1 + 3 * (last_year_of_GW - first_year_of_GW + 1)), file_name - (-1 + 4 * (last_year_of_GW - first_year_of_GW + 1)))))))]
  year_dt = year_dt$file_name
  print(year_dt)
  
  lookup_table_all_years_2[, `:=`(Well_ID, NULL)]
  
  lookup_table_all_years_2 = merge(lookup_table_all_years_2, well_capacity_data, 
                                   by.x= c("WSTA", "SOIL_ID", "Well_capacity"), 
                                   by.y= c("weather_station", "Soil_Type", "Well_capacity"), 
                                   allow.cartesian = T)
  lookup_table_all_years_2 = lookup_table_all_years_2[tot_quarters <= QCircle_Count]
  lookup_table_all_years_2 = lookup_table_all_years_2[quarter      <= QCircle_Count]
  lookup_table_all_years_2[, mean_profit_quarter := mean(profit), by=c("Well_ID", "tot_acres", "quarter")]
  
  setkey(lookup_table_all_years_2, Well_ID, tot_acres, quarter)
  
  foo = unique(lookup_table_all_years_2, by=c("Well_ID", "tot_acres", "quarter"))
  foo[, profit_tot_acres := sum(mean_profit_quarter), by=c("Well_ID", "tot_acres")]
  foo = unique(foo, by=c("Well_ID", "tot_acres"))
  foo[, max_profit_tot_acres := max(profit_tot_acres), by="Well_ID"]
  foo = foo[max_profit_tot_acres   ==     profit_tot_acres,.(Well_ID, tot_acres, id=1)]
  
  setkey(foo,                      Well_ID, tot_acres)
  setkey(lookup_table_all_years_2, Well_ID, tot_acres)
  lookup_table_all_years_2 = lookup_table_all_years_2[foo]
  
  lookup_table_all_years_2 = lookup_table_all_years_2[HDAT ==
                                                        year_dt]
  
  # setkey(lookup_table_all_years_2, WSTA, SOIL_ID, Well_capacity, quarter)
  # lookup_table_all_years_2[, quarter := 1:.N, by=c("WSTA", "SOIL_ID", "Well_capacity")]
  
  # setkey(lookup_table_all_years_2, WSTA, SOIL_ID, Well_capacity)
  # setkey(well_capacity_data, weather_station, Soil_Type, Well_capacity)
  
  # lookup_table_all_years_2[, count := .N, by="Well_ID"]
  
  # lookup_table_all_years_2 = lookup_table_all_years_2[well_capacity_data]
  # lookup_table_all_years_2 = unique(lookup_table_all_years_2, by=c("Well_ID", "quarter"))
  
  
  # lookup_table_all_years_2[, foo := ifelse(CR %like% "dry" | CR == "FA", 0, 32.5)]
  # lookup_table_all_years_2[, tot_acres:= sum(foo), by="Well_ID"]
  
  econ_output = lookup_table_all_years_2[, .(Well_ID, well_capacity = well_capacity_org,
                                             tot_acres, quarter, CR, PAW, irrigation_ac_in = irrigation, irrigation_depth_mm = round(irrigation/32.5 * 25.4), profit, yield_kg_ac)]
  econ_output[, `:=`(row, 1:.N)]
  econ_output[, `:=`(tax, tax_amount)]
  write.csv(econ_output, well_capacity_file_year, row.names = FALSE)
  
  econ_output = econ_output[,.(Well_ID, year = year_dt, well_capacity, tot_acres, quarter, CR, PAW, irrigation = irrigation_ac_in, profit, yield_kg_ac, row, tax)]
  econ_output_in = fread(econ_output_file)
  econ_output = rbind(econ_output_in, econ_output)
  write.csv(econ_output, econ_output_file, row.names = FALSE)
}
