#' This function produces annual well capacity for each well in each year.
#' @param well_capacity_for_econ         is the file name that all the output well capacities are written into. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/well_capacity.csv".
#' @param first_year_of_simulation       is the first year that the hydro-economic simulation starts. Defaults to 2000.
#' @param default_well_capacity_col_name is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.
#' @param sim_id_or_well_num             is the number id for the simulation or the number of retired wells.
#' @return                               returns the output table.
#' @export
gen_output_CREP = function (well_capacity_files = "./Well Capacity",
          well_capacity_for_econ = "./Econ_output/well_capacity.csv",
          first_year_of_simulation = 2000,
          default_well_capacity_col_name = "Well_Capacity(gpm)",
          sim_id_or_well_num = 1)
{
  filenames = list.files(path = well_capacity_files, pattern = "*.csv",
                         full.names = TRUE)
  ldf <- lapply(filenames, fread, fill = T)
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  ldf = rbindlist(ldf)
  foo = data.table::data.table(Well_ID = 1, V1 = 1, file_name = paste0(well_capacity_files,
                                                                       "/", first_year_of_simulation, "_Capacity.csv"))

  setnames(foo, old = "V1", new = default_well_capacity_col_name)
  ldf = rbind(ldf, foo)
  ldf[, `:=`(file_name, substr(file_name, nchar(file_name) -
                                 16, nchar(file_name) - 13))]
  ldf[, `:=`(file_name, as.integer(file_name))]
  ldf[, `:=`(simulation_id, sim_id_or_well_num)]
  write.csv(ldf, well_capacity_for_econ, row.names = F)
}
