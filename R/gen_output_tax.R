#' This function produces annual well capacity for each well in each year.
#' @param tax_amount                     is the amount of tax on the unit of groundwater extracted. Defaults to 1.1.
#' @param well_capacity_for_econ         is the file name that all the output well capacities are written into. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/well_capacity.csv".
#' @param first_year_of_simulation       is the first year that the hydro-economic simulation starts. Defaults to 2000.
#' @param default_well_capacity_col_name is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.
#' @return                               returns the output table.
#' @examples
#' \dontrun{
#' gen_lookup_tax(tax_amount = 2)
#' }
#' @export
gen_output_tax = function(tax_amount=1.1,
                                      well_capacity_files     = "C:/Users/manirad/Downloads/test/Well Capacity",
                                      well_capacity_for_econ  = "C:/Users/manirad/Downloads/test/Econ_output/well_capacity.csv",
                                      first_year_of_simulation = 2000,
                                      default_well_capacity_col_name = "Well_Capacity(gpm)"){

  filenames = list.files(path = well_capacity_files, pattern="*.csv", full.names=TRUE)                                  # now read in the output of the MODFLOW model for this year
  ldf <- lapply(filenames, fread, fill=T)
  ldf <- mapply(cbind, ldf, "file_name"=filenames, SIMPLIFY=F)

  ldf = rbindlist(ldf)                                                                                               # to do this we need to read in all the existing outputs
  ldf = rbind(ldf, data.table(Well_ID=1, V1=1, file_name= paste0(well_capacity_files, "/", first_year_of_simulation, "_Capacity.csv"))) # put them together
  setnames(ldf, old = "V1", new = default_well_capacity_col_name)


  ldf[, file_name := substr(file_name, nchar(file_name)-16, nchar(file_name)-13)]                                    # get the year from file name
  ldf[, file_name := as.integer(file_name)]                                                                          # so it needs a little cleaning
  # foo = rnorm(1, mean = 200, sd = 5)
  # ldf[, policy_value := foo]

  ldf[, tax := tax_amount]
  write.table(ldf, sep = ",", file = well_capacity_for_econ,
              row.names = F, col.names = !file.exists(well_capacity_for_econ), append = T)

}
