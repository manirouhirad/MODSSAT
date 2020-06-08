#' This function generates the lookup table with a tax policy for KS.
#' @param seed                           Determines the random draw. This value is updated within a loop for bootstrapping.
#' @param number_of_CREP_wells           is the number of wells that will be retired as a result of the conservation policy. Defaults to 169.
#' @param well_id_file                   is the file that contains the well ID's.
#' @param output_file                    is the output file that includes well ID for retired wells.
#' @return                               returns the output table.
#' @export
gen_CREP_wells = function (seed = 1,
          number_of_CREP_wells = 169,
          well_id_file = "./input_files/all_well_IDs.csv",
          output_file  = "CREP_wells.csv")
{
  set.seed(seed)
  well_ids = fread(well_id_file)
  well_ids = well_ids[sample(.N, number_of_CREP_wells)]
  write.csv(well_ids, output_file, row.names = F)
}
