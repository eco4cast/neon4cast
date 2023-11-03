get_target <- function(variable, duration, project_id = "neon4cast", lazy = FALSE){

  s3_targets <- arrow::s3_bucket("bio230014-bucket01/challenges/targets", endpoint_override = "sdsc.osn.xsede.org")

  target <- arrow::open_csv_dataset(s3_targets,
                                    schema = arrow::schema(
                                      project_id = arrow::string(),
                                      site_id = arrow::string(),
                                      datetime = arrow::timestamp(unit = "ns", timezone = "UTC"),
                                      duration = arrow::string(),
                                      variable = arrow::string(),
                                      observation = arrow::float()),
                                    skip = 1) |>
    dplyr::filter(variable %in% variable,
                  duration == duration,
                  project_id == project_id)

  if(!lazy) target <- target |> dplyr::collect()

}
