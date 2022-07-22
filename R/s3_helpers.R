# DEPRECATE and mark for removal

#' Download target data from s3
#'
#' @param dir full path to working directory
#' @param theme forecast theme
#' @param s3_region s3 region
#' @export
#'
get_target <- function(dir, theme, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  download_s3_objects(dir,
                      bucket = "neon4cast-targets",
                      prefix = theme)
}

####

download_s3_objects <- function(dir, bucket, prefix, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  
  files <- aws.s3::get_bucket(bucket = bucket,
                              prefix = prefix,
                              region = s3_region,
                              use_https = as.logical(Sys.getenv("USE_HTTPS")),
                              max = Inf)
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  files_present <- TRUE
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],
                          bucket = bucket,
                          file = file.path(dir, bucket, keys[i]),
                          region = s3_region,
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }else{
    message("Requested files are not available on the s3 bucket")
    files_present <- FALSE
  }
  invisible(files_present)
}
