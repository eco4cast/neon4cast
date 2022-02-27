#' Download target data from s3
#'
#' @param dir full path to working directory
#' @param config flare configuration object
#'
#' @return
#' @export
#'
get_target <- function(dir, site, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  download_s3_objects(dir,
                      bucket = "targets",
                      prefix = site)
}

####

download_s3_objects <- function(dir, bucket, prefix, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  
  files <- aws.s3::get_bucket(bucket = bucket,
                              prefix = prefix,
                              region = region,
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],
                          bucket = bucket,
                          file = file.path(dir, bucket, keys[i]),
                          region = region,
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}