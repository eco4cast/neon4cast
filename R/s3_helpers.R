#' Download target data from s3
#'
#' @param dir full path to working directory
#' @param theme forecast theme
#'
#' @return
#' @export
#'
get_target <- function(dir, theme, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  download_s3_objects(dir,
                      bucket = "targets",
                      prefix = theme)
}

####

download_s3_objects <- function(dir, bucket, prefix, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  
  files <- aws.s3::get_bucket(bucket = bucket,
                              prefix = prefix,
                              region = s3_region,
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],
                          bucket = bucket,
                          file = file.path(dir, bucket, keys[i]),
                          region = s3_region,
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}