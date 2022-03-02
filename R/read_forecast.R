
#GENRALIZATION:  Specific target variables, missing spatial dimensions
read_forecast <- function(file_in, 
                          target_variables = c("oxygen", 
                                               "temperature", 
                                               "richness",
                                               "abundance", 
                                               "nee",
                                               "le", 
                                               "vswc",
                                               "gcc_90",
                                               "ixodes_scapularis",
                                               "amblyomma_americanum"),
                          reps_col = "ensemble",
                          ...){

  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){  
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, lazy = FALSE, show_col_types = FALSE) 

    
  } else if(grepl("[.]nc", file_in)){ #if file is nc
    out <- read_forecast_nc(file_in, target_variables, reps_col)
  }
  
  out
}

#GENERALIZATION: Specific target variables
read_forecast_nc <- function(file_in,
                             target_variables = c("oxygen", 
                                                  "temperature", 
                                                  "richness",
                                                  "abundance", 
                                                  "nee",
                                                  "le", 
                                                  "vswc",
                                                  "gcc_90",
                                                  "ixodes_scapularis",
                                                  "amblyomma_americanum"),
                             reps_col = "ensemble")
{    
  nc <- ncdf4::nc_open(file_in)
  time_nc <- as.integer(ncdf4::ncvar_get(nc, "time"))
  t_string <- strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]]
  if(t_string[1] == "days"){
    tustr<-strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")
    time_nc <-lubridate::as_date(time_nc,origin=unlist(tustr)[3])
  }else{
    tustr <- lubridate::as_datetime(strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]][3])
    time_nc <- as.POSIXct.numeric(time_nc, origin = tustr)
  } 
  targets <- names(nc$var)[which(names(nc$var) %in% target_variables)]
  ncdf4::nc_close(nc)
  
  nc_tidy <- tidync::tidync(file_in)
  df <- nc_tidy %>% tidync::hyper_tibble(select_var = targets[1])
  
  if(length(targets) > 1){
  for(i in 2:length(targets)){
    new_df <- nc_tidy %>% tidync::hyper_tibble(select_var = targets[i]) %>% 
      dplyr::select(targets[i]) 
    df <- dplyr::bind_cols(df, new_df)
  }
  }
    
  time_tibble <- tibble::tibble(time = unique(df$time),
                                new_value = time_nc)
  
  df <- df %>% 
    dplyr::left_join(time_tibble, by = "time") %>% 
    dplyr::mutate(time = new_value) %>% 
    dplyr::select(-new_value)
  
  if("site" %in% names(df)){
    nc <- ncdf4::nc_open(file_in)
    #GENERALIZATION:  Hack because ticks didn't make siteID unique in Round 1
    if(("ixodes_scapularis" %in% nc$var | "amblyomma_americanum" %in% nc$var) & "plotID" %in% nc$var){
      siteID <- ncdf4::ncvar_get(nc, "plotID")
    }else{
      siteID <- ncdf4::ncvar_get(nc, "siteID")  
    }
    ncdf4::nc_close(nc)
    
    site_tibble  <- tibble::tibble(site = unique(df$site),
                                   new_value = as.vector(siteID))
    df <- df %>% 
      dplyr::left_join(site_tibble, by = "site") %>% 
      dplyr::mutate(site = new_value) %>% 
      dplyr::select(-new_value) 
  }
  
  if("depth" %in% names(df)){
    nc <- ncdf4::nc_open(file_in)
    depth <- ncdf4::ncvar_get(nc, "depth")
    ncdf4::nc_close(nc)
    
    depth_tibble  <- tibble::tibble(depth = unique(df$depth),
                                   new_value = as.vector(depth)) 
    df <- df %>% 
      dplyr::left_join(depth_tibble, by = "depth") %>% 
      dplyr::mutate(time = new_value) %>% 
      dplyr::select(-new_value)
  }
  
  out <- df %>% 
    dplyr::select(any_of(c("time", "site","depth","ensemble", "forecast","data_assimilation", targets)))
  
  out
  
}