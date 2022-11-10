
# date_recorded should be of type date
# num_private ??
# public_meeting should be of type bool
# permit should be of type bool

change_datatypes <- function(data){
  convert_to_bool <- c("public_meeting",
                       "permit")
  convert_to_factor <- c("district_code",
                         "region_code")
  data[, convert_to_bool] <- data[, lapply(.SD, as.logical), .SDcols = convert_to_bool]
  data[, convert_to_factor] <- data[, lapply(.SD, as.factor), .SDcols = convert_to_factor]
  data[, date_recorded := as.Date(date_recorded)]
  return(data)
}





