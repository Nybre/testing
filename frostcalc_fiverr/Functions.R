
library("jsonlite")
library("lubridate")
library("httr")

probability_color = "purple"
temperature_color = "red"

from_date = as.Date("1990-01-01")
#from_date = as.Date("1979-01-01")

freeze_temperature = 1

number_day_in_period = 8
quantiles_values = c(0.2, 0.8)
square_size_km = 5
data_source = "ERA5"
data_type = "temperature_2m_min"
use_avg = TRUE

api_root = "https://api.staging.ibisa.tech/"
api_key = "QhlSkxuol24YzcgnXjM3d6aMgmTFg1HCAc4Md8zf"
frost_api_name = "frostcalculator"
stat_api_name = "tsstats"

from_production = FALSE
file_description = "analyse a location"
export_plots = TRUE
graph_type = "b"
export_graph_width = 1024

number_useless_colomn = 8

to_date = Sys.Date() - 4*31 # We remove 3 month because of teh delay to get ERA5 data


time_code_to_date_north = function(time_code) {
  year = 1901
  month = time_code %/% 100
  day = time_code %% 100
  return(ISOdate(year, month, day))
}


call_api = function(final_url, body_list, api_key) {
  body_json_1 = toJSON(body_list, auto_unbox = TRUE, digits=NA)
  body_json = gsub('\\[]', '\\{}', body_json_1)
  # raw_data = tryCatch(
  #   {respo = httr::POST(url = final_url, add_headers("Content-Type" = 'application/json', 'X-API-KEY' = api_key), body = body_json)},
  #   error=function(cond) {
  #     cat(paste0("Error while loading: ", final_url,"\n"))
  #     return(tryCatch(
  #       {httr::POST(url = final_url, add_headers("Content-Type" = 'application/json', 'X-API-KEY' = api_key), body = body_json)},
  #       error=function(cond) 
  #       {cat(paste0("Error with the request :",final_url,"\n"))
  #         stop(cond)
  #       }))}
  # )
  raw_data = httr::POST(url = final_url, add_headers("Content-Type" = 'application/json', 'X-API-KEY' = api_key), body = body_json)
  
  if (raw_data$status_code != 200) {
    stop(paste0("Error during teh execution of the request. Status ", raw_data$status_code, ", message content : '",
                fromJSON(content(raw_data, 'text')),"'. Resquest was : '",body_json,"'"))
  }
  
  result = fromJSON(content(raw_data, 'text'))
  
  return (result)
}


probability = function(latitude, longitude) {
  
  final_url = paste0(api_root,frost_api_name)
  
  body_list = list(latitude = latitude, longitude = longitude, 
                   from_date = as.character(from_date), freeze_temperature = freeze_temperature
  )
  
  
  result = call_api(final_url, body_list, api_key)
  
  probalilbities = data.frame(
    time_code = as.numeric(names(result)),
    freeze_probability = unlist(result)
  )
  probalilbities = probalilbities[order(probalilbities$time_code),]
  
  
  
  if (latitude > 0) {
    probalilbities_with_date = cbind(date = time_code_to_date_north(probalilbities$time_code), probalilbities)
  } else {
    stop("Not implemented")
  }
  
  
  return(probalilbities_with_date)
  
}


history = function(latitude, longitude) {
  
  final_url = paste0(api_root,stat_api_name)
  
  body_list = list(latitude = latitude, longitude = longitude, 
                   to_date = to_date,
                   from_date = as.character(from_date),
                   square_size_km = square_size_km,
                   data_source = data_source,
                   data_type = data_type,
                   number_day_in_period = number_day_in_period,
                   quantiles_list = quantiles_values,
                   use_avg = use_avg, use_sum = !use_avg
  )
  
  result = call_api(final_url, body_list, api_key)
  
  data_periode = data.frame(
    period = result$period,
    median = result$median,
    quantile1 = result$quantiles[1][[1]],
    quantile2 = result$quantiles[2][[1]]
  )
  
  
  return(data_periode)
  
}


