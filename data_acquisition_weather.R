library(httr)
library(glue)
library(jsonlite)
library(dplyr)
library(imager)
library(tidyverse)
library(ggplot2)
library(ggdark)
library(cimir)


# https://api.open-meteo.com/v1/dwd-icon?latitude=53.55&longitude=9.99&hourly=temperature_2m,relativehumidity_2m,apparent_temperature,rain,weathercode,cloudcover,windspeed_10m,winddirection_10m&timezone=Europe%2FBerlin

weather_api <- function(path = "") {
  #url <- modify_url(url = "https://api.open-meteo.com/", path = glue("v1/dwd-icon?latitude=53.55&longitude=9.99&hourly=temperature_2m,relativehumidity_2m,apparent_temperature,rain,weathercode,cloudcover,windspeed_10m,winddirection_10m&timezone=Europe%2FBerlin"))
  url <- "https://api.open-meteo.com/v1/dwd-icon?latitude=53.55&longitude=9.99&hourly=temperature_2m,relativehumidity_2m,apparent_temperature,rain,weathercode,cloudcover,windspeed_10m,winddirection_10m&timezone=Europe%2FBerlin"
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}


weather_imgs <- GET("https://gist.githubusercontent.com/stellasphere/9490c195ed2b53c707087c8c2db4ec0c/raw/db92e194f4f2109a68a706e46bc624eb3cbe3889/descriptions.json") %>%
                .$content %>% 
                rawToChar() %>% 
                fromJSON()

weather_json <- weather_api() %>% 
        .$content %>% 
        rawToChar() %>% 
        fromJSON()

current_time <- format(Sys.time(), "%Y-%m-%dT%H:00")

curr_idx <- which(sapply(weather_json[["hourly"]][["time"]], function(y) current_time %in% y))

curr_temp <- weather_json[["hourly"]][["temperature_2m"]][[curr_idx]]
curr_hum <- weather_json[["hourly"]][["relativehumidity_2m"]][[curr_idx]]
curr_apparent_temp <- weather_json[["hourly"]][["apparent_temperature"]][[curr_idx]]
curr_rain <- weather_json[["hourly"]][["rain"]][[curr_idx]]
curr_wcode <- weather_json[["hourly"]][["weathercode"]][[curr_idx]]
curr_wind <- weather_json[["hourly"]][["windspeed_10m"]][[curr_idx]]
curr_winddir <- weather_json[["hourly"]][["winddirection_10m"]][[curr_idx]] %>% 
                cimis_degrees_to_compass()

curr_weather_img <- weather_imgs[[as.character(curr_wcode)]][["day"]][["image"]]

render_weather_img <- glue("[![]({curr_weather_img})](http://openweathermap.org)")

render_weather_img

curr_weather_tab <- glue("|                      |                         |  
                          |:---------------------|------------------------:|
                          | temperature          | {curr_temp} °C          |  
                          | apparent temperature | {curr_apparent_temp} °C |
                          | relative humidity    | {curr_hum} %            |  
                          | rain                 | {curr_rain} mm          |
                          | wind speed           | {curr_wind} km/h        |
                          | wind direction       | {curr_winddir} °        |
                          ")

  
curr_weather_tab



weather_tbl <- as_tibble(weather_json[["hourly"]]) %>%
               mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:00"))

max_temp <- weather_tbl %>% 
            summarize(max(temperature_2m)) %>%
            first %>% first %>%
            as.numeric()

min_temp <- weather_tbl %>% 
            summarize(min(temperature_2m)) %>%
            first %>% first %>%
            as.numeric()

weather_tbl %>%
  
  # Set up x, y
  ggplot(aes(x = time, y = temperature_2m, group=1)) +
  
  geom_line() + 
  dark_theme_gray() +
  scale_x_datetime(name="", breaks = NULL, date_breaks = "1 day", date_labels = "%d/%m")+
  scale_y_continuous(name="°C", limits=c(min_temp, max_temp))

