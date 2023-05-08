# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing


# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.radon-bikes.de/"

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_category_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".megamenu__item > a") %>%
  html_attr('href') %>%
    
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"wear")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.radon-bikes.de{subdirectory}bikegrid/")
  )


# 2.0 COLLECT BIKE DATA ----
get_bike_data <- function(bike_category_url) {
  
  # get category name
  category_name = bike_category_url %>% str_extract("(?<=/)[:alpha:]+(?=/bikegrid/)")
  
  # Wait between each request to reduce the load on the server 
  # Otherwise we could get blocked
  #Sys.sleep(5)
  # Get the names, prices and URLs for the bikes of the first category
  html_bike_category  <- read_html(bike_category_url)
  
  bike_model_names_tbl <- html_bike_category %>%
    html_nodes(css = ".m-bikegrid__info > a > div > .a-heading.a-heading--small")%>%
    html_text()%>%
    str_extract("[:alnum:].*[:alnum:]")%>%
    # Convert vector to tibble
    enframe(name = "position", value = "model_name")
  
  bike_model_prices <- html_bike_category %>%
    html_nodes(css = ".m-bikegrid__info > a > div > .m-bikegrid__price.currency_eur > .m-bikegrid__price--active")%>%
    html_text()%>%
    str_extract("[0-9]+")%>%
    # Convert vector to tibble
    enframe(name = "position", value = "model_price") %>%
    mutate(model_price = as.numeric(model_price)) %>%
    mutate(model_price = scales::dollar(model_price, big.mark = ".", 
                                        decimal.mark = ",", 
                                        prefix = "", 
                                        suffix = " €"))
  
  bike_model_urls <- html_bike_category %>%
    html_nodes(css = ".m-bikegrid__info > a")%>%
    html_attr("href")%>%
    # Convert vector to tibble
    enframe(name = "position", value = "model_url")%>%
    # Add the domain, because we will get only the subdirectories
    mutate(
      model_url = glue("https://www.radon-bikes.de{model_url}")
    )
  
  
  bikes_tbl <- left_join(bike_model_names_tbl, bike_model_prices) %>%
    left_join(bike_model_urls) %>%
    add_column(category_name)
  return(bikes_tbl)
}

# Run the function with the first url to check if it is working
bike_category_url <- bike_category_tbl$url[1]
bike_data_tbl     <- get_bike_data(bike_category_url)


# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)

