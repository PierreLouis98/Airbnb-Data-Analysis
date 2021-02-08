library(dplyr)
library(stringr)
library(data.table)
library(glue)

########################### Functions Declaration ############################


# Extract metadata from url scrapped csv file
extract_all_meta <- function(urls) {
    extract_meta <- function(link) str_split(link, '/', simplify=TRUE)[4:7]
    meta <- sapply(urls$listings_data_url, extract_meta)
    meta <- transpose(data.frame(meta))
    df <- data.frame(meta, urls)
    names(df) <- c("country", "region", "city", "date", "url")
    return(df)
}

# Prepare and clean data for a given Country
prepare_data <- function(listings_city, calendar_city) {
    
    # Used for generated csv file name
    city <- unique(listings_city$city)
    date <- unique(listings_city$date)
    
    # Make Id type match for future join
    listings_city <- listings_city %>%  
        mutate(id = as.numeric(id))
    
    calendar_city <- calendar_city %>%  
        mutate(listing_id = as.numeric(listing_id))
    
    print(glue("Preparing {city} at {date}..."))
    
    # Columns to Keep 
    columns_listings <- c("id","country", "region", "city","date", "neighbourhood_cleansed", "latitude", "longitude", 
                          "property_type", "room_type", "accommodates", "bedrooms", "beds", "price", "minimum_nights",  "maximum_nights", "review_scores_rating")
    
    # Select Columns to Keep 
    listings_city <- listings_city %>% 
        select(columns_listings) %>% 
        arrange(id)
    
    # Add case of more then 5 bedrooms
    listings_city$bedrooms <- ifelse(listings_city$bedrooms >= 5, "5+", listings_city$bedrooms)
    
    # Cleaning of the calendar
    
    # Arrange df by id and dates
    calendar_city <- calendar_city %>% 
        arrange(listing_id, date)
    
    # Get Number of day by id
    calendar_city <- calendar_city %>%
        group_by(listing_id) %>%
        mutate(day_nb = row_number()) %>%
        ungroup()
    
    # Get Binary type for Availability
    calendar_city <- calendar_city %>%
        mutate(available = ifelse(available == "t", 1, 0))
    
    # Clean and give type to price
    calendar_city <- calendar_city %>%
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", ""))
    calendar_city <- calendar_city %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", ""))
    calendar_city <- calendar_city %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))
    
    # Get Revenue based on price and availability
    calendar_city <- calendar_city %>%
        mutate(revenue = price*(1-available))
    
    # Get availability, price and revenue for the next 30 days
    calendar_city <- calendar_city %>%
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                  .groups = "keep")
    
    # Join calendar and listing
    listings_cleansed <- listings_city %>% left_join(calendar_city, by = c("id" = "listing_id"))
    
    # Create a csv file for the city
    write.csv(listings_cleansed, file.path("./data/cities/", glue("listing_{city}_{date}.csv")))
}

# Download and prepare data from urls by keeping the last top n dates
download_data <- function(df, countries, last_n_dates) {
    
    # Apply to every Countries
    for (country in countries) {
        
        # Extract given Country
        df_country <- df[df$country == country,]
        
        # Order then by date and keep the n most recent scrapped dates
        df_country <- df_country[order(as.Date(df_country$date, format="%Y/%m/%d")),]
        df_country <- top_n(group_by(df_country, city), last_n_dates)
        
        # For every city for the n latest dates, add meta and prepare data (with calendar and listings)
        lapply(1:nrow(df_country), 
               function(i) {
                   print(glue("{i}/{nrow(df_country)}"))
                   row <- df_country[i,]
                   
                   listings <- fread(row$url)
                   listings$country <- row$country
                   listings$region <- row$region
                   listings$city <- row$city
                   listings$date <- row$date
                   
                   calendars <- fread(str_replace(row$url, "listings.csv", "calendar.csv"))
                   calendars$country <- row$country
                   calendars$region <- row$region
                   calendars$city <- row$city
                   calendars$date <- row$date
                   
                   prepare_data(listings, calendars)
               }
        )
    }
}

# Merge all cities csv into one dataframe
load_global_listings <- function(){
    files  <- list.files(file.path("../data/cities/"), pattern = '\\.csv')
    tables <- lapply(file.path("../data/cities/", files), read.csv, header = TRUE)
    final_df <- do.call(rbind , tables)
    final_df$X <- NULL
    final_df <- final_df %>%
        mutate(latitudelongitude = str_c(latitude,":",longitude))
    #write.csv(final_df, file.path("./data/",  "global_listings.csv"), row.names=FALSE)
    return(final_df)
}


############################ Function usage ####################################

# Extract important data from url
#urls <- read.csv(file.path("./data/all_data_urls.csv"))
#df <- extract_all_meta(urls)

# Download data for list a countries
#countries <- c("france", "spain", "the-netherlands", "germany", "belgium","italy") #
#download_data(df, countries, 3)

# Get Final preprocess dataset
#df <- load_global_listings()

############################ End ###############################################
