### Scrape BACP registered counsellors
# Required libraries
library(polite)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
library(magrittr)
library(downloader)
library(readxl)
library(sf)

# Pages to scrape
pages <- seq(0, 37490, 10)

log_start <- Sys.time()


# Check that the robots.txt accepts scraping
bacp_bow <- polite::bow(url = "https://www.bacp.co.uk/search/Register", 
                        force = TRUE)

# Scraping function to be mapped onto each page URL
get_posts <- function(page, bow = bacp_bow){
  print(paste0("https://www.bacp.co.uk/search/Register?skip=", page))
  general <- polite::scrape(bacp_bow,
                           query = list(skip = page)) %>%
    rvest::html_node("main") %>%
    rvest::html_node("form") %>%
    rvest::html_nodes("div.template-search__body") %>%
    rvest::html_nodes("div.template-search__results") %>%
    rvest::html_nodes("article") 
  
  name_post <- general %>%
    rvest::html_nodes("header") %>%
    rvest::html_nodes("div.search-result__title") %>%
    rvest::html_text()
  # Split out names and locations - name comes first
  nums <- 1:length(name_post)
  names <- name_post[seq(1, length(nums), 2)]
  # Remove backslash followed by r or n and multiple spaces
  names <- gsub("(\\\r)|(\\\n)|(  )", "", names)
  locations <- name_post[seq(2, length(nums), 2)]
  postcodes <- stringr::str_extract(locations, "[A-Z]{1,3}[0-9]{1,3}[A-Z]*")  
  
  # Get accreditation status
  member_type <- general %>%
    rvest::html_nodes("div.search-result__body") %>%
    rvest::html_nodes("div.search-result__status") %>%
    rvest::html_attr("aria-label")
  
  result <- data.frame(names = names,
                       locations = locations,
                       postcodes = postcodes,
                       member_type = member_type,
                       stringsAsFactors = FALSE)
  print(paste0(Sys.time() - log_start))
  return(result)
}

# Get to each person's details - get name and postcode
#result <- purrr::map_dfr(pages, ~get_posts(.x))
#write.csv(result, "bacp_postcodes.csv")
result_saved <- read.csv("bacp_postcodes.csv")


# Count no per postcode
postcode_count <- result_saved %>%
  dplyr::filter(!is.na(postcodes)) %>%
  dplyr::group_by(postcodes, member_type) %>%
  dplyr::summarise(value = n()) %>%
  dplyr::ungroup()


### Add population per postcode ----
## England & Wales
# Source: 2011 census, NOMIS table KS101EW
postcode_pop <- read.csv("https://www.nomisweb.co.uk/output/census/2011/Postcode_Estimates_Table_1.csv")
postcode_pop$postcodes <- gsub(" +", "", gsub("[0-9][A-Z]{2}$", "", postcode_pop$Postcode))
pop_totals <- postcode_pop %>%
  dplyr::group_by(postcodes) %>%
  dplyr::summarise(population = sum(Total)) %>%
  dplyr::ungroup()
## Scotland
scot_pop <- read.csv("https://www.isdscotland.org/Products-and-Services/GPD-Support/Population/Census/_docs/2011/2011census_lc_pcsec2011_pop.csv?14:28:51")
scot_pop$postcodes <- gsub(" .+", "", scot_pop$pcsec2011)
scot_totals <- scot_pop %>%
  dplyr::group_by(postcodes) %>%
  dplyr::summarise(population = sum(Total)) %>%
  dplyr::ungroup()
## Northern Ireland
downloader::download("https://www.ninis2.nisra.gov.uk/Download/Census%202011/Headcount%20and%20Household%20Estimates%20for%20Postcodes.zip", 
         dest="ni_data.zip", mode="wb") 
unzip("ni_data.zip")
ni_pop <- readxl::read_excel("Headcount and Household Estimates for Postcodes.xlsx", 
                     skip = 4,
                     col_names = c("Postcode", "Total", "Males", "Females", "Households"),
                     col_types = c("text", "numeric", "numeric", "numeric", "numeric")) %>%
  dplyr::slice(., 1:(n()-9)) %>%
  dplyr::mutate(Total = ifelse(is.na(Total), 0, Total))
ni_pop$postcodes <- gsub(" ?[0-9][A-Z]{2}$", "", ni_pop$Postcode)
ni_totals <- ni_pop %>%
  dplyr::group_by(postcodes) %>%
  dplyr::summarise(population = sum(Total)) %>%
  dplyr::ungroup()

postcode_totals <- rbind(rbind(pop_totals, scot_totals), ni_totals)


### Combine data ----
postcode_data <- dplyr::full_join(postcode_count, postcode_totals, by = "postcodes") %>%
  dplyr::filter(!(is.na(postcodes)|is.na(population)))

scale_vec <- function(x){
  return((x - min(x, na.rm = T)) / 
           (max(x, na.rm = T) - min(x, na.rm = T)))
}

# Make sure all postcodes and membership types are represented
all_posts <- expand.grid(postcodes = unique(postcode_data$postcodes),
                         member_type = unique(postcode_data$member_type)[!is.na(unique(postcode_data$member_type))])

postcode_all <- dplyr::right_join(postcode_data, all_posts, by = c("postcodes", "member_type")) %>%
  dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>%
  dplyr::group_by(postcodes) %>% 
  # Disregard type of BACP membership - just add them all together
  dplyr::summarise(value = sum(value),
                   population = max(population, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(population > -1) %>%
  # Calculate number of counsellors per capita
  dplyr::mutate(memb_percap = value / population) %>%
  # Transformations because of skew
  dplyr::mutate(memb_percap_ra = rank(memb_percap),
                memb_log = log(value),
                memb_sqrt = sqrt(value),
                memb_cubrt = log10(value^(1/3)),
                memb_percap_log = log(memb_percap),
                memb_percap_sqrt = sqrt(memb_percap),
                memb_percap_cubrt = log10(memb_percap^(1/3)))

# Scale transformed columns to 0-1 for visualisation
trans_cols <- c("memb_percap", "memb_percap_ra", 
                "memb_log", "memb_sqrt", "memb_cubrt",
                "memb_percap_log", "memb_percap_sqrt", "memb_percap_cubrt")
postcode_sc <- as.data.frame(apply(postcode_all[, trans_cols], 
                     2, scale_vec))
colnames(postcode_sc) <- paste0(trans_cols, "_sc")
postcode_all <- cbind(postcode_all, postcode_sc)

### Get postcode polygons ----
downloader::download("https://datashare.is.ed.ac.uk/bitstream/handle/10283/2597/GB_Postcodes.zip?sequence=1&isAllowed=y",
                     dest = "poly.zip", mode = "wb")
unzip("poly.zip")
postalDistrict <-  sf::st_read('GB_Postcodes/PostalDistrict.shp')
postalDistrict$postcodes <- postalDistrict$PostDist

# Merge and visualise data
postcode_poly <- st_as_sf(merge(postcode_all, postalDistrict))

plot(postcode_poly["memb_sqrt_sc"], lty = 0)
plot(postcode_poly["memb_percap_cubrt_sc"], lty = 0)
plot(postcode_poly["memb_percap_ra_sc"], lty = 0)
