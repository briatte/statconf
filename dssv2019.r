
library(dplyr)
library(httr) # for geocoding
library(purrr)
library(readr)
library(robotstxt)
library(rvest)
library(stringr)
library(tidyr) # `unnest`

# ------------------------------------------------------------------------------
# Scrape panels and papers
# ------------------------------------------------------------------------------

r <- "https://iasc-isi.org/dssv2019/"
stopifnot(robotstxt::paths_allowed(r))

cat("Scraping", r)

r <- str_c(r, "programme-aug-") # 13--15
d <- tibble()

for (i in 13:15) {
  
  h <- read_html(str_c(r, i))
  
  h <- map2(
    
    # panel titles
    html_nodes(h, "article h2") %>% 
      html_text %>% 
      # remove first part of text (time and room),
      # keep only acronym, title, organiser
      str_replace(html_text(html_nodes(h, "article h2 span")), ""),
    
    # panels...
    html_nodes(h, xpath = "//article/h2/following-sibling::table") %>% 
      # ... and papers
      map(html_nodes, "tr") %>% 
      map(
        ~ tibble(
          title = html_nodes(., xpath = "td/p[@class='prog-list-title']") %>% 
            html_text,
          author = html_nodes(., xpath = "td/p[@class='prog-list-author']") %>% 
            html_text
        )
      ),
    
    ~ tibble::add_column(.y, panel = .x, .before = 1)  
    
  ) %>% 
    bind_rows
  
  # every author has an (affiliation)
  stopifnot(str_detect(h$author, "\\)"))
  
  d <- bind_rows(d, h)
  
  cat(".")
  
}

cat("\n")

# ------------------------------------------------------------------------------
# Get one author per row
# ------------------------------------------------------------------------------

d$author <- map(
  d$author, 
  ~ str_split(., "\\)") %>% # strings become "author (affil."
    unlist %>% 
    str_trim %>% 
    str_subset("\\w+") # remove final ""
) %>% 
  map(
    ~ tibble(
      author = str_replace(., "(.*?)\\s\\((.*)", "\\1"),
      affiliation = str_replace(., "(.*?)\\s\\((.*)", "\\2")
    )
  )

d <- tidyr::unnest(d) %>% 
  # clean up text
  mutate_if(
    is.character, # all columns
    ~ str_replace_all(., "\\s+", " ") %>% # remove multiple spaces
      str_replace_all("\\.$", "") %>%     # remove final '.'
      str_trim
  )

cat(
  n_distinct(d$panel), "panels,", n_distinct(d$title), "papers,",
  nrow(d), "authors,", n_distinct(d$affiliation), "affiliations.\n"
)

# quick way to get countries
d$country <- str_split(d$affiliation, ",\\s?") %>%
  # if the split returned 2+ elements, get the last one, otherwise return NA
  map2(., map(., length), ~ if_else(.y == 1, NA_character_, chuck(.x, .y))) %>% 
  unlist %>% 
  str_to_lower %>% 
  # manual corrections
  if_else(. %in% c("", "roc"), NA_character_, .) %>% 
  if_else(. %in% c("tx usa", "us"), "usa", .) %>% 
  if_else(. %in% c("taiwa"), "taiwan", .) %>% 
  if_else(. %in% c("jpn"), "japan", .) %>% 
  # finalize
  str_to_title %>% 
  if_else(. %in% c("Usa"), "USA", .)

# mostly Japan, Taiwan, Indonesia
table(d$country, exclude = NULL)

readr::write_tsv(d, "dssv2019-papers.tsv")

# ------------------------------------------------------------------------------
# Lazy way of geocoding affiliations (using geocode.xyz)
# ------------------------------------------------------------------------------

f <- "dssv2019-geocodes.tsv"
if (!file.exists(f)) {
  
  g <- select(d, affiliation) %>% 
    distinct %>% 
    tibble::add_column(lon = NA_real_, lat = NA_real_, city = NA_character_)
  
  for (i in nrow(g):1) {
    
    j <- g$affiliation[ i ]
    cat(str_pad(i, 3), j)
    
    j <- list(locate = j, geoit = "JSON")
    j <- try(httr::POST("https://geocode.xyz", body = j), silent = TRUE)
    
    if ("try-error" %in% class(j)) {
      
      cat(": network error (!!!)\n")
      
    } else if (httr::status_code(j) != 200) {
      
      cat(": error, status code", httr::status_code(j), "(!!!)\n")
      
    } else {
      
      j <- httr::content(j)
      
      if (is.null(j$error$code)) {
        
        g$lon[ i ]  <- as.numeric(j$longt)
        g$lat[ i ]  <- as.numeric(j$latt)
        g$city[ i ] <- j$standard$city
        cat(": found,", j$standard$confidence, "\n")
        
      } else {
        
        cat(": not found (!!!)\n")
        
      }
      
    }
    
    Sys.sleep(1.25)
    
  }
  
  # quick check: almost all geocoded cities are those listed in affiliations
  filter(g, !str_detect(str_to_lower(affiliation), str_to_lower(city)))
  readr::write_tsv(g, f)
  
}

g <- readr::read_tsv(f, col_types = "cddc")

# ------------------------------------------------------------------------------
# Lazy way of plotting the data on a world map
# ------------------------------------------------------------------------------

# library(rnaturalearth)
# library(rnaturalearthdata)

# world <- ne_countries(scale = "medium", returnclass = "sf")

# library(ggplot2)

# ggplot(data = world) +
#   geom_sf() + 
#   geom_point(data = left_join(d, g, by = "affiliation") %>% 
#                group_by(affiliation, lon, lat) %>% 
#                tally, aes(lon, lat, size = n), color = "red", alpha = 0.5)

# have a nice day
