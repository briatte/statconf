
# ------------------------------------------------------------------------------
# Lazy way of geocoding affiliations (using geocode.xyz)
# ------------------------------------------------------------------------------

f <- "compstat-geocodes.tsv"
if (!file.exists(f)) {
  
  g <- select(d, country, affiliation) %>% 
    distinct %>% 
    tibble::add_column(lon = NA_real_, lat = NA_real_, city = NA_character_)
  
  for (i in nrow(g):1) {
    
    cat(str_pad(i, 3), str_c(g$affiliation[ i ], g$country[ i ], sep = ", "))
    j <- list(locate = str_c(g$affiliation[ i ], g$country[ i ], sep = ", "), geoit = "JSON")
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
