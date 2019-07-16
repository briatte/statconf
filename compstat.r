
library(dplyr)
library(purrr)
library(readr)
library(robotstxt)
library(rvest)
library(stringr)
library(tidyr) # `unnest`

for (y in c(2016, 2018)) {

  d <- str_c("compstat", y)
  dir.create(d, showWarnings = FALSE)
  
  h <- str_c("http://www.", d, ".org/fullprogramme.php")
  stopifnot(robotstxt::paths_allowed(h))
  
  h <- read_html(h)
  
  # # panel titles
  # html_nodes(h, xpath = "//table/tr[1]/td[1]")
  
  # paper titles
  t <- html_nodes(h, xpath = "//td/a[contains(@href, 'cmstatistics.org')]") %>%
    html_text
  
  # paper URLs
  j <- html_nodes(h, xpath = "//td/a[contains(@href, 'cmstatistics.org')]") %>% 
    html_attr("href")
  
  cat(d, ": ")
  for (i in rev(j)) {
    
    cat(which(i == j))
    
    f <- httr::parse_url(i)$query$`in` # abstract id
    f <- file.path(d, str_c(f, ".html"))
    
    if (!file.exists(f)) {
      try(download.file(i, f, mode = "wb", quiet = TRUE), silent = TRUE)
    }
    if (file.exists(f)) {
      cat("...")
    } else {
      cat("(error)")
    }
    
  }
  cat("\n")
 
  # save titles and authors (skipping abstracts, //span[3])
  d <- full_join(
    tibble::tibble(
      uid = map_chr(j, ~ httr::parse_url(.)$query$`in`),
      title = t
    ),
    list.files(d, full.names = TRUE) %>% 
      map_dfr(
        ~ tibble::tibble(
          uid = str_replace_all(basename(.), "\\D", ""),
          author = read_html(.) %>% 
            html_node(xpath = "//div[@class='container']//span[2]") %>%
            html_text
        )
      ),
    by = "uid"
  ) %>% 
    readr::write_tsv(str_c("compstat", y, ".tsv"))
  
}

f <- list.files(pattern = "compstat\\d{4}\\.tsv")
d <- map_dfr(
  f,
  ~ readr::read_tsv(.x, col_type = "icc") %>% 
    tibble::add_column(
      year = str_replace_all(.x, "\\D", "") %>% 
        as.integer,
      .before = 1
    )
)

file.remove(f)
print(table(d$year))

# detect non-countries in bracketed strings...
unlist(str_extract_all(d$author, "\\([A-Z]+\\)"))
# ... and remove them to avoid confusing them with countries
d$author <- str_replace_all(d$author, "\\((FIUS|AMU|CNR|ELSTAT|JRC)\\)", "")
# one more special case, detected by checking `country` later
d$author <- str_replace_all(d$author, "\\(The graduate university for advanced studies\\)", "")

# almost all presentations have a presenter
d$author[ !str_detect(d$author, "\\) \\[presenting\\]") ]

# move "[presenter] before (country) part
d$author <- str_replace(d$author, "(.*?)\\((.*?)\\)\\s\\[presenting\\](.*)", "\\1[p] (\\2)\\3")
d$author <- map(
  d$author,
  ~ str_split(., "\\)") %>% 
    unlist %>% 
    str_subset("\\w+") # remove final empty ""
)

d <- tidyr::unnest(d) %>%
  mutate(
    presenter = as.integer(str_detect(author, "\\[p\\]")),
    author = str_replace(author, "\\s+\\[p\\]", ""), # clean up
    country = str_extract(author, "\\((.*)") %>% 
      str_replace("^\\(", ""),
    author = str_replace(author, "\\s+\\((.*)", ""), # clean up
    affiliation = str_replace(author, "(.*?)\\s-\\s(.*)", "\\2"),
    author = str_replace(author, "(.*?)\\s-\\s(.*)", "\\1") # clean up
  )

readr::write_tsv(d, "compstat-papers.tsv")

# have a nice day
