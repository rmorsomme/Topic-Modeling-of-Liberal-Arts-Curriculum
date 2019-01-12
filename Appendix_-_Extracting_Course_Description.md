Appendix - Extracting Course Description
================
Raphaël Morsomme
2019-01-12

We extract the description of each course from the course catalogues. In the `Setup`, we create several objects that we use in the following two loops. We first loop (`Loop 1`) through each course catalogue present in `corpus`. In the loop, we first identify the section of the catalogue containing the course descriptions and assign it to `cat_description`. We then exclude from `cat_descriptions` the headers, appendix, etc, so that it only contains course descriptions. We take advantage of the fact that the the first line of a course description always starts with the code of the course, meaning that the first three characters of the first page of a description are one of the following COR, HUM, SCI, SSC, SKI, PRO, UGR, CAP. Using `grep()`, we can therefore easily identify the first page of each description which we assign to `first_pages_description`.

We then loop (`Loop 2`) through the first pages of the course descriptions. In the loop, we check if the description is one- or two-page long: if the following page is the first page of a description, then the description is only one page long; otherwise, the description is two page long and consists of the current and the following page\[^7\]. Once we have a course description, we use `substring(first = 1, last = 7)` to extract the code of the course (3 letters and 4 numbers) and save the description, with its code and the year of the catalogue in the tibble `d_description`.

``` r
library(tm)
library(tidyverse)

# Import data
corpus <- Corpus(x             = DirSource("Catalogues"),
                 readerControl = list(reader = readPDF(control = list(text = "-layout"))))

# Setup
n_catalogue    <- length(corpus)
calendar_years <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")
d_description  <- tibble(Code            = character(0),
                         `Calendar Year` = character(0),
                         Description     = character(0))

course_code <- c("COR", "HUM", "SCI", "SSC", "SKI", "PRO", "UGR", "CAP")
course_code <- paste(course_code, collapse = "|")

content_to_exclude <- c(
  "^Core Courses \\(COR\\)",
  "^Humanities \\(HUM\\)",
  "^Sciences \\(SCI\\)",
  "^Social Sciences \\(SSC\\)",
  "^Skills Trainings \\(SKI\\)", "^Skills Training \\(SKI\\)",
  "^Project \\(PRO\\)", "^Projects \\(PRO\\)",
  "^Undergraduate Research \\(UGR\\)",
  " UCM Undergraduate\r\nResearch", "UCM Undergraduate\r\n  Research"
  )

content_to_exclude <- paste(content_to_exclude, collapse = "|")

# Loop 1
for(n in 1 : n_catalogue){
  
  cat <- content(corpus[[n]])
  
  # Description Section
  page_description_start <- grep(pattern = "^Core Courses \\(COR\\)", x = cat) + 1
  page_description_end   <- grep(pattern = "Appendix"               , x = cat) - 1
    if(length(page_description_end) == 0) page_description_end <- length(cat)
  cat_description <- cat[page_description_start : page_description_end]
  
  # Course Descriptions Only
  pages_to_exclude <- grep(pattern = content_to_exclude, x = cat_description)
  cat_description  <- cat_description[!1:length(cat_description) %in% pages_to_exclude]
  
  # First Pages of Course Descriptions
  first_three_letters     <- substr(cat_description, start = 1, stop = 3)
  first_pages_description <- grep(pattern = course_code, x = first_three_letters)

  # Loop 2
  for(page in first_pages_description){
    
    # Extract Description
    following_page <- page + 1 # for convenience
    description <- if(following_page %in% first_pages_description) cat_description[page]
                   else                                            paste(cat_description[page : following_page], collapse = " ")
    
    # Save Description
    Code <- substring(description, first = 1, last = 7)      
    year <- calendar_years[n]
    d_description <- d_description %>%
      add_row(Code            = Code,
              `Calendar Year` = year,
              Description     = description)
    
  } # close for-loop 2 (page)
} # close for-loop 1 (n)

print(d_description)
```

    ## # A tibble: 831 x 3
    ##    Code    `Calendar Year` Description                                    
    ##    <chr>   <chr>           <chr>                                          
    ##  1 COR1002 2014-2015       "COR1002 - Philosophy of Science\r\nCourse coo~
    ##  2 COR1003 2014-2015       "COR1003 - Contemporary World History\r\nCours~
    ##  3 COR1004 2014-2015       "COR1004 - Political Philosophy\r\nCourse coor~
    ##  4 COR1005 2014-2015       "COR1005 - Modeling Nature\r\nCourse coordinat~
    ##  5 HUM1003 2014-2015       "HUM1003 - Cultural Studies I: Doing Cultural ~
    ##  6 HUM1007 2014-2015       "HUM1007 - Introduction to Philosophy\r\nCours~
    ##  7 HUM1010 2014-2015       "HUM1010 - Common Foundations of Law in Europe~
    ##  8 HUM1011 2014-2015       "HUM1011 - Introduction to Art; Representation~
    ##  9 HUM1012 2014-2015       "HUM1012 - Pop Songs and Poetry: Theory and An~
    ## 10 HUM1013 2014-2015       "HUM1013 - The Idea of Europe: The Intellectua~
    ## # ... with 821 more rows
