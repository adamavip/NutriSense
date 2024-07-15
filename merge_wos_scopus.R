library(tidyverse)


# Read the research outputs from WOS and Scopus
scopus<- readr::read_csv("input/biblio/scopus.csv",show_col_types = FALSE)
wos<- readxl::read_excel("input/biblio/wos.xls")

# Select the relevant variables
scopus_reduced<- scopus %>% select(c("Title","Year", "Source title",
                                     "Cited by","DOI","Abstract",
                                     "Correspondence Address", "Publisher"))
wos_reduced<- wos %>% select(c("Article Title","Publication Year", "Source Title",
                               "Times Cited, All Databases","DOI","Abstract",
                               "Reprint Addresses", "Email Addresses", "Publisher"))

# print
print(names(wos))
print(names(scopus))

# Function to extract emails
extract_emails_scopus <- function(strings) {
  email_pattern <- "email: ([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,})"
  emails <- regmatches(strings, regexpr(email_pattern, strings, perl = TRUE))
  emails <- sub("email: ", "", emails)
  return(emails)
}

# Function to extract countries
extract_countries_scopus <- function(strings) {
  country_pattern <- ",\\s+([A-Za-z]+)\\s*([A-Za-z']*)([A-Za-z]*);\\s*email:"
  countries <- regmatches(strings, regexpr(country_pattern, strings, perl = TRUE))
  countries <- sub(", \\s*", "", countries)
  countries <- sub(";\\s*email:", "", countries)
  return(countries)
}

extract_countries_wos <- function(strings) {
  country_pattern <- "\\s+([A-Za-z]+)\\s*([A-Za-z']*)([A-Za-z]*).$"
  countries <- regmatches(strings, regexpr(country_pattern, strings, perl = TRUE))
  countries <- sub(", \\s*", "", countries)
  countries <- sub("\\.\\s*", "", countries)
  return(countries)
}

# Extract emails and countries for scopus
scopus_reduced <- scopus_reduced %>% drop_na(`Correspondence Address`)
emails_scopus <- extract_emails_scopus(scopus_reduced$`Correspondence Address`)
countries_scopus <- extract_countries_scopus(scopus_reduced$`Correspondence Address`)

print(emails_scopus)

# add emails and countries to scopus
scopus_reduced["emails"]<- emails_scopus
scopus_reduced["countries"]<- countries_scopus

# Extract emails and countries for wos
countries_wos <- extract_countries_wos(wos_reduced$`Reprint Addresses`)
# add countries to wos
wos_reduced["countries"]<- countries_wos

# Data manipulation for scopus
scopus_reduced_mod <- scopus_reduced %>% 
  select(-c(`Correspondence Address`)) %>% 
  rename(
    "Source Title" = "Source title",
    "Citation Count" = "Cited by",
    "Emails" = "emails",
    "Countries" = "countries"
  )

# Data manipulation for wos
wos_reduced_mod <- wos_reduced %>% 
  select(-c(`Reprint Addresses`)) %>% 
  rename(
    "Title" = "Article Title",
    "Year" = "Publication Year",
    "Citation Count" = "Times Cited, All Databases",
    "Emails" = "Email Addresses",
    "Countries" = "countries" 
  )

# Add source column to scopus and wos
scopus_reduced_mod["Source"] = "Scopus"
wos_reduced_mod["Source"] = "WOS"

# Merge scopus and wos data
merged <- rbind(scopus_reduced_mod,wos_reduced_mod)

# Remove the duplicates
merged_final <- merged %>% distinct(DOI, .keep_all = TRUE)

# Remove whitespace from start and end of strings
merged_final$Countries <- stringr::str_trim(merged_final$Countries,side = "both")

# Homogenize country name
merged_final <- merged_final %>% mutate(
  Countries = case_when(
    Countries == "United States" ~ "USA",
    TRUE ~ Countries
  )
)
view(merged_final)

# Export final database
write.csv(merged_final, file = "input/biblio/merged_scopus_wos.csv", row.names = F)
