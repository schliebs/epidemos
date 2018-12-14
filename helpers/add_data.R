library(tidyverse)

# CRS Codes
crs <- readxl::read_excel("helpers/offline/sectors_csv_ms.xlsx")
devtools::use_data(crs,overwrite = TRUE)

## UN Voting Patterns

#install.packages("unvotes")

un <- unvotes::un_votes

names(un)

info <- unvotes::un_roll_calls

unwide <- 
  un %>% 
  mutate_at(vars(country),
            funs(. %>% aidR::unifyCountrynames())) %>% 
  select(-country_code) %>% 
  spread(country,vote) %>% 
  rename(`United States` = `United States of America`,
         `United Kingdom` = `United Kingdom of Great Britain and Northern Ireland`) %>% 
  select(rcid,one_of(aidR::donorCountries()))

match <- 
  left_join(un %>% 
              mutate_at(vars(country),
                        funs(. %>% aidR::unifyCountrynames())) %>% 
              filter(country %in% aidR::subsaharaCountries()),
            unwide,
            by = c("rcid")) %>% 
  left_join(.,
            info %>% select(rcid,date),
            by = "rcid") %>% 
  mutate_at(vars(date),
            funs(year = lubridate::year(as.Date(.))))


match2 <- 
  match %>% 
  mutate_at(vars(one_of(aidR::donorCountries())),
            funs(ifelse(. == vote,1,0)))

match2$country %>% table()

match3 <- 
  match2 %>% 
  group_by(country,year) %>% 
  summarise_at(vars(one_of(aidR::donorCountries())),
               funs(mean(.,na.rm = T))) 

match4 <- 
  match3 %>% 
  mutate_at(vars(one_of(aidR::donorCountries())),
            funs(zoo::rollapply(., width=3, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")))

unmatch = match4

devtools::use_data(unmatch)
devtools::use_data(unvotes)


## Security Council Membership


