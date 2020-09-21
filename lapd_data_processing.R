base_code_dir = "~/fasttt/"
setwd(base_code_dir)
source('constants_and_libraries.R')

# read in large dataset (2010-2020)
stops_full <- read.csv(paste0(base_input_dir, "LAPD_Portal.csv"))
stops_full <- stops_full %>%
  filter(mdy(Stop.Date) >= "2018-07-01" & mdy(Stop.Date) <= "2019-04-30") %>%
  filter(!is.na(Officer.1.Serial.Number)) %>%
  mutate(timestamp=ymd_hm(paste(mdy(Stop.Date), Stop.Time)))
stops_full <- stops_full[!duplicated(stops_full$timestamp), ] # multiple entries for multiple suspects on same stop
stops_full$timestamp <- factor(stops_full$timestamp)
stops_full$Officer.1.Serial.Number <- factor(stops_full$Officer.1.Serial.Number)

# read in small detailed dataset (July 2018-April 2019)
stops <- read.csv(paste0(base_input_dir, "RIPA_MASTER_July_April.csv"))
stops <- stops %>% dplyr::rename(suspect.race = Race,
                          frisked.bc.weapons = Search,
                          found.weapon = Contraband)
stops$suspect.race <- plyr::mapvalues(stops$suspect.race, 
                                     c('Latino', 'MiddleEastSouthAsian', 'multiracial'), 
                                     c('Hispanic', 'Other', 'Multiracial'))
stops$suspect.race <- factor(stops$suspect.race, levels = c('White', 'Black', 'Hispanic'))
stops <- stops %>% filter(!is.na(suspect.race),
                          Response.to.Call.for.Service=="N") %>%
  mutate(timestamp=ymd_hm(paste(mdy(Date.of.Stop), Time.of.Stop)))
stops$timestamp <- factor(stops$timestamp)
stops <- stops %>% filter(stops$timestamp %in% levels(stops_full$timestamp))

# precinct
stops$precinct <- plyr::mapvalues(stops$timestamp,
                                 as.character(stops_full$timestamp),
                                 as.character(stops_full$Officer.1.Division.Number))
stops$precinct <- droplevels(stops$precinct)
stops <- stops %>% filter(precinct %in% as.factor(sprintf("%02d", c(1:21))))
stops$precinct <- droplevels(stops$precinct)

# officer id
stops$officerid <- plyr::mapvalues(stops$timestamp,
                                  as.character(stops_full$timestamp),
                                  as.character(stops_full$Officer.1.Serial.Number))
stops$officerid <- droplevels(stops$officerid)

# requirements for threshold test
temp <- stops %>%
  group_by(officerid, suspect.race, .drop=FALSE) %>%
  dplyr::summarise(num_stops = n(), 
            num_searches = sum(frisked.bc.weapons)) %>%
  group_by(officerid) %>%
  dplyr::summarise(min_stops = min(num_stops),
            min_searches = min(num_searches)) %>%
  filter(min_stops > 0 & min_searches > 0)
valid_ids = levels(droplevels(temp$officerid))
stops <- stops %>%
  filter(officerid %in% valid_ids)
stops$officerid = droplevels(stops$officerid)
write.csv(stops, paste0(base_input_dir, "RIPA_MERGE_July_April.csv"))
