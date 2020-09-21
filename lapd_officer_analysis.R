base_code_dir = "~/fasttt/"
setwd(base_code_dir)

source('constants_and_libraries.R')
source('mixture_analysis.R')
library(abind)
library(mgcv)

# for single run of ftt
process_RData <- function(output_dir, output_name){
  file_prefix = "la_stop_and_frisk_search_decision"
  model_name = "model_mixture"
  
  nofficers = 2106
  batch_size = 100
  I = seq(1, nofficers, batch_size)
  J = I + batch_size-1
  J[length(J)] = nofficers
  
  for(x in 1:length(I)){
    i = I[x]
    j = J[x]
    
    out_name = paste0(output_dir, sprintf('%s_%s_%d-%d.RData', file_prefix, model_name, i, j))
    load(out_name)
    
    if(x==1){
      total <- obs
    } else{
      total <- total %>% rbind(obs)
    }
    obs <- total
  }
  write.csv(obs, output_name)
}

# for all runs of ftt
bootstrap_analysis <- function(nruns){
  obs = read.csv(paste0(base_output_dir, "run1.csv")) %>% arrange(location_variable)
  thresh = obs %>% select(thresholds)
  for(i in 2:nruns){
    file_name = sprintf(paste0(base_output_dir, "run%d.csv", i))
    tmp = read.csv(file_name) %>% arrange(location_variable) %>% select(thresholds)
    thresh <- abind(thresh, tmp)
  }
  thresh = rowMeans(thresh)
  obs$thresholds = thresh
  return(obs)
}

# process all days of tests
folders = list.dirs(base_output_dir, recursive = FALSE)
nruns = length(folders)
for(i in 1:nruns){
  output_dir = folders[i]
  output_name = sprintf(paste0(base_output_dir, "run%d.csv", i))
  process_RData(output_dir, output_name)
}
obs <- bootstrap_analysis(nruns)

# threshold plots
var = 'frisk threshold'
size_column = 'num_stops'
breaks = c(.1, .2, .4)
limits = c(.1, .5)
filename = '~/threshold_plot.png'
make_threshold_plot(obs, var, filename, size_column, breaks, limits, log_scale = TRUE)

##### officer features for GAM
officers = obs %>% group_by(location_variable) %>%
  summarise(nstops_ripa = sum(num_stops))

# small dataset
stops <- read.csv(paste0(base_input_dir, 'RIPA_MERGE_July_April.csv'))

# filter large dataset by officers in small dataset
officer_list = unique(stops$officerid)

# large dataset
stops_decade <- read.csv("~/LAPD_Portal.csv") %>%
  filter(Officer.1.Serial.Number %in% officer_list | Officer.2.Serial.Number %in% officer_list)

officers <- stops_decade %>%
  filter(Officer.1.Serial.Number %in% officer_list) %>%
  group_by(Officer.1.Serial.Number) %>%
  summarise(nstops_o1 = n(),
            start_year = year(min(mdy(Stop.Date))),
            service_duration = as.integer(max(mdy(Stop.Date))-min(mdy(Stop.Date)))) %>%
  ungroup()
  
officers2_inc <- stops_decade %>%
  filter(Officer.2.Serial.Number %in% officer_list) %>%
  group_by(Officer.2.Serial.Number) %>%
  summarise(nstops_o2 = n()) %>%
  ungroup() 

Officer.2.Serial.Number = setdiff(officer_list, officers2_inc$Officer.2.Serial.Number)
nstops_o2 <- Officer.2.Serial.Number * 0
officers2_exc <- data.frame(Officer.2.Serial.Number, nstops_o2)

officers2 <- officers2_inc %>%
  add_row(officers2_exc)

officers$nstops_o2 = plyr::mapvalues(officers$Officer.1.Serial.Number,
                                     officers2$Officer.2.Serial.Number,
                                     officers2$nstops_o2)

officers_temp <- stops_decade %>%
  group_by(Officer.1.Serial.Number) %>%
  filter(Officer.1.Serial.Number %in% officer_list) %>%
  summarise(nprecincts = length(unique(Officer.1.Division.Number)))

officers <- merge(officers, officers_temp, by='Officer.1.Serial.Number')
officers <- officers %>% rename(officer_id = Officer.1.Serial.Number)

officers$nstops_total = officers$nstops_o1 + officers$nstops_o2

# race specific thresholds
tmp = obs %>% filter(driver_race == 'Hispanic')
t_hispanic = tmp$thresholds
tmp <- obs %>% filter(driver_race == 'Black')
t_black = tmp$thresholds
tmp <- obs %>% filter(driver_race == 'White')
t_white = tmp$thresholds
ratio_b_w = t_black / t_white
ratio_h_w = t_hispanic / t_white

officers$target <- ratio_b_w

# GAM
gam = gam(target ~ s(service_duration) +
            s(nstops_total, service_duration) +
            s(nstops_total) +
            s(nstops_o1) +
            s(nstops_o2) +
            s(nprecincts),
          data=officers,
          method='REML')
plot(gam, residuals=TRUE, scheme=2, pages=1)