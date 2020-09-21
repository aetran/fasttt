base_code_dir = "~/fasttt/"
setwd(base_code_dir)
source('constants_and_libraries.R')
source('data_processing.R')
source('run_mixture_mcmc.R')
  
# Which steps of the analysis to run.
lapd = TRUE
officer_model = TRUE
mixture_model = FALSE

remake_data = FALSE
rerun_all_mixture_frisk_models = TRUE
rerun_all_old_frisk_models = FALSE
rerun_all_mixture_stop_models = FALSE
rerun_all_old_stop_models = FALSE
rerun_placebo_models = FALSE
rerun_substratification_models = FALSE
rerun_threshold_heterogeneity = FALSE
remake_plots_and_tables = FALSE

if(lapd==TRUE){
  city='la'
} else {
  city='nyc'
}
stops_per_precinct_filename = paste0(base_input_dir, city, '_stops_per_precinct.csv')

if(remake_data){
  # this remakes all stop and frisk dataframes for the main analyses. See data_processing.R. 
  # Does not remake dataframes for robustness checks (placebo checks etc). 
  make_all_stop_and_frisk_data(lapd=lapd, officer=officer_model) 
}

# rerun all models. See threshold_mixture_model.R. 
if(rerun_all_mixture_frisk_models){
  message("Rerunning all our frisk models!")
  if(mixture_model==TRUE){
    new_frisk_model_name = 'model_het.stan'
  } else{
    new_frisk_model_name = 'model_mixture.stan'
  }
  file_prefix = paste0(city, '_stop_and_frisk_search_decision')
  run_threshold_test(file_prefix, new_frisk_model_name)
}

if(rerun_all_old_frisk_models){
  message("Rerunning all old frisk models!")
  run_threshold_test('nyc_stop_and_frisk_search_decision', old_frisk_model_name)
}

if(rerun_all_mixture_stop_models){
  message("Rerunning all mixture stop models!")
  file_prefix = paste0(city, '_stop_and_frisk')
  run_threshold_test(file_prefix, new_stop_model_name)
  # for(perturbation in white_population_perturbations){
  #   run_threshold_test(sprintf('nyc_stop_and_frisk_white_perturbation_%2.5f', perturbation), new_stop_model_name)
  # }
}

if(rerun_all_old_stop_models){
  message("Rerunning all old stop models!")
  run_threshold_test('nyc_stop_and_frisk', old_stop_model_name)
}

if(rerun_placebo_models){
  message("Rerunning placebo models!")
  source("placebo_tests.R")
  run_placebo_tests('search_decision')
}

if(rerun_substratification_models){
  message("Refitting substratification models (this is a few models, so takes a while)")
  source("substratification_robustness_checks.R")
  run_all_stratification_models()
}

if(rerun_threshold_heterogeneity){
  source('simulate_data.R')
  run_on_simulated_data(c(0, 0.05, 0.1, 0.2, 0.5, 1), 'stop_decision')
  run_on_simulated_data(c(0, 0.05, 0.1, 0.2, 0.5, 1), 'search_decision')
}

if(remake_plots_and_tables){
  source('make_plots_for_paper.R')
}