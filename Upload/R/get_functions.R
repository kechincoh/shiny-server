#function to source the required functions

get_functions <- function()
{
  ## main functions ##
  source('./R/convert_date.R')        #function to convert to date type
  source('./R/check_nodb.R')
  source('./R/calculate.R')           #function to calculate the number of days to progression;otherwise,number of days to last contact date
  source('./R/extract_col.R')         #function to extract desired columns
  source('./R/latest.R') 
  source('./R/check_date_format.R')   #function to check if the date columns are in correct format
  source('./R/comb_columns.R')        #function to combine progression and relapse used in check_prog_relapsed.R
  source('./R/check_prog_relapsed.R')
  source('./R/empty_as_na.R')         #function to fill blanks with "NA" 
  
  ## plot functions ##
  source('./R/plot1.R')
  source('./R/plot2.R')
  source('./R/plot3.R')
  source('./R/plot4.R')
  source('./R/plot_overall.R')
  source('./R/plot_overall_prog.R')
  source('./R/plot_overall_best.R')
  source('./R/plot_overall_vital.R')
  
  ## Monitor functions ##
  source('./R/Report_13401nodb.R') 
  source('./R/Report_13272nodb.R')
  source('./R/Report_13277nodb.R')
  source('./R/Report_13351nodb.R')
  source('./R/Report_13384nodb.R')
  source('./R/Report_13447nodb.R')
  source('./R/Report_14036nodb.R')
  source('./R/Report_14108nodb.R')
  source('./R/Report_15342nodb.R')
  
  ## Death functions ##
  source('./R/Report_death_13401_nodb.R')
  source('./R/Report_death_13272_nodb.R')
  source('./R/Report_death_13277_nodb.R')
  source('./R/Report_death_13351_nodb.R')
  source('./R/Report_death_13384_nodb.R')
  source('./R/Report_death_13447_nodb.R')
  source('./R/Report_death_14036_nodb.R')
  source('./R/Report_death_14108_nodb.R')
  source('./R/Report_death_15342_nodb.R')
  
  ## Demo functions ##
  source('./R/Report_demo_13401_nodb.R')
  source('./R/Report_demo_13272_nodb.R')
  source('./R/Report_demo_13277_nodb.R')
  source('./R/Report_demo_13351_nodb.R')
  source('./R/Report_demo_13384_nodb.R')
  source('./R/Report_demo_13447_nodb.R')
  source('./R/Report_demo_14036_nodb.R')
  source('./R/Report_demo_14108_nodb.R')
  source('./R/Report_demo_15342_nodb.R')
}