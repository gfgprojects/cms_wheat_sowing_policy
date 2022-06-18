#This script computes the dynamics of exchanges (imp.dyn) and prices (sessions.prices)
# imp.dyn has three dimensions [importer,exporter, time]
# type dimnames(imp.dyn) to see the order of areas
# sessions.prices has two dimensions [time,exporter]
# type dimnames(sessions.prices) to see the order of areas 


#set the time tick in which the simulation start using real data (production and population) 
#This serves to adjust x axis labels in plots

#years of Neutral
#1994-7;2001-7;2009-10;2013-14
year.neutral<-2013
#str.for.unzip<-paste("unzip -o /home/giulioni/Documenti/models/cms_wheat_forecast/output/",year.neutral,".zip -d /home/giulioni/Documenti/models/cms_wheat_forecast/output/",sep="")
#system(str.for.unzip)

year.for.current.tick<-year.neutral-1993  #used in file r_aggregate_import_dynamics.R

str1<-system("grep startUsingInputsFromTimeTick= ../../rs_model/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
str2<-unlist(strsplit(str1,"="))[2]
str3<-unlist(strsplit(str2,";"))[1]
start.real.data<-as.numeric(str3)

#outputdir<-"../output_p_fao_d_adjusted"
outputdir<-"../output/output_neutral"
source("r_source_for_comparison.R")
imp.dyn1<-imp.dyn
source("r_sim_prices.R")
sessions.prices1<-sessions.prices
source("r_import_expenditures.R")
source("r_aggregate_import_dynamics.R")
imp.dyn.cum1<-imp.dyn.cum
imp.dyn.expenditures.cum1<-imp.dyn.expenditures.cum

outputdir<-"../output"
source("r_source_for_comparison.R")
imp.dyn2<-imp.dyn
source("r_sim_prices.R")
sessions.prices2<-sessions.prices
source("r_import_expenditures.R")
source("r_aggregate_import_dynamics.R")
imp.dyn.cum2<-imp.dyn.cum
imp.dyn.expenditures.cum2<-imp.dyn.expenditures.cum

source("r_compute_import_change_aggregate.R")



