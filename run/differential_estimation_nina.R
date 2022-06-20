library("DEoptim")

#(start code from t_cost_comparison.R)

year.neutral<-2013
year.for.current.tick<-year.neutral-1993  #used in file r_aggregate_import_dynamics.R

str1<-system("grep startUsingInputsFromTimeTick= ../rs_model/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
str2<-unlist(strsplit(str1,"="))[2]
str3<-unlist(strsplit(str2,";"))[1]
start.real.data<-as.numeric(str3)



outputdir<-"output/output_neutral"
source("scripts/r_source_for_comparison.R")
imp.dyn1<-imp.dyn
source("scripts/r_sim_prices.R")
sessions.prices1<-sessions.prices
source("scripts/r_import_expenditures.R")
source("scripts/r_aggregate_import_dynamics.R")
imp.dyn.cum1<-imp.dyn.cum
imp.dyn.expenditures.cum1<-imp.dyn.expenditures.cum

outputdir<-"output"

#(end code from t_cost_comparison.R)


original.input.data<-read.csv("../rs_model/cms_wheat/data/data_2013_nina/producers_fao_1993_2016_2013_nina.csv")

system("cp ../rs_model/cms_wheat/data/data_2013_nina/buyers_Misc_tuned_1993_2016_2013_nina.csv ../rs_model/cms_wheat/data/buyers_Misc.csv")

#system("date |mutt -s 'differential evolution iniziato' g.giulioni@gmail.com,edidigiu@gmail.com")
#system("date |mutt -s 'differential evolution iniziato' g.giulioni@gmail.com")
source("differential01_function.R")
#source("differential02_dati.R")

#par1 = production change in 
#par2 = production change in

minpar01<--0.05
minpar02<--0.05

maxpar01<-0.05
maxpar02<-0.05

minimi<-c(minpar01,minpar02)

massimi<-c(maxpar01,maxpar02)

if(F){
soluzione<-DEoptim(valutazione,minimi,massimi,control = DEoptim.control(NP=5,itermax=2))
#NP=5

system("date > stima.txt")
write("estimated parameters","stima.txt",append=T)
#for(i in 1:length(soluzione$optim$bestmem)){
#write(soluzione$optim$bestmem[i],"stima.txt",append=T)
#}
write(soluzione$optim$bestmem[1],"stima.txt",append=T)
write(soluzione$optim$bestmem[2],"stima.txt",append=T)
#write(soluzione$optim$bestmem[3],"stima.txt",append=T)
#write(soluzione$optim$bestmem[4],"stima.txt",append=T)
#write(soluzione$optim$bestmem[5],"stima.txt",append=T)
write("best value","stima.txt",append=T)
write(soluzione$optim$bestval,"stima.txt",append=T)
write("number of iterations","stima.txt",append=T)
write(soluzione$optim$iter,"stima.txt",append=T)
write("number of times the function was evaluated","stima.txt",append=T)
write(soluzione$optim$nfeval,"stima.txt",append=T)
write(soluzione$member$bestmemit,"best.txt")
}

