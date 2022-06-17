#this script load data from sim_input_demand.csv and sim_input_demand_minus_production.csv
#if they are not in the folder,
#run r_compute_aggregate_demand_and_supply.R before to create them



data.demand<-read.csv("sim_input_demand.csv")
buyers.demand<-data.demand[13:23,]
buyers.demand<-buyers.demand[,-1]
buyers.demand.names<-names(buyers.demand)
buyers.demand.base.year<-buyers.demand[,which(buyers.demand.names==paste("X",year.neutral,sep=""))]

if(F){
	data.import.sim.input<-read.csv("sim_input_demand_minus_production.csv")
	import.sim.input<-data.import.sim.input[13:23,]
	import.sim.input<-import.sim.input[,-1]
	import.sim.input.2014<-import.sim.input$X2014
	import.sim.input.2015<-import.sim.input$X2015
	import.sim.input.2016<-import.sim.input$X2016
}


data.producers<-read.csv("../../rs_model/cms_wheat/data/producers_w.csv")
data.buyers<-read.csv("../../rs_model/cms_wheat/data/buyers_Food.csv")
buyers.names<-data.buyers$Area[(nrow(data.producers)+1):nrow(data.buyers)]

buyers.position.in.imp.dyn<-numeric()
for(i in buyers.names){
	buyers.position.in.imp.dyn[length(buyers.position.in.imp.dyn)+1]<-which(dimnames(imp.dyn.cum1)[[1]]==i)
}

dimnames(imp.dyn.cum1)[[2]]<-dimnames(imp.dyn.cum1)[[1]]

#start.year<-2013
start.year<-year.neutral-year.for.check #year.neutral set in r_cost_comparison.R while year.for.check is set in r_aggregate_import_dynamics.R 
#phase<-"nina"
n.period<-1
#main.title<-paste("change in Q,",phase,"in 2013 and 2014")
#main.title1<-paste("change in P,",phase,"in 2013 and 2014")
#limit.in.palette<-4
#limit.in.palette1<-32

imp.aggreg.neut<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.aggreg.nino<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.aggreg.nina<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))

imp.expenditures.aggreg.neut<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.expenditures.aggreg.nino<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.expenditures.aggreg.nina<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))

for(n.period in 1:(dim(imp.dyn.cum1)[3])){
	imp.table.nina<-imp.dyn.cum3[,,n.period]
	imp.table.nino<-imp.dyn.cum2[,,n.period]
	imp.table.neutral<-imp.dyn.cum1[,,n.period]
	imp.table3<-imp.table.nina[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.table2<-imp.table.nino[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.table1<-imp.table.neutral[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.aggreg.nina[,n.period]<-rowSums(imp.table3)
	imp.aggreg.nino[,n.period]<-rowSums(imp.table2)
	imp.aggreg.neut[,n.period]<-rowSums(imp.table1)

	imp.expenditures.table.nina<-imp.dyn.expenditures.cum3[,,n.period]
	imp.expenditures.table.nino<-imp.dyn.expenditures.cum2[,,n.period]
	imp.expenditures.table.neutral<-imp.dyn.expenditures.cum1[,,n.period]
	imp.expenditures.table3<-imp.expenditures.table.nina[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.expenditures.table2<-imp.expenditures.table.nino[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.expenditures.table1<-imp.expenditures.table.neutral[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.expenditures.aggreg.nina[,n.period]<-rowSums(imp.expenditures.table3)
	imp.expenditures.aggreg.nino[,n.period]<-rowSums(imp.expenditures.table2)
	imp.expenditures.aggreg.neut[,n.period]<-rowSums(imp.expenditures.table1)
}

av.prices.neut<-imp.expenditures.aggreg.neut/imp.aggreg.neut
av.prices.nino<-imp.expenditures.aggreg.nino/imp.aggreg.nino
av.prices.nina<-imp.expenditures.aggreg.nina/imp.aggreg.nina

imp.aggreg.change.nino<-imp.aggreg.nino-imp.aggreg.neut
imp.aggreg.change.nina<-imp.aggreg.nina-imp.aggreg.neut
imp.expenditures.aggreg.change.nino<-imp.expenditures.aggreg.nino-imp.expenditures.aggreg.neut
imp.expenditures.aggreg.change.nina<-imp.expenditures.aggreg.nina-imp.expenditures.aggreg.neut
av.prices.change.nino<-100*(av.prices.nino-av.prices.neut)/av.prices.neut
av.prices.change.nina<-100*(av.prices.nina-av.prices.neut)/av.prices.neut

imp.aggreg.change.nino.demand.ratio<-imp.aggreg.change.nino
imp.aggreg.change.nina.demand.ratio<-imp.aggreg.change.nina

for(i in 1:ncol(imp.aggreg.change.nino)){
	imp.aggreg.change.nino.demand.ratio[,i]<-100*imp.aggreg.change.nino[,i]/buyers.demand.base.year
	imp.aggreg.change.nina.demand.ratio[,i]<-100*imp.aggreg.change.nina[,i]/buyers.demand.base.year
}

write.table(av.prices.change.nino,file=paste("change_price_nino",year.neutral,".csv",sep=""),sep=",",quote=F,col.names=NA)
write.table(av.prices.change.nina,file=paste("change_price_nina",year.neutral,".csv",sep=""),sep=",",quote=F,col.names=NA)
write.table(imp.aggreg.change.nino.demand.ratio,file=paste("change_import_nino",year.neutral,".csv",sep=""),sep=",",quote=F,col.names=NA)
write.table(imp.aggreg.change.nina.demand.ratio,file=paste("change_import_nina",year.neutral,".csv",sep=""),sep=",",quote=F,col.names=NA)


