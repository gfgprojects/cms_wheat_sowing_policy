valutazione<-function(parameters){
#========= update simulation inputs
tmp.data<-original.input.data
#modify South America
tmp.data[2,29]<-round(tmp.data[2,29]*(1+parameters[1])) 
#modify South America
tmp.data[3,29]<-round(tmp.data[3,29]*(1+parameters[2]))
write.table(tmp.data,"tmp_file.csv",sep=",",row.names=F,quote=F)
write("","tmp_file.csv",append=T)
	
#=========== copia file di input della simulazione

system("cp tmp_file.csv ../rs_model/cms_wheat/data/producers.csv")
system("cp tmp_file.csv ../rs_model/cms_wheat/data/producers_w.csv")

#=========== cancella file output

system("rm output/z*")

#=========== lancia la simulazione

system("./run_batch")


#=========== funzione da minimizzare (code from t_cost_comparison.R)

# start code from r_source_for_comparison.R

	#read buyers data
	#outputdir settata nel file r_cost_comparison.R
data.buyers<-read.csv(paste(outputdir,"/z_buyers.csv",sep=""))
	#data.buyers<-read.csv("../output/z_buyers.csv")
data.buyers<-data.buyers[,-1]
buyers<-levels(as.factor(data.buyers$Name))

data.buyers.firstLines<-data.buyers[1:length(buyers),]
row.index<-order(unlist(data.buyers.firstLines[2]))

min.C<-data.buyers.firstLines[row.index,6]
max.C<-data.buyers.firstLines[row.index,7]

min.t<-min(unlist(data.buyers[1]))
max.t<-max(unlist(data.buyers[1]))

#read the origin file that will be used to computes exchanges 
data<-read.csv(paste(outputdir,"/z_origin.csv",sep=""))
#data<-read.csv("../output/z_origin.csv")
data<-data[,-1]
column.split<-strsplit(as.character(data[2][[1]]),":")
tmp.data<-unlist(column.split)
idx<-seq(1,length(tmp.data),by=2)
buyers.col<-tmp.data[idx]
sellers.data.str<-tmp.data[idx+1]
sellers.data<-strsplit(sellers.data.str,";")

ticks<-as.numeric(levels(as.factor(data[,1])))
buyers<-levels(as.factor(buyers.col))

#Three dimensions array: buyers, producers, time
#for each time step this is a matrix in which each row is a buyer and each column is a producer
#Not producing buyers are also included in producers, but there are zeros in their columns
#A row sum gives the total quantity bought by the country
#A column sum gives the total quantity produced by the country
imp.dyn<-array(0,dim=c(length(buyers),length(buyers),length(ticks)),dimnames=list(buyers))
for(i in 1:length(ticks)){
	tmp.idx<-which(data[,1]==ticks[i])
	tmp.buyers.col<-buyers.col[tmp.idx]
	tmp.sellers.data<-sellers.data[tmp.idx]
	tmp.imp.matrix<-matrix(0,ncol=length(buyers),nrow=length(buyers))
	for(buy in tmp.buyers.col){
		row.idx<-which(buyers==buy)
		this.buyer.data<-tmp.sellers.data[which(tmp.buyers.col==buy)]
		for(j in 1:length(this.buyer.data[[1]])){
			#	j<-2
			seller.and.q<-strsplit(this.buyer.data[[1]][j],"\\|")
			seller<-seller.and.q[[1]][1]
			quantity<-as.numeric(seller.and.q[[1]][2])
			col.idx<-which(buyers==seller)
			tmp.imp.matrix[row.idx,col.idx]<-quantity
			imp.dyn[row.idx,col.idx,i]<-quantity
		}
	}

}
dimnames(imp.dyn)[[2]]<-dimnames(imp.dyn)[[1]]


	
# end code from r_source_for_comparison.R
	
imp.dyn2<-imp.dyn
# start code from r_sim_price.R

#####compute prices from simulation

data.sessions<-read.csv(paste(outputdir,"/z_sessions.csv",sep=""))
#data.sessions<-data.sessions[,-1]
sessions<-levels(as.factor(data.sessions$SessionDescription))

session_parts<-strsplit(sessions[1]," @ ")
areas.in.prices<-unlist(session_parts)[1]
data.session<-data.sessions[which(data.sessions$SessionDescription==sessions[1]),]
sessions.prices<-data.session$MarketPrice
sessions.quantities<-data.session$QuantityExchanged
sessions.offered.quantities<-data.session$OfferedQuantity


for(i in 2:length(sessions)){
	session_parts<-strsplit(sessions[i]," @ ")
	areas.in.prices<-c(areas.in.prices,unlist(session_parts)[1])
	data.session<-data.sessions[which(data.sessions$SessionDescription==sessions[i]),]
	sessions.prices<-cbind(sessions.prices,data.session$MarketPrice)
	sessions.quantities<-cbind(sessions.quantities,data.session$QuantityExchanged)
	sessions.offered.quantities<-cbind(sessions.offered.quantities,data.session$OfferedQuantity)
}

dimnames(sessions.prices)[[2]]<-areas.in.prices

#USA prices
usa.position<-1
for(i in 2:length(sessions)){
	session_parts<-strsplit(sessions[i]," @ ")
	if(session_parts[[1]][1]=="United States of America"){
		usa.position<-i
	}
}
usa.prices<-sessions.prices[,usa.position]


# end code from r_sim_price.R

sessions.prices2<-sessions.prices



#start code from r_import_expenditures.R

imp.dyn.expenditures<-imp.dyn
imp.dyn.expenditures[,,]<-0
areas.in.imp.dyn<-unlist(dimnames(imp.dyn)[[1]])
producers.idx.in.imp.dyn<-numeric()
for(area in areas.in.prices){
producers.idx.in.imp.dyn[length(producers.idx.in.imp.dyn)+1]<-which(areas.in.imp.dyn==area)
}
buyers.idx.in.imp.dyn<-seq(1,length(areas.in.imp.dyn))[-producers.idx.in.imp.dyn]

for(j in buyers.idx.in.imp.dyn){
for(i in 1:nrow(sessions.prices)){
	imp.dyn.expenditures[j,producers.idx.in.imp.dyn,i]<-imp.dyn[j,producers.idx.in.imp.dyn,i]*sessions.prices[i,]
}
}
#end code from r_import_expenditures.R


#start code from r_aggregate_import_dynamics.R

year.for.check<-1 #if 1 include year before the change. In this year changes computed in r_compute_import_change_aggregate.R must be 0
current.tik<-24+(year.for.current.tick-year.for.check)*12+1    #year.for.current.tick is set in r_cost_comparison.R 
n.tiks.to.aggregate<-12
n.of.iterations<-floor((dim(imp.dyn)[3]-current.tik)/n.tiks.to.aggregate)

imp.dyn.cum<-array(0,dim=c(length(buyers),length(buyers),n.of.iterations),dimnames=list(buyers))
imp.dyn.expenditures.cum<-array(0,dim=c(length(buyers),length(buyers),n.of.iterations),dimnames=list(buyers))
for(i in 1:n.of.iterations){
	#	print(current.tik)
	imp.mat<-imp.dyn[,,current.tik]
	imp.mat.exp<-imp.dyn.expenditures[,,current.tik]
	for(j in 1:11){
		current.tik<-current.tik+1
		#	print(current.tik)
		imp.mat<-imp.mat+imp.dyn[,,current.tik]
		imp.mat.exp<-imp.mat.exp+imp.dyn.expenditures[,,current.tik]
	}
	for(k1 in 1:dim(imp.dyn)[1]){
		for(k2 in 1:dim(imp.dyn)[2]){
			imp.dyn.cum[k1,k2,i]<-imp.mat[k1,k2]
			imp.dyn.expenditures.cum[k1,k2,i]<-imp.mat.exp[k1,k2]
		}
	}
	current.tik<-current.tik+1
	#	print("")

}

#end code from r_aggregate_import_dynamics.R
imp.dyn.cum2<-imp.dyn.cum
imp.dyn.expenditures.cum2<-imp.dyn.expenditures.cum

# start code from r_compute_import_change_aggregate.R

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


data.producers<-read.csv("../rs_model/cms_wheat/data/producers_w.csv")
data.buyers<-read.csv("../rs_model/cms_wheat/data/buyers_Food.csv")
buyers.names<-data.buyers$Area[(nrow(data.producers)+1):nrow(data.buyers)]

buyers.position.in.imp.dyn<-numeric()
for(i in buyers.names){
	buyers.position.in.imp.dyn[length(buyers.position.in.imp.dyn)+1]<-which(dimnames(imp.dyn.cum1)[[1]]==i)
}

dimnames(imp.dyn.cum1)[[2]]<-dimnames(imp.dyn.cum1)[[1]]

#start.year<-2013
start.year<-year.neutral-year.for.check #year.neutral set in r_cost_comparison.R while year.for.check is set in r_aggregate_import_dynamics.R 
n.period<-1
#main.title<-paste("change in Q,",phase,"in 2013 and 2014")
#main.title1<-paste("change in P,",phase,"in 2013 and 2014")
#limit.in.palette<-4
#limit.in.palette1<-32

imp.aggreg.neut<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.aggreg.policy<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))

imp.expenditures.aggreg.neut<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))
imp.expenditures.aggreg.policy<-matrix(0,nrow=length(buyers.position.in.imp.dyn),ncol=dim(imp.dyn.cum1)[3],dimnames=list(buyers.names,seq(start.year,start.year+dim(imp.dyn.cum1)[3]-1)))

for(n.period in 1:(dim(imp.dyn.cum1)[3])){
	imp.table.policy<-imp.dyn.cum2[,,n.period]
	imp.table.neutral<-imp.dyn.cum1[,,n.period]
	imp.table2<-imp.table.policy[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.table1<-imp.table.neutral[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.aggreg.policy[,n.period]<-rowSums(imp.table2)
	imp.aggreg.neut[,n.period]<-rowSums(imp.table1)

	imp.expenditures.table.policy<-imp.dyn.expenditures.cum2[,,n.period]
	imp.expenditures.table.neutral<-imp.dyn.expenditures.cum1[,,n.period]
	imp.expenditures.table2<-imp.expenditures.table.policy[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.expenditures.table1<-imp.expenditures.table.neutral[buyers.position.in.imp.dyn,-buyers.position.in.imp.dyn]
	imp.expenditures.aggreg.policy[,n.period]<-rowSums(imp.expenditures.table2)
	imp.expenditures.aggreg.neut[,n.period]<-rowSums(imp.expenditures.table1)
}

av.prices.neut<-imp.expenditures.aggreg.neut/imp.aggreg.neut
av.prices.policy<-imp.expenditures.aggreg.policy/imp.aggreg.policy

imp.aggreg.change.policy<-imp.aggreg.policy-imp.aggreg.neut
imp.expenditures.aggreg.change.policy<-imp.expenditures.aggreg.policy-imp.expenditures.aggreg.neut
av.prices.change.policy<-100*(av.prices.policy-av.prices.neut)/av.prices.neut

imp.aggreg.change.policy.demand.ratio<-imp.aggreg.change.policy

for(i in 1:ncol(imp.aggreg.change.policy)){
	imp.aggreg.change.policy.demand.ratio[,i]<-100*imp.aggreg.change.policy[,i]/buyers.demand.base.year
}




# end code from r_compute_import_change_aggregate.R


sum(av.prices.change.policy[,-1]^2)

}
