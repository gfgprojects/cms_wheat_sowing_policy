production.fao.whole.period<-read.csv("/home/giulioni/tmp/wheat_fore/cms_wheat_new_inputs/data_from_edmondo_production_fao/split_no/producersOBS_oceania_from_commodity_balance_nosplit.csv")
production.fao<-production.fao.whole.period[40:(ncol(production.fao.whole.period)-2)]
areas.names.in.ed.file<-production.fao.whole.period$Area


food<-read.csv("/home/giulioni/tmp/wheat_fore/codice/02/cms_wheat/data/buyers_Food.csv",)
feed<-read.csv("/home/giulioni/tmp/wheat_fore/codice/02/cms_wheat/data/buyers_Feed.csv",)
seed<-read.csv("/home/giulioni/tmp/wheat_fore/codice/02/cms_wheat/data/buyers_Seed.csv",)
misc<-read.csv("/home/giulioni/tmp/wheat_fore/codice/02/cms_wheat/data/buyers_Misc_tuned_1993_2016.csv",)

sim.demand<-food[,5:ncol(food)]+feed[,5:ncol(feed)]+seed[,5:ncol(seed)]+misc[,5:ncol(misc)]
#sim.demand<-sim.demand[-ncol(sim.demand)]
areas.names.in.sim.input.files<-food$Area

areas.pos<-numeric()
for(area in areas.names.in.sim.input.files){
	areas.pos[length(areas.pos)+1]<-which(production.fao.whole.period==area)
}
#cbind(areas.names.in.sim.input.files,areas.names.in.ed.file[areas.pos])

production.fao.in.sim.order<-production.fao[areas.pos,]

demand<-sim.demand
demand[13:23,]<-demand[13:23,]+production.fao.in.sim.order[13:23,]

import<-demand-production.fao.in.sim.order

import.share.of.demand<-import/demand


names(production.fao.in.sim.order)<-seq(1993,2016)
names(demand)<-seq(1993,2016)
names(import)<-seq(1993,2016)
write.table(cbind(areas.names.in.sim.input.files,demand),"sim_input_demand.csv",row.names=F,sep=",")
write.table(cbind(areas.names.in.sim.input.files,production.fao.in.sim.order),"sim_input_production.csv",row.names=F,sep=",")
write.table(cbind(areas.names.in.sim.input.files,import),"sim_input_demand_minus_production.csv",row.names=F,sep=",")



