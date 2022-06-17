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
