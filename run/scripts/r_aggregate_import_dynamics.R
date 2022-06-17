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
