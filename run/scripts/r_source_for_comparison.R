
#parte di codice mossa nel file r_cost_comparison.R
if(F){
	#set the time tick in which the simulation start using real data (production and population) 
	#This serves to adjust x axis labels in plots
	str1<-system("grep startUsingInputsFromTimeTick= /home/giulioni/tmp/wheat_fore/codice/02/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
	str2<-unlist(strsplit(str1,"="))[2]
	str3<-unlist(strsplit(str2,";"))[1]
	start.real.data<-as.numeric(str3)
}

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


