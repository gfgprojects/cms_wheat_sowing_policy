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


