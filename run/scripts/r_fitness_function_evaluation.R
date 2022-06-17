y.prices<-read.csv("../data/normalizedUSYearlyPrices.csv")
normalized.weighted.world.yearly.prices.real<-y.prices$us.normalized.av.price[-1]


### compute normalized price neutral

data.sessions<-read.csv("../output/output_neutral/z_sessions.csv")
#data.sessions<-read.csv("/Users/giulioni/Documents/rs_model_cms_wheat_run/output/z_sessions.csv")
data.sessions<-data.sessions[,-1]
sessions<-levels(as.factor(data.sessions$SessionDescription))

#select a country (5 for Canada, 7 for Oceania, 11 for united states)
data.session<-data.sessions[which(data.sessions$SessionDescription==sessions[11]),]
sessions.prices<-matrix(data.session$MarketPrice,ncol=1)
sessions.quantities<-data.session$QuantityExchanged
sessions.offered.quantities<-data.session$OfferedQuantity


str1<-system("grep startUsingInputsFromTimeTick= ../../rs_model/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
str2<-unlist(strsplit(str1,"="))[2]
str3<-unlist(strsplit(str2,";"))[1]
start.real.data<-as.numeric(str3)

sessions.prices<-sessions.prices[((start.real.data+1)):(start.real.data+(nrow(y.prices))*12)]


#due to change in the objective function now the following variables relates to a single country but where not renamed for compatibility with other files

wapm<-matrix(sessions.prices,ncol=12,byrow=T)
weighted.world.yearly.prices.sim<-rowMeans(wapm)
normalized.weighted.world.yearly.prices.sim<-weighted.world.yearly.prices.sim/weighted.world.yearly.prices.sim[13]


### compute normalized price nino

data.sessions<-read.csv("../output/output_nino/z_sessions.csv")
#data.sessions<-read.csv("/Users/giulioni/Documents/rs_model_cms_wheat_run/output/z_sessions.csv")
data.sessions<-data.sessions[,-1]
sessions<-levels(as.factor(data.sessions$SessionDescription))

#select a country (5 for Canada, 7 for Oceania, 11 for united states)
data.session<-data.sessions[which(data.sessions$SessionDescription==sessions[11]),]
sessions.prices<-matrix(data.session$MarketPrice,ncol=1)
sessions.quantities<-data.session$QuantityExchanged
sessions.offered.quantities<-data.session$OfferedQuantity


str1<-system("grep startUsingInputsFromTimeTick= ../../rs_model/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
str2<-unlist(strsplit(str1,"="))[2]
str3<-unlist(strsplit(str2,";"))[1]
start.real.data<-as.numeric(str3)

sessions.prices<-sessions.prices[((start.real.data+1)):(start.real.data+(nrow(y.prices))*12)]


#due to change in the objective function now the following variables relates to a single country but where not renamed for compatibility with other files

wapm<-matrix(sessions.prices,ncol=12,byrow=T)
weighted.world.yearly.prices.sim<-rowMeans(wapm)
normalized.weighted.world.yearly.prices.sim.nino<-weighted.world.yearly.prices.sim/weighted.world.yearly.prices.sim[14]



### compute normalized price nina


data.sessions<-read.csv("../output/output_nina/z_sessions.csv")
#data.sessions<-read.csv("/Users/giulioni/Documents/rs_model_cms_wheat_run/output/z_sessions.csv")
data.sessions<-data.sessions[,-1]
sessions<-levels(as.factor(data.sessions$SessionDescription))

#select a country (5 for Canada, 7 for Oceania, 11 for united states)
data.session<-data.sessions[which(data.sessions$SessionDescription==sessions[11]),]
sessions.prices<-matrix(data.session$MarketPrice,ncol=1)
sessions.quantities<-data.session$QuantityExchanged
sessions.offered.quantities<-data.session$OfferedQuantity


str1<-system("grep startUsingInputsFromTimeTick= ../../rs_model/cms_wheat/src/cms_wheat/Cms_builder.java",intern=T)
str2<-unlist(strsplit(str1,"="))[2]
str3<-unlist(strsplit(str2,";"))[1]
start.real.data<-as.numeric(str3)

sessions.prices<-sessions.prices[((start.real.data+1)):(start.real.data+(nrow(y.prices))*12)]


#due to change in the objective function now the following variables relates to a single country but where not renamed for compatibility with other files

wapm<-matrix(sessions.prices,ncol=12,byrow=T)
weighted.world.yearly.prices.sim<-rowMeans(wapm)
normalized.weighted.world.yearly.prices.sim.nina<-weighted.world.yearly.prices.sim/weighted.world.yearly.prices.sim[14]




plot(normalized.weighted.world.yearly.prices.real,type="l",xlim=c(0,25))
lines(normalized.weighted.world.yearly.prices.sim,col="green")
#lines(normalized.weighted.world.yearly.prices.sim.nino,col="red")
#lines(normalized.weighted.world.yearly.prices.sim.nina,col="blue")

