cimp<-read.csv("../change_import_nino2013.csv")
names(cimp)<-c("Area","2012","2013","2014","2013","2014")
cpri<-read.csv("../change_price_nino2013.csv")
names(cpri)<-c("Area","2012","2013","2014","2013","2014")
effects.nino<-read.csv("effects_nino.csv")
names(effects.nino)<-c("Area","ninoeffect")
effects.nina<-read.csv("effects_nina.csv")
names(effects.nina)<-c("Area","ninaeffect")

#gathering months
#Eastern Africa  10
#Northern Africa 7
#Central America 5
#Southern Asia 6

rows.effects.nino<-c(1,3,6,8)
rows.effects.nina<-c(3,8)

weighted.effects.nino<-matrix(0,nrow=11,ncol=2)
weighted.effects.nino[1,1]<-effects.nino[1,2]*(12-10)/12
weighted.effects.nino[1,2]<-effects.nino[1,2]*(10)/12
weighted.effects.nino[3,1]<-effects.nino[3,2]*(12-7)/12
weighted.effects.nino[3,2]<-effects.nino[3,2]*(7)/12
weighted.effects.nino[6,1]<-effects.nino[6,2]*(12-5)/12
weighted.effects.nino[6,2]<-effects.nino[6,2]*(5)/12
weighted.effects.nino[8,1]<-effects.nino[8,2]*(12-6)/12
weighted.effects.nino[8,2]<-effects.nino[8,2]*(6)/12


net.cimp<-cimp
net.cimp[,3]<-round(cimp[,3]+weighted.effects.nino[,1],digits=2)
net.cimp[,4]<-round(cimp[,4]+weighted.effects.nino[,2],digits=2)

filename<-paste("mpinput_areas_effects.mp",sep="")
write(paste("area1:=\"",effects.nino[1,1],"\";",sep=""),filename)
write(paste("o1:=",effects.nino[1,2],";",sep=""),filename,append=T)
write(paste("a1:=",effects.nina[1,2],";",sep=""),filename,append=T)
ratiotw<-round(effects.nino[1,2]/3,digits=2)
if(ratiotw>1){ratiotw<-1}
if(ratiotw<(-1)){ratiotw<-(-1)}
write(paste("ro1:=",ratiotw,";",sep=""),filename,append=T)
ratiotw<-round(effects.nina[1,2]/3,digits=2)
if(ratiotw>1){ratiotw<-1}
if(ratiotw<(-1)){ratiotw<-(-1)}
write(paste("ra1:=",ratiotw,";",sep=""),filename,append=T)

write(paste("fo1:=",round(weighted.effects.nino[1,1],digit=2),";",sep=""),filename,append=T)
write(paste("so1:=",round(weighted.effects.nino[1,2],digit=2),";",sep=""),filename,append=T)

for(j in 2:11){
write(paste("area",j,":=\"",effects.nino[j,1],"\";",sep=""),filename,append=T)
write(paste("o",j,":=",effects.nino[j,2],";",sep=""),filename,append=T)
write(paste("a",j,":=",effects.nina[j,2],";",sep=""),filename,append=T)
ratiotw<-round(effects.nino[j,2]/3,digits=2)
if(ratiotw>1){ratiotw<-1}
if(ratiotw<(-1)){ratiotw<-(-1)}
write(paste("ro",j,":=",ratiotw,";",sep=""),filename,append=T)
ratiotw<-round(effects.nina[j,2]/3,digits=2)
if(ratiotw>1){ratiotw<-1}
if(ratiotw<(-1)){ratiotw<-(-1)}
write(paste("ra",j,":=",ratiotw,";",sep=""),filename,append=T)
write(paste("fo",j,":=",round(weighted.effects.nino[j,1],digit=2),";",sep=""),filename,append=T)
write(paste("so",j,":=",round(weighted.effects.nino[j,2],digit=2),";",sep=""),filename,append=T)
}

for(i in 3:6){
	filename<-paste("mpinput",i-2,"nino.mp",sep="")
	write("",filename)
	for(j in 1:11){
		ratiotw<-cpri[j,i]/12
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rpf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("pf",j,":=",round(cpri[j,i],digits=2),";",sep=""),filename,append=T)
		ratiotw<-net.cimp[j,i]/3
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rqf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("qf",j,":=",round(net.cimp[j,i],digits=2),";",sep=""),filename,append=T)

		ratiotw<-cimp[j,i]/3
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rcf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("cf",j,":=",round(cimp[j,i],digits=2),";",sep=""),filename,append=T)
	
	}
}

cimp<-read.csv("../change_import_nina2013.csv")
names(cimp)<-c("Area","2012","2013","2014","2013","2014")
cpri<-read.csv("../change_price_nina2013.csv")
names(cpri)<-c("Area","2012","2013","2014","2013","2014")

weighted.effects.nina<-matrix(0,nrow=11,ncol=2)
weighted.effects.nina[3,1]<-effects.nina[3,2]*(12-7)/12
weighted.effects.nina[3,2]<-effects.nina[3,2]*(7)/12
weighted.effects.nina[8,1]<-effects.nina[8,2]*(12-7)/12
weighted.effects.nina[8,2]<-effects.nina[8,2]*(7)/12

net.cimp<-cimp
net.cimp[,3]<-cimp[,3]+weighted.effects.nina[,1]
net.cimp[,4]<-cimp[,4]+weighted.effects.nina[,2]

write(paste("fa1:=",round(weighted.effects.nina[1,1],digit=2),";",sep=""),filename,append=T)
write(paste("sa1:=",round(weighted.effects.nina[1,2],digit=2),";",sep=""),filename,append=T)
for(j in 2:11){
write(paste("fa",j,":=",round(weighted.effects.nina[j,1],digit=2),";",sep=""),filename,append=T)
write(paste("sa",j,":=",round(weighted.effects.nina[j,2],digit=2),";",sep=""),filename,append=T)
}


for(i in 3:6){
	filename<-paste("mpinput",i-2,"nina.mp",sep="")
	write("",filename)
	for(j in 1:11){
		ratiotw<-cpri[j,i]/12
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rpf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("pf",j,":=",round(cpri[j,i],digits=2),";",sep=""),filename,append=T)
		ratiotw<-net.cimp[j,i]/3
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rqf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("qf",j,":=",round(net.cimp[j,i],digits=2),";",sep=""),filename,append=T)

		ratiotw<-cimp[j,i]/3
		if(ratiotw>1){ratiotw<-1}
		if(ratiotw<(-1)){ratiotw<-(-1)}
		write(paste("rcf",j,":=",round(ratiotw,digits=2),";",sep=""),filename,append=T)
		write(paste("cf",j,":=",round(cimp[j,i],digits=2),";",sep=""),filename,append=T)
	
	}
}

