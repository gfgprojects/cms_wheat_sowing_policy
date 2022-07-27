#change the outputdirs variables in r_cost_comparison.R

source("r_cost_comparison.R")
diff.prices.changes<-av.prices.change.nina-av.prices.change.nino
G_p_phase<-sum(av.prices.change.nino^2)
G_p_phase_plus_policy<-sum(av.prices.change.nina^2)
print(paste("G_p_phase",round(G_p_phase,digits=2)))
print(paste("G_p_phase_plus_policy",round(G_p_phase_plus_policy,digits=2)))
print(round(diff.prices.changes[,-1],digits=2))

diff.quantities.changes<-imp.aggreg.change.nina.demand.ratio-imp.aggreg.change.nino.demand.ratio
G_WA_phase<-sum(imp.aggreg.change.nino.demand.ratio^2)
G_WA_phase_plus_policy<-sum(imp.aggreg.change.nina.demand.ratio^2)
print(paste("G_WA_phase",round(G_WA_phase,digits=3)))
print(paste("G_WA_phase_plus_policy",round(G_WA_phase_plus_policy,digits=3)))
print(round(diff.quantities.changes[,-1],digits=3))
#print(diff.quantities.changes[,-1])

