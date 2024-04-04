#install.packages("gdata")

library(gdata)

n.cups=6 #number of cups on the platform
n.trials=2 # number of consecutive trials that need to be correct
memory.size=3 #number of cups remebered that apes chose last

max.trials=8


n.corr.dumb<-function(n.cups){#totally dumb ape
	return(length(unique(sample(n.cups, replace=T)))) ##returns number of unique cups
}

n.corr.smart<-function(n.cups){
	ires=c(rep(0, memory.size), rep(NA, n.cups))
	to.chose=1:n.cups
	lapply(X=(memory.size+1):(memory.size+n.cups), FUN=function(x){
		ires[x]<<-resample(x=setdiff(to.chose, ires[(x-memory.size):(x-1)]), size=1)
	})
	return(length(unique(ires[-(1:memory.size)])))  ##returns number of unique cups
}

#plot distribution of the choices
xx=unlist(lapply(X=rep(n.cups, 10000), FUN=n.corr.smart))
plot(table(xx))
abline(h=500, lty=3)

crit.thresh=n.cups #how many of the cups need to be correct


##distribution of number of trials until criterion
n.simus=10000
all.res=rep(NA, n.simus)
for(i in 1:n.simus){
	res=unlist(lapply(X=rep(n.cups, n.trials), FUN=n.corr.smart))
	trial.count=n.trials ##starting number of trials
	while(sum(res>=crit.thresh)<n.trials){
		res=c(res[-1], n.corr.smart(n.cups))
		trial.count=trial.count+1
	}
	all.res[i]=trial.count
	print(i)
}

#save.image("X:/R/EF battery/Updating/ape_simu_6cup.RData")

save.image("X:/R/EF battery/Updating/ape_simu_6cup_ms4.RData")
#significance threshold: number of trials until criterion
quantile(all.res, 0.05)

##plotting distribution
source("W:/Statistics/R scripts/Roger/cum_plot.r")
xx=all.res
xx[xx>500]=500
cum.plot(nvec=xx)

#p for ape achieving goal at trial 100:

func<-function(x){mean(all.res<=x)}
p.values=unlist(lapply(X=n.trials:max.trials, FUN=func))
p.values




##group level 
#
load("X:/R/EF battery/Updating/ape_simu_6cup_ms3.RData")

#number of subjects 
sub_total=6
sub_success=5
max.trials=8


n.corr.subj<-function(sub_total){ #number of subj who reach criterion within max number of trials
	return(sum(sample(all.res, sub_total, replace=F)<=max.trials))
}


n.simus2=1000
all.gres=rep(NA, n.simus2)
for(i in 1:n.simus2){
gres=unlist(n.corr.subj(sub_total))
	trial.count2=1 
	while(gres<sub_success){
	gres=unlist(n.corr.subj(sub_total))
			trial.count2=trial.count2+1
	}
	all.gres[i]=trial.count2
	print(i)
}


##plotting distribution
source("W:/Statistics/R scripts/Roger/cum_plot.r")
xx=all.gres
xx[xx>500]=500
cum.plot(nvec=xx)

#p_value
mean(all.gres<=1)




########################### when do they make mistakes in the sequence? ##########
library(gdata)

n.cups=6 #number of cups on the platform
n.trials=4 # number of consecutive trials that need to be correct
memory.size=5 #number of cups remebered that apes chose last

max.trials=8




loc.error.dumb<-function(n.cups){#totally dumb ape
	return(anyDuplicated(sample(n.cups, replace=T))) ##position of mistakes in the sequence
}

#plot distribution of the choices
xx=unlist(lapply(X=rep(n.cups, 10000), FUN=loc.error.dumb))
plot(table(xx))
abline(h=50000, lty=3)
###plot proportions
prop.table(table(xx))
plot(prop.table(table(xx)))
abline(h=0.05, lty=3)



loc.error.smart<-function(n.cups){
	ires=c(rep(0, memory.size), rep(NA, n.cups))
	to.chose=1:n.cups
	lapply(X=(memory.size+1):(memory.size+n.cups), FUN=function(x){
		ires[x]<<-resample(x=setdiff(to.chose, ires[(x-memory.size):(x-1)]), size=1)
	})
	return(anyDuplicated(ires[-(1:memory.size)])) } ##position of mistakes in the sequence

	#plot distribution of the choices
xx=unlist(lapply(X=rep(n.cups, 10000), FUN=loc.error.smart))

prop.table(table(xx))
plot(table(xx))
abline(h=500, lty=3)

###plot proportions
plot(prop.table(table(xx)))
abline(h=0.05, lty=3)

##distribution of errors in sequence

crit.thresh=6 #when the first error occurs


n.simus=1000
all.res=rep(NA, n.simus)
for(i in 1:n.simus){
	res=unlist(lapply(X=rep(n.cups, n.trials), FUN=loc.error.smart))
	trial.count=n.trials ##starting number of trials
	while(sum(res>=crit.thresh 	| res<1)<n.trials){
		res=c(res[-1], loc.error.smart(n.cups))
		trial.count=trial.count+1
	}
	all.res[i]=trial.count
	print(i)
}

#significance threshold: number of trials until criterion
quantile(all.res, 0.05)

##plotting distribution
source("W:/Statistics/R scripts/Roger/cum_plot.r")
xx=all.res
xx[xx>500]=500
cum.plot(nvec=xx)

#p for ape achieving goal at trial 100:

func<-function(x){mean(all.res<=x)}
p.values=unlist(lapply(X=n.trials:max.trials, FUN=func))
p.values







