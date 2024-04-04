library(lme4)

all.data=read.table(file="C:/Users/cjv3/R/EF battery/Updating_exp1/Updating_exp1_fs_data final.txt", header=T, sep="\t")

test.data.prep=data.frame(all.data[rep(1:nrow(all.data), each=6), c("Subject", "Sex", "Species", "Age", "Breeding", "Order", "Phase", "Number_boxes", "Session_within_phase", "Session_overall", "Trial_number")], 
	trial.id=rep(1:nrow(all.data), each=6),
	choice.nr.within.trial=rep(1:6, nrow(all.data)), 
	choice.where=as.vector(unlist(c(t(all.data[, c("first_choice", "second_choice", "third_choice", "fourth_choice", "fifth_choice", "sixth_choice")])))),
	choice.correct=as.vector(unlist(c(t(cbind(NA, all.data[, c("second_choice_correct", "third_choice_correct", "fourth_choice_correct", "fifth_choice_correct", "sixth_choice_correct")]))))))
u.trial.id=unique(test.data.prep$trial.id)
last.choice=rep(NA, nrow(test.data.prep))
previous.correct=rep(NA, nrow(test.data.prep))
any.error.earlier=rep(NA, nrow(test.data.prep))
for(i in 1:length(u.trial.id)){
	sel.data=subset(test.data.prep, trial.id==u.trial.id[i])
	#if(length(unique(sel.data$choice.where[!is.na(sel.data$choice.where)]))<sum(!is.na(sel.data$choice.where)))
	last.choice[test.data.prep$trial.id==u.trial.id[i]][2:6]=unlist(lapply(2:nrow(sel.data), function(x){
		ires=NA
		if(!is.na(sel.data$choice.where[x])){
			xx=which(sel.data$choice.where[1:(x-1)]==sel.data$choice.where[x])
			if(length(xx)>0){
				ires=max(xx)
			}
		}
		return(ires)
	}))
	previous.correct[test.data.prep$trial.id==u.trial.id[i]]=c(NA, 1, sel.data$choice.correct[2:5])
	xx=c(1, sel.data$choice.correct[1:5])
	xx[is.na(xx)]=1
	any.error.earlier[test.data.prep$trial.id==u.trial.id[i]]=c("no", "yes")[1+cummax(xx==0)]
}
test.data.prep$last.choice=last.choice
test.data.prep$previous.correct=previous.correct
test.data.prep$any.error.earlier=any.error.earlier
test.data=subset(test.data.prep, choice.nr.within.trial>1 & !is.na(choice.where))



##fragment left from another approach including all cups that could be chosen:
test.data.prep=subset(test.data.prep, !is.na(choice.where))# 	for(j in 2:nrow(sel.data)){
test.data=c()
choice.id=0
for(i in 1:length(u.trial.id)){
	sel.data=subset(test.data.prep, trial.id==u.trial.id[i])
	if(nrow(sel.data)==2){
		cup.id=3:4
	}else if(nrow(sel.data)==3){
		cup.id=3:5
	}else if(nrow(sel.data)==4){
		cup.id=2:5
	}else if(nrow(sel.data)==5){
		cup.id=2:6
	}else{
		cup.id=1:6
	}
	for(j in 2:nrow(sel.data)){
		choice.id=choice.id+1
		idata=data.frame(sel.data[j, ], cup.id=cup.id, choice.id=choice.id)
		idata$last.choice=unlist(lapply(1:nrow(idata), function(x){
			xx=which(sel.data$choice.where[1:(j-1)]==idata$cup.id[x])
			if(length(xx)>0){
				return(max(xx))
			}else{
				return(NA)
			}
		}))
		if(j>2){
			idata$previous.correct=sel.data$choice.correct[j-1]
		}else{
			idata$previous.correct=1
		}
		idata$any.error.earlier=as.numeric(sum(sel.data$choice.correct[1:(j-1)]==0, na.rm=T)>0)
		test.data=rbind(test.data, idata)
	}
}	
test.data=subset(test.data, !is.na(last.choice))
error.ny=as.numeric(test.data$choice.correct==0 & test.data$choice.where==test.data$cup.id)
xx=tapply(error.ny, test.data$choice.id, sum)
xx=xx[xx>0]
lag.from.last.choice=test.data$choice.nr.within.trial-test.data$last.choice
test.data$previous.correct=as.factor(c("no", "yes")[1+as.numeric(test.data$previous.correct)])
test.data$any.error.earlier=as.factor(test.data$any.error.earlier)
number.cups=as.vector(table(test.data$choice.id)[as.character(test.data$choice.id)])
table(number.cups, test.data$Number_boxes)
cup.is.edge=
	(test.data$Number_boxes==2 & (test.data$cup.id==3 | test.data$cup.id==4)) |
	(test.data$Number_boxes==3 & (test.data$cup.id==3 | test.data$cup.id==5)) |
	(test.data$Number_boxes==4 & (test.data$cup.id==2 | test.data$cup.id==5)) |
	(test.data$Number_boxes==5 & (test.data$cup.id==2 | test.data$cup.id==6)) |
	(test.data$Number_boxes==6 & (test.data$cup.id==1 | test.data$cup.id==6))
test.data$cup.is.edge=as.factor(c("no", "yes")[1+as.numeric(cup.is.edge)])
cup.is.edge.code=as.numeric(cup.is.edge==levels(cup.is.edge)[2])
cup.is.edge.code=cup.is.edge.code-mean(cup.is.edge.code)
source("C:/Users/cjv3/R/R scripts/Roger/diagnostic_fcns.r")
source("C:/Users/cjv3/R/R scripts/Roger/helpers.r")
xx.fe.re=fe.re.tab(fe.model="error.ny~lag.from.last.choice+previous.correct+any.error.earlier+cup.is.edge+Number_boxes+Sex+Age+Trial_number", re="(1|Subject)+(1|choice.id)+(1|trial.id)", other.vars="number.cups", 
	data=data.frame(test.data, error.ny, lag.from.last.choice, number.cups, cup.is.edge))
xx.fe.re$summary
test.data$lag.from.last.choice=lag.from.last.choice
z.lag.from.last.choice=as.vector(scale(lag.from.last.choice))
previous.correct.code=as.numeric(test.data$previous.correct==levels(test.data$previous.correct)[2])
previous.correct.code=previous.correct.code-mean(previous.correct.code)
any.error.earlier.code=as.numeric(test.data$any.error.earlier==levels(test.data$any.error.earlier)[2])
any.error.earlier.code=any.error.earlier.code-mean(any.error.earlier.code)
z.Number_boxes=as.vector(scale(test.data$Number_boxes))
z.Trial_number=as.vector(scale(test.data$Trial_number))
z.age=as.vector(scale(test.data$Age))

contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))
full=glmer(error.ny~z.lag.from.last.choice+previous.correct+any.error.earlier+z.Number_boxes+cup.is.edge+Sex+z.age+z.Trial_number+offset(log((1/number.cups)))+
	(1+z.lag.from.last.choice+previous.correct.code+any.error.earlier.code+z.Number_boxes+z.Trial_number+cup.is.edge.code||Subject)+
	(1|choice.id)+(1+cup.is.edge.code||trial.id),
	family=binomial, data=test.data, control=contr)
ranef.diagn.plot(full)
null=glmer(error.ny~Sex+z.Trial_number+offset(log((1/number.cups)))+
	(1+z.lag.from.last.choice+previous.correct.code+any.error.earlier.code+z.Number_boxes+z.Trial_number+cup.is.edge.code||Subject)+
	(1|choice.id)+(1+cup.is.edge.code||trial.id),
	family=binomial, data=test.data, control=contr)
wt.txt(c.tab(anova(null, full, test="Chisq"), 3))
#          Df     AIC     BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)
# null 13.000 864.812 934.175 -419.406  838.812     NA     NA         NA
# full 19.000 849.261 950.638 -405.631  811.261 27.551  6.000      0.000
tests=as.data.frame(drop1(full, test="Chisq"))
wt.txt(c.tab(tests, 3))
#                           Df     AIC    LRT Pr(Chi)
# <none>                    NA 849.261     NA      NA
# z.lag.from.last.choice 1.000 861.890 14.629   0.000
# previous.correct       1.000 847.980  0.719   0.397
# any.error.earlier      1.000 848.211  0.950   0.330
# z.Number_boxes         1.000 852.712  5.451   0.020
# cup.is.edge            1.000 847.976  0.715   0.398
# Sex                    1.000 852.099  4.838   0.028
# z.age                  1.000 853.403  6.142   0.013
# z.Trial_number         1.000 850.709  3.448   0.063

wt.txt(c.tab(summary(full)$coefficients, 3))
#                        Estimate Std. Error z value Pr(>|z|)
# (Intercept)              -3.329      1.140  -2.920    0.003
# z.lag.from.last.choice    0.626      0.116   5.380    0.000
# previous.correctyes       0.958      1.100   0.871    0.384
# any.error.earlier1        1.178      1.120   1.051    0.293
# z.Number_boxes           -0.331      0.116  -2.844    0.004
# cup.is.edgeyes           -0.285      0.350  -0.813    0.416
# Sexm                      0.650      0.294   2.211    0.027
# z.age                     0.360      0.145   2.489    0.013
# z.Trial_number           -0.204      0.112  -1.816    0.069

wt.txt(c.tab(as.data.frame(summary(full)$varcor), 3))
# grp                          var1 var2  vcov sdcor
# choice.id             (Intercept)   NA 0.000 0.000
# trial.id         cup.is.edge.code   NA 3.709 1.926
# trial.id.1            (Intercept)   NA 0.000 0.000
# Subject          cup.is.edge.code   NA 0.000 0.000
# Subject.1          z.Trial_number   NA 0.000 0.000
# Subject.2          z.Number_boxes   NA 0.000 0.000
# Subject.3  any.error.earlier.code   NA 0.838 0.915
# Subject.4   previous.correct.code   NA 0.000 0.000
# Subject.5  z.lag.from.last.choice   NA 0.000 0.000
# Subject.6             (Intercept)   NA 0.000 0.000



source("C:/Users/cjv3/R/R scripts/Roger/Diagnostic_fcns.r")
wt.txt(c.tab(round(overdisp.test(full),3)))

# chisq          df     P dispersion.parameter
# 1749.436 1525.000 0.000                1.147

 xres=lm(error.ny~z.lag.from.last.choice+previous.correct+any.error.earlier+z.Number_boxes+cup.is.edge+Sex+z.age+z.Trial_number+offset(log((1/number.cups))), data=test.data)

library(car)
vif(xres)


setwd("C:/Users/cjv3/R/EF battery/Updating_exp1/")
write.table(test.data, file = "test.data.txt", quote = FALSE, sep = "/",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE)


save.image("/home/roger_mundry/mnt/psychology/board/Transfer/christoph_voelter/Updating/error_model.RData")
