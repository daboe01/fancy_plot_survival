# 1.9.2008: survival plot tool by dr. boehringer
# todo: option to append the n= per group to the legends
# rework jitter.groups to give deterministic results

library(ggplot2)
library(survival)


plot.survival.fancy=function(s, conf.int=F, auto.scale=F, xmax=0, mark.time=T, displace.groups=F, levels=c(), reorder.groups=T, reorder.groups.FUN=NULL, xlim=NULL, ...){
	s.plot=data.frame(time=NA, surv=NA, min=NA, max=NA, group=NA, evnt=NA) # is there a better way to create an empty data.frame?
	pind=1
	if(length(names(s$strata)))
	{	off=0
		for(j in names(s$strata))
		{	if(displace.groups ==F) off=0
			s.plot=rbind(data.frame(time=0*max(s$time), surv=1+off, min=1, max=1, group=j, evnt=0), s.plot)
			uind= pind+s$strata[j]-1
			s.plot=rbind(s.plot,data.frame(time=s$time[pind: uind]+ off*max(s$time), surv=s$surv[pind: uind]+ off, min=s$upper[pind:uind], max=s$lower[pind:uind], group= rep(j, s$strata[j]), evnt=s$n.event[pind:uind] ) )
			pind= uind+1
			off=off+0.005
		}
	} else
	{	s.plot=rbind(data.frame(time=0, surv=1, min=1, max=1, group=NA, evnt=0), s.plot)
		s.plot=rbind(s.plot,data.frame(time=s$time, surv=s$surv, min=s$upper, max=s$lower, group= NA, evnt=s$n.event ) )
	}	
	
	s.plot=s.plot[!is.na(s.plot$evnt),]
	s.plot$stratum=NA
	if(xmax) s.plot=subset(s.plot, time< xmax)
	if( max (is.na(s.plot$group)) )
	{	s.plot=s.plot[order(s.plot$time), ];
		q=qplot(time, surv, data= subset(s.plot, evnt==0 & time>0), geom=ifelse(mark.time,"point","blank"),...)
		if(!is.null(xlim)) q=q+ coord_cartesian(xlim = xlim)
		if(auto.scale)
			if(conf.int)
				q+
				geom_step(data=s.plot,   aes(x=time, y=surv, group=group), size=1.2)+
				geom_ribbon(data=s.plot, aes(x=time, min=min, max=max), size=0, alpha=0.3)
			else
				q+
				geom_step(data=s.plot,   aes(x=time, y=surv), size=1.2)
		else
			if(conf.int)
				q+scale_y_continuous(limits=c(0,1))+
				geom_step(data=s.plot,   aes(x=time, y=surv), size=1.2)+
				geom_ribbon(data=s.plot, aes(x=time, min=min, max=max), size=0,alpha=0.3)
			else
				q+
				scale_y_continuous(limits=c(0,1))+
				geom_step(data=s.plot,   aes(x=time, y=surv), size=1.2)	}
	else
	{	stratavec=c()
		if(length(grep('.*strata.*?=',s.plot$group, perl=T)))
		{	stratavec=levels(as.factor(sub('.*strata.*?=','',s.plot$group, perl=T) ))
			for(j in stratavec )
			{	i= grep(j,s.plot$group)
					s.plot$stratum[i]=j;
			}
			s.plot$group=sub(', strata.*$', '', s.plot$group, perl = TRUE) #crude hack to elim the strata...
		}
	
		s.plot$stratum =sub('.*=', '', s.plot$stratum, perl = TRUE)

		s.plot$group.stripped=sub('.*=', '', s.plot$group, perl = TRUE) # another crude hack...
		s.plot$group= s.plot$group.stripped
		if(length(levels)) s.plot$group=factor(s.plot$group.stripped, levels=levels, ordered=T)
		else 
		if(!is.null(reorder.groups.FUN))
		{	s.plot$group=as.factor(s.plot$group)
			X= reorder.groups.FUN(s.plot$group)
			s.plot$group=reorder(s.plot$group, X )
		} else if(reorder.groups) s.plot$group=reorder(as.factor(s.plot$group), s.plot$surv, FUN= function(x) sum(-x)/pmax(length(x),1) )
		s.plot=s.plot[order(s.plot$group,s.plot$time), ];

		if(length(stratavec))
			 q=qplot(time, surv, data= subset(s.plot, evnt==0&time>0), geom=ifelse(mark.time,"point","blank"), colour=group, facets= stratum ~., ...)
		else
		{   my.df= subset(s.plot, evnt==0&time>0)
			my.geom= ifelse(mark.time,"point","blank")
			if(nrow(my.df)==0)
			{
				my.df=s.plot
				my.geom="blank"
			}
			q=qplot(time, surv, data= my.df, geom= my.geom, colour=group, ...)
		}
		if(!is.null(xlim)) q=q+ coord_cartesian(xlim = xlim)
		if(auto.scale)
			if(conf.int)
				return(q+
				geom_step(data=s.plot,   aes(x=time, y=surv, group=group, colour=group), size=1.2)+scale_colour_brewer(palette = 3)+
				geom_ribbon(data=s.plot, aes(x=time, min=min, max=max, fill=group), size=0, alpha=0.3)+ scale_fill_brewer(palette = 3))
			else
			{	return(q+
				geom_step(data=s.plot,   aes(x=time, y=surv, group=group, colour=group), size=1.2))
			}
		else
			if(conf.int)
				return(q+scale_y_continuous(limits=c(-0.05,1.1))+
				geom_step(data=s.plot,   aes(x=time, y=surv, group=group, colour=group), size=1.2)+scale_colour_brewer(palette = 3)+
				geom_ribbon(data=s.plot, aes(x=time, min=min, max=max, fill=group), size=0, alpha=0.3)+ scale_fill_brewer(palette = 3))
			else
			{	return(q+
				scale_y_continuous(limits=c(-0.05,1.1))+
				geom_step(data=s.plot, aes(x=time, y=surv, group=group, colour=group), size=1.2))

			}
	}
}
