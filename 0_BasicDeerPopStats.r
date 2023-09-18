#####################
#brief script to calculate deer population density and culling stats
#data from Seivwright et al., 2017)
#####################

#1) popn density
    popCounts<-c(16.5, 15.2, 12.0, 15.6, 11.9, 10.4, 16.7, 11.2)
    mean(popCounts)
    sd(popCounts)
    range(popCounts)
    Kendall::MannKendall(popCounts)
    
#2) culling
#frm 2000 onward only (data from 1995 until 1999 excluded as before our ndvi study period (lc change study even shorted than that: 2007 to 2019/2021))
    culls<-c(424,355,281,347,564,535,336,450,350,500,231,232,200,250,350,315,250)
    mean(culls)
    sd(culls)
    range(culls)
    Kendall::MannKendall(culls)
