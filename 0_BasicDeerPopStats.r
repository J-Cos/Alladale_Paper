#####################
#brief script to calculate deer population density and culling stats
#data from Seivwright et al., 2017)
#####################
library(tidyverse)


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



# make df for visualisation
    pop_df<-data.frame(popCounts=popCounts,
            Year=c(2002, 2003, 2004, 2005, 2006, 2008, 2015, 2017)
            ) %>%
                mutate(popCounts=popCounts*10)
    cull_df<-data.frame(culls=culls,
            Year=c(2000:2016)
            )
    df<-dplyr::left_join(cull_df, pop_df) %>%
        pivot_longer(cols=-Year, names_to="type", values_to="count") %>%
        filter(!is.na(count))


png(file.path("Figures","FigureSX.png"), height = 6, width = 12, units = 'in', res = 300)
    ggplot(df, aes(x=Year))+
            geom_line(aes(y=count, color=type))+
            geom_point(aes(y=count, color=type), size=4)+
            theme_bw()+ 
            ggsci::scale_color_npg(labels=c("Deer Culls", "Deer Density (/1000ha)"))+
            ylim(c(0, NA))+
            ylab("Numer of Deer")+
            theme(
                legend.title=element_blank(),
                text=element_text(size=16, color="black")
            )
dev.off()


