    GetNdviChangeRast<-function(mk_res, NDVIdat){
        ndviChangeRast<-((mk_res[["sl"]]<0.05)+1)  *  ifel(mk_res["tau"]>0, 1, -1) * ifel(is.na(NDVIdat[[1]]), NA, 1)
        ndviChangeRast<-terra::as.factor(ndviChangeRast)
        levels(ndviChangeRast)<-data.frame("ID"=c(-2, -1, 1, 2), "label"=c("sigDec", "nonsigDec", "nonsigInc", "sigInc"))

        return(ndviChangeRast)
    }