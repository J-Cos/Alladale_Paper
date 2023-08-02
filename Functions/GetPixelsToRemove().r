GetPixelsToRemove<-function(cell, AcceptableRunOfNAs) {
    if (max(rle(cell)$lengths)<=AcceptableRunOfNAs) {
        return(1)
    } else if (max(rle(cell)$lengths)>AcceptableRunOfNAs) {
        return(NA)
    }
}