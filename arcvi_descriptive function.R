##############################################################
#
# Function to create "descriptivos" given a feature and 
# the target variable.
#
##############################################################
list_of_packages = c('ggplot2','data.table')
for (p in list_of_packages){
    if(!require(p,character.only=TRUE)){
        install.packages(p)
        library(p)
    }
}

#' Computes the probability and precentage of population of a feature
#' given the target (binary 0-1)
#'
#' @param   feature: either numeric or character/factor is allowed
#' @param   target: numeric /in {0,1} 
#' @param   equidistributed: T/F indicating if the bins have to be equidistributed
#' @param   bins: integer indicating the number of bins
#'
#'
#' @return   Prints the value of the data frame and plots the descriptivo
#' @export


arcvi_descriptive = function(feature,target,equidistributed=T,bins=10){ 
    df = data.table(feature,target)
    bins=10
    
    print(df)
    
    if(class(feature) %in% c("numeric","integer")){
        if(uniqueN(feature)<7){
            df[,feature:=as.character(feature)]  
        }else{
            if(equidistributed) breaks = unique(quantile(feature, probs=seq(0, 1, 1.0/bins),na.rm=T))
            else breaks = bins # for equidistributed
            df[,feature:=cut(feature, breaks=breaks, include.lowest=T)]
        }
    }
    
    df[is.na(feature), feature:="NA"]
    df = df[,.(prob=mean(target), prct=.N/nrow(df), num=.N), by=feature][order(feature)]
    
    x = ggplot(df) +
        geom_bar(aes(x = feature, weight = prct)) +
        geom_line(aes(x = as.numeric(factor(feature)), y = prob)) + 
        theme(axis.text.x = element_text(angle = 40, hjust = 1),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
    print(x)
    
    return(df)
}