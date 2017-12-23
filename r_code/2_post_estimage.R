# A function to install and load packages
loadPkg = function(toLoad){
       for(lib in toLoad){
              if(! lib %in% installed.packages()[,1])
              {install.packages(lib, repos='http://cran.rstudio.com/')}
              suppressMessages( library(lib, character.only=TRUE))}}

#Load packages that we need 
packs=c("gridExtra", "dplyr", "ggplot2", "arm", "multiwayvcov", "lmtest", 
        "reshape2", "ggthemes", "foreign", "MASS", "pwt9", "WDI", "cshapes",
        "readr", "countrycode", "GGally", "texreg", "scales")
loadPkg(packs)



###function to make a coefficient plot(for ols, glm)
coef_plot= function(ModelResults, varnames = NULL, data){
       library(dplyr)  
       require(ggplot2)
       library(gridExtra)
       
       modelcoef = summary(ModelResults)$coefficients[1:length(ModelResults$coefficients), 1]
       modelse = summary(ModelResults)$coefficients[1: length(ModelResults$coefficients), 2]
       ylo = modelcoef - 1.96*(modelse)
       yhi = modelcoef + 1.96*(modelse)
       names = names(ModelResults$coefficients)
       dfplot = data.frame(names, modelcoef, modelse, ylo, yhi)
       #dfplot$names <- as.character(dfplot$names)
       dfplot <- dfplot %>% 
              dplyr::mutate(color_sig = ifelse(ylo < 0 & yhi > 0, "#ef8a62", "#2c7bb6"))
       if(!is.null(varnames)){
              dfplot <- dfplot[-match(varnames, dfplot$names),]
       }
       else{dfplot <- dfplot}
       
       p <- ggplot(dfplot, aes(x=names, y=modelcoef, ymin=ylo, ymax=yhi)) +
              geom_pointrange(size=1, shape=16, colour=dfplot$color_sig) + 
              coord_flip() + 
              geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
              xlab('') + ylab('') + 
              theme(legend.position="none",
                    axis.text = element_text(size=14), 
                    axis.title=element_text(size=14)) 
       return(p)
}
#including all but "(Intercept)"
#model 1: coef plot
png("./images/coefm1_lm.png", width=200, height=200)
coef_plot(m1_lm, varnames = "(Intercept)", data = state.panel) + 
       ggtitle("Coefficient Plot(Model 1, OLS)")
dev.off()

# model 4: polity binary
coef_plot(m2, varnames = "(Intercept)", data = state.panel) + 
       ggtitle("Coefficient Plot(Model 5, Logit)")
ggsave("./images/coefm1_m2.png",units = "cm",
      width = 10, height = 6 )


##For logit model

####function to make density plot based on simulation from clustering standard errors
cluster_densityplot <- function(ModelResults, n.sim = 1000, varname, data, 
                                clusterid, label = c("lab1", "lab2")){
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       cluster <- data[,clusterid]
       #Cluster by ccode
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)
       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)
       X1 <- model.matrix(ModelResults)
       X2 <- model.matrix(ModelResults)
       X1[,varname] <- 0
       non <- apply(apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean) #1 indicates row, 2= columns
       X2[,varname] <- 1
       yes <- apply(apply(X2, 1, function (x) plogis(sim %*% x)), 1, mean)
       simprodicted <- data.frame(non = non,
                                  yes = yes)
       require(reshape2)
       simprodicted <- melt(simprodicted)
       levels(simprodicted$variable)
       levels(simprodicted$variable) <- label
       library(ggthemes)
       legend_title <- ""
       p <- ggplot(simprodicted, aes(x=value, fill=variable)) + theme_tufte() +  
              geom_density(alpha=0.4) + 
              labs(x="Predicted Probability", title="",fill="") + 
              theme(legend.position="bottom") + labs(x = "", caption =  paste0("Note: density plots are based on ", n.sim," MC iterations")) + 
              scale_fill_manual(legend_title, values = c("gold","blue"))
       return(p)
}

p <- cluster_densityplot(m2, n.sim = 1000, varname = "polity_dummy", data = state.panel,
                         clusterid = "ccode", label = c("Non-Democracy", "Democracy"))
#you can add a title
p + ggtitle("Density plot for the distribution of regime type")
###stata to r
ggsave("./images/m2_density.png",units = "cm",
       width = 10, height = 6 )

