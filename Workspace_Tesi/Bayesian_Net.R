library(visNetwork)
library(bnlearn)
library(tidyverse)
library(DAAG)



# Structure Learning
bn.hc2 <- hc(train_final2, score = "loglik-g")


par(mfrow = c(1,1))

plot(bn.hc2, main = "Hill-Climbing Bayesian Network",  highlight = c("LY", "WBC",'AST',"NE"))
plot.network(bn.hc2)

bn.hc2


# Fitting del modello
bn_mod2 <- bn.fit(bn.hc2, data = train_final2, method = "mle")

bn_mod2

# Data Generation
ais_sim2 <- rbn(bn_mod2, 1389)
head(ais_sim2)


write.csv(ais_sim2,"Desktop\\bnet_dataR.csv", row.names = FALSE)


# Visualizzazione diagnostiche modello
bn.fit.qqplot(bn_mod2, xlab = "Theoretical Quantiles",
              ylab = "Sample Quantiles", )


bn.fit.histogram(bn_mod2, density = TRUE, xlab = "Residuals",
                 ylab =  "Density")

bn.fit.xyplot(bn_mod2, xlab = "Fitted values", ylab = "Residuals")



# Definizione funzioni per esplorare la rete creata
## Plot interattivo
plot.network <- function(structure, ht = "800px", cols = "darkturquoise", labels = nodes(structure)){
  if(is.null(labels)) labels <- rep("", length(nodes(structure)))
  nodes <- data.frame(id = nodes(structure),
                      label = labels,
                      color = cols,
                      shadow = TRUE
  )
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = FALSE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "60%"))
}

