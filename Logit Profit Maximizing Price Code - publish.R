####################################################
####################################################
#####PROFIT MAXIMIZING PRICE UNDER LOGIT DEMAND#####
####################################################
####################################################


###########
###INTRO###
###########
## This code accompanies the logit demand profit maximization notes.
## The code is broken down into sections in the order of the paper.
## First, code is provided to recreate the line graphs for the
## geometric intuition for the problem. Then, an example script is
## provided to solve for the profit maximizing price under logit demand
## with two firms. The third section provides an example of solving for
## the profit maximizing price with six firms, and the fourth section
## provides code to solve for the profit maximizing price with three
## firms under nested logit demand. Finally, code is provided as backup
## for the Linear Guess and Check Example in Appendix 2.

#####################################
###PROFIT FUNCTION LINE GRAPH CODE###
#####################################
## Graph 1: Line graph of Starbucks's profit function given Dunkin's
## price of $3.00
rm(list = ls())

library("dplyr")
library("reshape2")
library("ggplot2")
library("scales")

## Set up the intial consumer parameters from the notes:
aS <- 6
bS <- -0.8
aD <- 5
bD <- -1
N <- 1000

## Set up a vector of Starbucks's prices to calculate Starbucks's profit.
## Also set up Dunkin's price equal to $3.00
sbux_prices = seq(1.50, 15, 0.01)
sbux_cost <- 1.50
dunks_price = 3.00

## calculate Starbucks's market share at each price, then use this to
## calculate Starbucks's profit when the market size is N.
sbux_share = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price)))
sbux_profit = (sbux_prices - sbux_cost)*N*sbux_share

sbux_profit_data <- as.data.frame(cbind(sbux_prices, sbux_share, sbux_profit))

## Graph Starbucks's profit when Dunkin's price is set to $3.00 using ggplot.
sbux_profit_graph1 <- ggplot(data = sbux_profit_data, aes(x = sbux_prices, y = sbux_profit)) + 
  geom_line(color = "#d7191c") + 
  scale_x_continuous(name = "Starbucks's Price", limits = c(0, 15), labels = label_dollar()) +
  scale_y_continuous(name = "Starbucks's Profit", limits = c(0, 6500), labels = label_dollar()) +
  labs(title = "Starbucks's Profit Function Given Dunkin's Price of $3.00") + 
  theme(plot.title = element_text(face = "bold", hjust = .5)) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold"))

sbux_profit_graph1

## Graph 2: Line graph of Starbucks's profit function given Dunkin's
## prices $2.00, $2.50, $3.00, $3.50, and $4.00

## Set up additional profit curve data for Dunkin's varied prices.
dunks_price1 <- 2.00
dunks_price2 <- 2.50
dunks_price3 <- 3.00
dunks_price4 <- 3.50
dunks_price5 <- 4.00

## calculate Starbucks's market share at each price, then use this to
## calculate Starbucks's profit when the market size is N.
sbux_share1 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price1)))
sbux_profit1 = (sbux_prices - sbux_cost)*N*sbux_share1

sbux_share2 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price2)))
sbux_profit2 = (sbux_prices - sbux_cost)*N*sbux_share2

sbux_share3 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price3)))
sbux_profit3 = (sbux_prices - sbux_cost)*N*sbux_share3

sbux_share4 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price4)))
sbux_profit4 = (sbux_prices - sbux_cost)*N*sbux_share4

sbux_share5 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_price5)))
sbux_profit5 = (sbux_prices - sbux_cost)*N*sbux_share5

sbux_profit_data2 <- as.data.frame(cbind(sbux_prices, sbux_profit1,
                                         sbux_profit2, sbux_profit3,
                                         sbux_profit4, sbux_profit5))

sbux_profit_data2 <- sbux_profit_data2 %>%
  melt(id = "sbux_prices") %>%
  rename("dunks_price" = "variable", "sbux_profit" = "value") %>%
  mutate(dunks_price = case_when(dunks_price == "sbux_profit1" ~ "$2.00",
                                 dunks_price == "sbux_profit2" ~ "$2.50",
                                 dunks_price == "sbux_profit3" ~ "$3.00",
                                 dunks_price == "sbux_profit4" ~ "$3.50",
                                 dunks_price == "sbux_profit5" ~ "$4.00"))

line_colors <- c("$2.00" = "#ffffbf", "$2.50" = "#fdae61", "$3.00" = "#d7191c", 
                 "$3.50" = "#abd9e9", "$4.00" = "#2c7bb6")

sbux_profit_graph2 <- ggplot(data = sbux_profit_data2, aes(x = sbux_prices, y = sbux_profit, group = dunks_price, color = dunks_price)) + 
  geom_line() +
  scale_color_manual(name = "Dunkin's Price", values = line_colors) +
  scale_x_continuous(name = "Starbucks's Price", limits = c(0, 15), labels = label_dollar()) +
  scale_y_continuous(name = "Starbucks's Profit", limits = c(0, 8000), labels = label_dollar()) +
  labs(title = "Starbucks's Profit Function Given Dunkin's Price of $3.00") + 
  theme(plot.title = element_text(face = "bold", hjust = .5)) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold"))

sbux_profit_graph2


####################################################################
###SOLVE FOR PROFIT MAXIMIZING PRICE UNDER LOGIT DEMAND - DUOPOLY###
####################################################################
## Try to solve the logit problem again, this time using the lambertW equation.
rm(list = ls())

library("pracma")

## Set up the intial consumer parameters from the notes:
aS <- 6
bS <- -0.8
aD <- 5
bD <- -1
N <- 1000
sbux_cost <- 1.50
dunks_cost <- 1.25

## Initialize a vector of prices to start the guess and check method. Ideally,
## these are somewhat close to where we expect the profit maximizing prices to be.
sbux_price_old <- 3
dunks_price_old <- 3

## Initialize the maximum interations for the guess and check method and a starting
## value for the error term
maxit <- 1
error <- 1

## Run a "while" loop to try values until we find a fixed point of the two
## equations. This loop will exit either when the error term is smaller than
## 10^(-9) or when the loop iterates the maximum number of times.
while(error > 0.00000001 & maxit < 10000) {
  ## Calculate the new prices using the prices you initialized above.
  sbux_price_new = (sbux_cost + ((1 + lambertWp(exp(aS - 1 + sbux_cost*bS)/(exp(aD + bD*dunks_price_old))))/-bS))
  dunks_price_new = (dunks_cost + ((1 + lambertWp(exp(aD - 1 + dunks_cost*bD)/(exp(aS + bS*sbux_price_old))))/-bD))

  ## Calculate the error term as the distance between the new prices and the
  ## old prices.
  error = sqrt((sbux_price_new - sbux_price_old)^2 + (dunks_price_new - dunks_price_old)^2)
  
  ## reassign the old prices to the new prices. Each iteration should get
  ## successively closer to the optimal price, and this will continue until
  ## we find the fixed point of this system of equations.
  sbux_price_old = sbux_price_new
  dunks_price_old = dunks_price_new
  maxit = maxit + 1
}


################################################
###DUOPOLY PROFIT MAXIMIZING PRICE FACET GRID###
################################################
## Graph 3: Side-by-side line graphs of Starbucks and Dunkin's profit functions
## using the optimal prices
rm(list = ls())

library("dplyr")
library("reshape2")
library("ggplot2")
library("scales")
library("patchwork")

## Set up the intial consumer parameters from the notes:
aS <- 6
bS <- -0.8
aD <- 5
bD <- -1
N <- 1000

## Set up a vector of Starbucks's prices to calculate Starbucks's profit.
## Also set up Dunkin's price equal to $3.00
sbux_prices = seq(1.50, 10, 0.01)
sbux_cost <- 1.50
dunks_optimal_price = 2.9579

## calculate Starbucks's market share at each price, then use this to
## calculate Starbucks's profit when the market size is N.
sbux_share6 = (exp(aS + bS*sbux_prices)/(exp(aS + bS*sbux_prices) + exp(aD + bD*dunks_optimal_price)))
sbux_profit6 = (sbux_prices - sbux_cost)*N*sbux_share6

sbux_profit_data3 <- as.data.frame(cbind(sbux_prices, sbux_share6, sbux_profit6))

## Graph Starbucks's profit when Dunkin's price is set to $2.6048 using ggplot.
sbux_profit_graph3 <- ggplot(data = sbux_profit_data3, aes(x = sbux_prices, y = sbux_profit6)) + 
  geom_line(color = "#d7191c") + 
  scale_x_continuous(name = "Starbucks's Price", limits = c(0, 10), labels = label_dollar()) +
  scale_y_continuous(name = "Starbucks's Profit", limits = c(0, 2000), labels = label_dollar()) +
  labs(title = "Starbucks's Profit Function Given Dunkin's Price of $3.00") + 
  theme(plot.title = element_text(face = "bold", hjust = .5)) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold"))

sbux_profit_graph3

## Now set up Dunkin's profit graph when Starbucks's price is set to the
## profit maximizing price:
## Set up a vector of Starbucks's prices to calculate Starbucks's profit.
## Also set up Dunkin's price equal to $3.00
dunks_prices = seq(1.50, 10, 0.01)
dunks_cost <- 1.25
sbux_optimal_price = 4.5157

## calculate Starbucks's market share at each price, then use this to
## calculate Starbucks's profit when the market size is N.
dunks_share = (exp(aD + bD*dunks_prices)/(exp(aS + bS*sbux_optimal_price) + exp(aD + bD*dunks_prices)))
dunks_profit = (dunks_prices - dunks_cost)*N*dunks_share

dunks_profit_data <- as.data.frame(cbind(dunks_prices, dunks_share, dunks_profit)) 

## Graph Dunkin's profit when Starbucks's price is set to $9.1371 using ggplot.
dunks_profit_graph <- ggplot(data = dunks_profit_data, aes(x = dunks_prices, y = dunks_profit)) + 
  geom_line(color = "#d7191c") + 
  scale_x_continuous(name = "Dunkin's Price", limits = c(0, 10), labels = label_dollar()) +
  scale_y_continuous(name = "Dunkin's Profit", limits = c(0, 2000), labels = label_dollar()) +
  labs(title = "Dunkin's Profit Function Given Starbucks's Price of $9.14") + 
  theme(plot.title = element_text(face = "bold", hjust = .5)) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_text(face = "bold"))

dunks_profit_graph

combined_plot <- sbux_profit_graph3 + dunks_profit_graph

combined_plot


######################################################################
###SOLVE FOR PROFIT MAXIMIZING PRICE UNDER LOGIT DEMAND - SIX FIRMS###
######################################################################
rm(list = ls())

library("pracma")

## Set the consumer parameters for the six coffee shops:

## Starbucks
aS <- 6
bS <- -0.8
sbux_cost <- 1.50

## Dunkin
aD <- 5
bD <- -1
dunks_cost <- 1.25

## Compass Coffee
aC <- 8
bC <- -0.8
compass_cost <- 2.50

## Peets
aP <- 5.7
bP <- -0.8
peets_cost <- 1.75

## Firehook
aF <- 8
bF <- -0.8
firehook_cost <- 3.50

## La Colombe
aL <- 5.8
bL <- -0.8
lacolombe_cost <- 2.00

N <- 1000

sbux_price_old <- 3
dunks_price_old <- 3
compass_price_old <- 3
peets_price_old <- 3
firehook_price_old <- 3
lacolombe_price_old <- 3

maxit <- 1
error <- 1

while(error > 0.00000001 & maxit < 10000) {
  sbux_price_new = (sbux_cost + ((1 + lambertWp(exp(aS - 1 + sbux_cost*bS)/(exp(aD + bD*dunks_price_old) + exp(aC + bC*compass_price_old) + exp(aP + bP*peets_price_old) + exp(aF + bF*firehook_price_old) + exp(aL + bL*lacolombe_price_old))))/-bS))
  dunks_price_new = (dunks_cost + ((1 + lambertWp(exp(aD - 1 + dunks_cost*bD)/(exp(aS + bS*sbux_price_old) + exp(aC + bC*compass_price_old) + exp(aP + bP*peets_price_old) + exp(aF + bF*firehook_price_old) + exp(aL + bL*lacolombe_price_old))))/-bD))
  compass_price_new = (compass_cost + ((1 + lambertWp(exp(aC - 1 + compass_cost*bC)/(exp(aS + bS*sbux_price_old) + exp(aD + bD*dunks_price_old) + exp(aP + bP*peets_price_old) + exp(aF + bF*firehook_price_old) + exp(aL + bL*lacolombe_price_old))))/-bC))
  peets_price_new = (peets_cost + ((1 + lambertWp(exp(aP - 1 + peets_cost*bP)/(exp(aS + bS*sbux_price_old) + exp(aD + bD*dunks_price_old) + exp(aC + bC*compass_price_old) + exp(aF + bF*firehook_price_old) + exp(aL + bL*lacolombe_price_old))))/-bP))
  firehook_price_new = (firehook_cost + ((1 + lambertWp(exp(aF - 1 + firehook_cost*bF)/(exp(aS + bS*sbux_price_old) + exp(aD + bD*dunks_price_old) + exp(aC + bC*compass_price_old) + exp(aP + bP*peets_price_old) + exp(aL + bL*lacolombe_price_old))))/-bF))
  lacolombe_price_new = (lacolombe_cost + ((1 + lambertWp(exp(aL - 1 + lacolombe_cost*bL)/(exp(aS + bS*sbux_price_old) + exp(aD + bD*dunks_price_old) + exp(aC + bC*compass_price_old) + exp(aP + bP*peets_price_old) + exp(aF + bF*firehook_price_old))))/-bL))
  
  error = sqrt((sbux_price_new - sbux_price_old)^2 + (dunks_price_new - dunks_price_old)^2 + (compass_price_new - compass_price_old)^2 + (peets_price_new - peets_price_old)^2 + (firehook_price_new - firehook_price_old)^2 + (lacolombe_price_new - lacolombe_price_old)^2)
  
  sbux_price_old = sbux_price_new
  dunks_price_old = dunks_price_new
  compass_price_old = compass_price_new
  peets_price_old = peets_price_new
  firehook_price_old = firehook_price_new
  lacolombe_price_old = lacolombe_price_new
  maxit = maxit + 1
}

sbux_share <- exp(aS + bS*sbux_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))
dunks_share <- exp(aD + bD*dunks_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))
compass_share <- exp(aC + bC*compass_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))
peets_share <- exp(aP + bP*peets_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))
firehook_share <- exp(aF + bF*firehook_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))
lacolombe_share <- exp(aP + bP*lacolombe_price_new)/(exp(aS + bS*sbux_price_new) + exp(aD + bD*dunks_price_new) + exp(aC + bC*compass_price_new) + exp(aP + bP*peets_price_new) + exp(aF + bF*firehook_price_new) + exp(aL + bL*lacolombe_price_new))

sbux_margin <- (sbux_price_new - sbux_cost)/sbux_price_new
dunks_margin <- (dunks_price_new - dunks_cost)/dunks_price_new
compass_margin <- (compass_price_new - compass_cost)/compass_price_new
peets_margin <- (peets_price_new - peets_cost)/peets_price_new
firehook_margin <- (firehook_price_new - firehook_cost)/firehook_price_new
lacolombe_margin <- (lacolombe_price_new - lacolombe_cost)/lacolombe_price_new


#########################################
###DO SOMETHING WITH ANTITRUST OR BLP?###
#########################################
## Use the "logit" function from the antitrust package to simulate a
## merger between Starbucks and Dunkin in this world. Notably, you
## need to be careful with the chosen values of a and b for each firm.
## If your chosen values are too extreme, then the logit function won't
## converge.

rm(list = ls())
library("antitrust")

prodNames <- c("Starbucks", "Dunkin", "Compass", "Peets", "Firehook", "LaColombe")
ownerPre <-c("Starbucks", "Dunkin", "Compass", "Peets", "Firehook", "LaColombe")
ownerPost <-c("Starbucks", "Starbucks", "Compass", "Peets", "Firehook", "LaColombe")

price <- c(2.99, 2.32, 4.46, 3.14, 5.10, 3.38)
shares <- c(.1591, .0629, .3619, .1038,.2173, .0859)
margins <- c(.4977, .4605, .4393, .4435, .3133, .4085)

share_test <- sum(shares)

insideSize <- 1000

names(price) <-
  names(shares) <-
  names(margins) <-
  prodNames

logit_result <- antitrust::logit(price,shares,margins,
                      ownerPre=ownerPre,ownerPost=ownerPost,
                      insideSize = insideSize,
                      labels=prodNames)

print(logit_result)           # return predicted price change
summary(logit_result)         # summarize merger simulation

## We find that a merger of Starbucks and Dunkin would lead to a 0.38%
## price increase in the coffee market in DC.

## See the values for the mean utility and optimal price
## coefficient for each coffee shop:
logit_result@slopes

## Here, "alpha" gives the value "that best satisfies all the FOCs for which 
## there are data". "meanval" then provides the mean valuations of each
## product. You will note that these accurately approximate the valuations
## that we set up.

#######################################################
###APPENDIX 2: GUESS AND CHECK METHOD LINEAR EXAMPLE###
#######################################################
rm(list = ls())

x_old <- 3
y_old <- 5

maxit <- 1
error <- 1

while(error > 0.00001 & maxit < 10000) {
  x_new = (y_old - 9)/(-2)
  y_new = 5 - x_old
  
  error = sqrt((y_new - y_old)^2 + (x_new - x_old)^2)
  
  x_old = x_new
  y_old = y_new
  maxit = maxit + 1
  
}
