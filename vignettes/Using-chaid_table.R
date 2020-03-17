## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----one, fig.width=7.0, fig.height=5.5, warning=FALSE, message=FALSE---------
library(CGPfunctions)
# library(CHAID)
library(dplyr)
library(knitr)

### fit tree to subsample see ?chaid
##  set.seed(290875)
##  USvoteS <- USvote[sample(1:nrow(USvote), 1000),]

##  ctrl <- chaid_control(minsplit = 200, minprob = 0.1)
##  chaidUS <- chaid(vote3 ~ ., data = USvoteS, control = ctrl)
print(chaidUS) 
plot(chaidUS)

## ----simple2, fig.width=7.0, fig.height=3.5-----------------------------------
# simplest use --- chaidUS is included in the package
class(chaidUS)

chaid_table(chaidUS)

mychaidtable <- chaid_table(chaidUS)

## ----simple3------------------------------------------------------------------
mychaidtable %>% 
  select(nodeID:ruletext) %>% 
  kable()

# Just node #2 show percentage
mychaidtable %>% 
  select(nodeID:ruletext) %>% 
  filter(nodeID == 2) %>% 
  mutate(pctBush = Bush/NodeN * 100) %>%
  kable(digits = 1)

# Just the children of node #5
mychaidtable %>% 
  select(nodeID:ruletext) %>% 
  filter(parent == 5) %>% 
  kable()

# stats for all splits including raw (unadjusted) p value
mychaidtable %>% 
  select(nodeID, NodeN, split.variable:rawpvalue) %>% 
  filter(!is.na(split.variable)) %>% 
  kable()


