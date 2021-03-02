### !!! need to add in citation for
# https://devinincerti.com/2018/02/10/psa.html

######### sensitivity analysis ######################

##### !!! copied and pasted out of main script
## & from older version
## so need to go through and update once finalised
## base model

## number of parameters
nr <- nrow(inputs)
## number of psa runs
npsa <- 1000

######## running different sampling distributions based on the distribution types
####**** beta moments****####

mbeta <- inputs[distribution=="beta"]
mbeta[ , alpha := ((value^2)*(1-value)/(low^2))-(value)]
mbeta[ , beta := alpha*((1-value)/value)]

sampling.beta <- function(x, n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(x[n.t, "alpha"])
  be <- as.numeric(x[n.t, "beta"])
  vec <- rbeta(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid=runid, scenario, parameter,value=vec)
  return(dt)
}

l.mbeta <- list()
# for each subgroup + resistance type
for (i in 1:nrow(mbeta)) {
  n.t <- i
  l.mbeta[[i]] <- sampling.beta(mbeta,n.t)
}


#####**** beta *****####
# with count - not currently used

#####**** lognormal ****######
# haven't done a gamma one in this current iteration 
ln.d <- inputs[distribution=="lognormal"]
ln.d[ , se := as.numeric(as.character(low))]
ln.d[ ,location := log(value^2 / sqrt(se^2 + value^2))]
ln.d[ , shape := sqrt(log(1 + (se^2 / value^2)))]


sampling.ln <- function(x, n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(x[n.t, "location"])
  be <- as.numeric(x[n.t, "shape"])
  vec <- rlnorm(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid=runid, scenario, parameter,value=vec)
  return(dt)
}

l.lognorm <- list()


for (i in 1:nrow(ln.d)) {
  n.t <- i
  l.lognorm[[i]] <- sampling.ln(ln.d,n.t)
}



###**** normal****#####
norm.d <- inputs[distribution=="normal"]
norm.d[ , SD := low]   

sampling.n <- function(x,n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(norm.d[n.t, "value"])
  be <- as.numeric(norm.d[n.t, "SD"])
  vec <- rnorm(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.norm <- list()

for (i in 1:nrow(norm.d)) {
  n.t <- i
  l.norm[[i]] <- sampling.n(norm.d,n.t)
}

### uniform distribution
###**** normal****#####
uni.d <- inputs[distribution=="uniform"]

sampling.u <- function(x,n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  mn <- as.numeric(uni.d[n.t, "low"])
  mx <- as.numeric(uni.d[n.t, "high"])
  vec <- runif(1000, mn, mx)
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.uni <- list()

for (i in 1:nrow(uni.d)) {
  n.t <- i
  l.uni[[i]] <- sampling.u(uni.d,n.t)
}

### just repeat for those not in PSA analysis currently the same values 1000 times

other.d <- inputs[is.na(distribution)]


sampling.other <- function(x, n.t) {
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  vec <- x[n.t, "value"]
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.other <- list()


for (i in 1:nrow(other.d)) {
  n.t <- i
  l.other[[i]] <- sampling.other(other.d,n.t)
}


###### combine to form 1000 data.tables to pull the parameters from 

### restack the individual lists before joining
mbeta <- rbindlist(l.mbeta)
mnorm <- rbindlist(l.norm)
lognorm <- rbindlist(l.lognorm)
uniform <- rbindlist(l.uni)
other <- rbindlist(l.other)

# Change colname of last data frame
colnames(other)[colnames(other) == "value.value"] <- "value"


## create data.table of all the current lists
psa.data <- list(mbeta, mnorm, lognorm, uniform,other)
bigdata <- rbindlist(psa.data)

## create array of each run to pull from for the model
# probably a faster way to do this 
# number of columns
nc <- ncol(psa.data[[1]])
cols <- colnames(bigdata)

#  Preallocate 
# psa.values <- array(0,
#                     dim = c(nr, nc, npsa),
#                     dimnames = list(1:nr, cols,
#                                     paste("run", 1:npsa, sep = "")))
# # fill
# for (i in 1:npsa) {
#   psa.values[, ,i] <- as.matrix(bigdata[runid==i])
# }

# ## save
# save(psa.values, file="data/psa.RData")


psa_output <- data.table(incr_cost=rep(0,1000),
                         incr_benefit=rep(0,1000),
                         incr_cost_a=rep(0,1000),
                         incr_benefit_a=rep(0,1000),
                         icer=rep(0,1000),
                         CBR=rep(0,1000),
                         NMB_H=rep(0,1000),
                         NMB_A=rep(0,1000))

for (i in 1:1000){
  
  inputs <- as.data.table(bigdata[runid==i])
  
  psa_output[i, ] <- model(inputs)
  
}

# save list
# save(psa_output, file="data/output_PSA.RData")

library(ggplot2)

ggplot(psa_output, aes(x=incr_benefit, y=incr_cost)) +
  geom_point() +labs(x = "Incremental QALYs", y="Incremental Cost (£)")+
  labs(title = "Probablistic Sensitivity Analysis - Healthcare")

ggplot(psa_output, aes(x=incr_benefit_a, y=incr_cost_a)) +
  geom_point() +labs(x = "Incremental Benefit (£)", y="Incremental Cost (£)")+
  labs(title = "Probablistic Sensitivity Analysis - Agriculture")



#################### univariate sensitivity analysis (two-way)


inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)

inputs.urh.low <- inputs
inputs.urh.low[parameter=="u_RH", value := 0.05]
model(inputs.urh.low)

inputs.urh.high <- inputs
inputs.urh.high[parameter=="u_RH", value := 0.13]
model(inputs.urh.high)

inputs.rcost.low <- inputs
inputs.rcost.low[parameter=="r_cost" & scenario=="human_0", value := 1400]
model(inputs.rcost.low)

inputs.rcost.high <- inputs
inputs.rcost.high[parameter=="r_cost" & scenario=="human_0", value := 1700]
model(inputs.rcost.high)

inputs.rmort.low <- inputs
inputs.rmort.low[parameter=="r_dead" & scenario=="human_0", value := 0.15]
model(inputs.rmort.low)

inputs.rmorthigh.high <- inputs
inputs.rmorthigh.high[parameter=="r_dead" & scenario=="human_0", value := 0.20]
model(inputs.rmorthigh.high)

#### hand input the results for univariate for now for plot

library(ggplot2)

tornado1 <- data.frame(variable = c("Reduction in Resistant Infection Risk in Humans from Intervention",
                                    "Cost of Resistant Infection",
                                    "Mortality from Resistant Infections"),
                       min = c(-512.8244, -448.1568, -482.11),
                       max = c(-512.8295, -544.2149, -591.29))

save(tornado1, file="data/output_tornado_HC.RData")

basecase1 <- -512.8295

ggplot(tornado1, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = basecase1, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))


### same for agriculture parameters

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.urh.low <- inputs
inputs.urh.low[parameter=="u_RA", value := 0.17]
model(inputs.urh.low)

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.urh.high <- inputs
inputs.urh.high[parameter=="u_RA", value := 0.32]
model(inputs.urh.high)

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.rcost.low <- inputs
inputs.rcost.low[parameter=="r_cost_impact" & scenario=="animal_0", value := 0.05]
model(inputs.rcost.low)

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.rcost.high <- inputs
inputs.rcost.high[parameter=="r_cost_impact" & scenario=="animal_0", value := 0.5]
model(inputs.rcost.high)

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.rmort.low <- inputs
inputs.rmort.low[parameter=="r_dead_impact" & scenario=="animal_0", value := 0.05]
model(inputs.rmort.low)

inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)
inputs.rmorthigh.high <- inputs
inputs.rmorthigh.high[parameter=="r_dead_impact" & scenario=="animal_0", value := 0.50]
model(inputs.rmorthigh.high)

tornado2 <- data.frame(variable = c("Reduction in Resistant Infection Risk in Livestock from Intervention",
                                    "Cost of Resistant Infection Impact",
                                    "Mortality from Resistant Infections Impact"),
                       min = c(1.65, 2.34, 2.35),
                       max = c(3.382, 2.53,2.49))

save(tornado2, file="data/output_tornado_Agr.RData")

basecase2 <- 2.418353

ggplot(tornado2, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = basecase2, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))