####################################################################################################################################################################################################

#Rscript to the paper: "Population monitoring of fire salamanders (Salamandra salamandra) with a new photo-recognition software "
#authors: P. Oswald, B. Tunnat & B. A. Caspers
#created: 27-04-2022
#last modified: 

####################################################################################################################################################################################################

# 1. LARVAL RECAPTURE ANALYSES TO OBTAIN RECAPTURE RATES, SURVIVAL, ESTIMATED POPULATION SIZE

# needed packages
library(R2ucare) # to test for goodness of fit (underlying assumptions)
library(dplyr) # for tidy data
library(magrittr) # for pipes
library(RMark)
MarkPath="D:/UniBieProgramme/MARK" # set path to were the Mark pogram is saved on your computer

########## ponds 2021
##### KoVK 
# based on weekly averages
# upload and manipulate data
KoVK21=read.delim("KoVK21.txt",colClass=c("character","character"))
KoVK21mut <- mutate(KoVK21, across(c(ID), as.factor))
str(KoVK21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK21.matrix <- KoVK21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVK21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK21.matrix, rep(1,nrow(KoVK21mut)))
# if p>0.05 no evidence for lack of fit
# process data
KoVK21.proc <- process.data(KoVK21mut, model = "POPAN")
# Smake design data (from processed data)
KoVK21.dd <- make.design.data(KoVK21.proc)
fit.KoVK21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoVK21.proc, ddl = KoVK21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoVK21.models <- fit.KoVK21.model()
KoVK21.models
summary(KoVK21.models[[2]], se=TRUE)
KoVK21.models[[2]]$results$derived

##### SG
# based on weekly averages
# upload and manipulate data
SG21=read.delim("SG21.txt",colClass=c("character","character"))
SG21mut <- mutate(SG21, across(c(ID), as.factor))
str(SG21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
SG21.matrix <- SG21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(SG21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(SG21.matrix, rep(1,nrow(SG21mut)))
# if p>0.05 no evidence for lack of fit
# process data
SG21.proc <- process.data(SG21mut, model = "POPAN")
# Smake design data (from processed data)
SG21.dd <- make.design.data(SG21.proc)
fit.SG21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = SG21.proc, ddl = SG21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
SG21.models <- fit.SG21.model()
SG21.models
summary(SG21.models[[1]], se=TRUE)
SG21.models[[1]]$results$derived

##### TG
# based on weekly averages
# upload and manipulate data
TG21=read.delim("TG21.txt",colClass=c("character","character"))
TG21mut <- mutate(TG21, across(c(ID), as.factor))
str(TG21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TG21.matrix <- TG21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TG21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TG21.matrix, rep(1,nrow(TG21mut)))
# if p>0.05 no evidence for lack of fit
# process data
TG21.proc <- process.data(TG21mut, model = "POPAN")
# Smake design data (from processed data)
TG21.dd <- make.design.data(TG21.proc)
fit.TG21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TG21.proc, ddl = TG21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TG21.models <- fit.TG21.model()
TG21.models
summary(TG21.models[[1]], se=TRUE)
TG21.models[[1]]$results$derived

##### TT
# based on weekly averages
# upload and manipulate data
TT21=read.delim("TT21.txt",colClass=c("character","character"))
TT21mut <- mutate(TT21, across(c(ID), as.factor))
str(TT21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TT21.matrix <- TT21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TT21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TT21.matrix, rep(1,nrow(TT21mut)))
# if p>0.05 no evidence for lack of fit
# process data
TT21.proc <- process.data(TT21mut, model = "POPAN")
# Smake design data (from processed data)
TT21.dd <- make.design.data(TT21.proc)
fit.TT21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TT21.proc, ddl = TT21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TT21.models <- fit.TT21.model()
TT21.models
summary(TT21.models[[2]], se=TRUE)
TT21.models[[2]]$results$derived

#########+ streams
##### KB
# based on weekly averages
# upload and manipulate data
KB21=read.delim("KB21.txt",colClass=c("character","character"))
KB21mut <- mutate(KB21, across(c(ID), as.factor))
str(KB21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KB21.matrix <- KB21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KB21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KB21.matrix, rep(1,nrow(KB21mut)))
# if p>0.05 no evidence for lack of fit
# process data
KB21.proc <- process.data(KB21mut, model = "POPAN")
# Smake design data (from processed data)
KB21.dd <- make.design.data(KB21.proc)
fit.KB21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KB21.proc, ddl = KB21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KB21.models <- fit.KB21.model()
KB21.models
summary(KB21.models[[1]], se=TRUE)
KB21.models[[1]]$results$derived

##### KoB
# based on weekly averages
# upload and manipulate data
KoB21=read.delim("KoB21.txt",colClass=c("character","character"))
KoB21mut <- mutate(KoB21, across(c(ID), as.factor))
str(KoB21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoB21.matrix <- KoB21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoB21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoB21.matrix, rep(1,nrow(KoB21mut)))
# if p>0.05 no evidence for lack of fit
# process data
KoB21.proc <- process.data(KoB21mut, model = "POPAN")
# Smake design data (from processed data)
KoB21.dd <- make.design.data(KoB21.proc)
fit.KoB21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoB21.proc, ddl = KoB21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoB21.models <- fit.KoB21.model()
KoB21.models
summary(KoB21.models[[1]], se=TRUE)
KoB21.models[[1]]$results$derived

##### MB
# based on weekly averages
# upload and manipulate data
MB21=read.delim("MB21.txt",colClass=c("character","character"))
MB21mut <- mutate(MB21, across(c(ID), as.factor))
str(MB21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
MB21.matrix <- MB21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(MB21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(MB21.matrix, rep(1,nrow(MB21mut)))
# if p>0.05 no evidence for lack of fit
# process data
MB21.proc <- process.data(MB21mut, model = "POPAN")
# Smake design data (from processed data)
MB21.dd <- make.design.data(MB21.proc)
fit.MB21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = MB21.proc, ddl = MB21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
MB21.models <- fit.MB21.model()
MB21.models
summary(MB21.models[[2]], se=TRUE)
MB21.models[[2]]$results$derived

##### VB
# based on weekly averages
# upload and manipulate data
VB21=read.delim("VB21.txt",colClass=c("character","character"))
VB21mut <- mutate(VB21, across(c(ID), as.factor))
str(VB21mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
VB21.matrix <- VB21mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(VB21mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(VB21.matrix, rep(1,nrow(VB21mut)))
# if p>0.05 no evidence for lack of fit
# process data
VB21.proc <- process.data(VB21mut, model = "POPAN")
# Smake design data (from processed data)
VB21.dd <- make.design.data(VB21.proc)
fit.VB21.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = VB21.proc, ddl = VB21.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
VB21.models <- fit.VB21.model()
VB21.models
summary(VB21.models[[1]], se=TRUE)
VB21.models[[1]]$results$derived



######### monthly data
##### KoVK 
# upload and manipulate data
KoVK21b=read.delim("KoVK21b.txt",colClass=c("character","character"))
KoVK21bmut <- mutate(KoVK21b, across(c(ID), as.factor))
str(KoVK21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK21b.matrix <- KoVK21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVK21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK21b.matrix, rep(1,nrow(KoVK21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoVK21b.proc <- process.data(KoVK21bmut, model = "POPAN")
# Smake design data (from processed data)
KoVK21b.dd <- make.design.data(KoVK21b.proc)
fit.KoVK21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoVK21b.proc, ddl = KoVK21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoVK21b.models <- fit.KoVK21b.model()
KoVK21b.models
summary(KoVK21b.models[[2]], se=TRUE)
KoVK21b.models[[2]]$results$derived

##### SG
# based on monhtly averages
# upload and manipulate data
SG21b=read.delim("SG21b.txt",colClass=c("character","character"))
SG21bmut <- mutate(SG21b, across(c(ID), as.factor))
str(SG21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
SG21b.matrix <- SG21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(SG21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(SG21b.matrix, rep(1,nrow(SG21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
SG21b.proc <- process.data(SG21bmut, model = "POPAN")
# Smake design data (from processed data)
SG21b.dd <- make.design.data(SG21b.proc)
fit.SG21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = SG21b.proc, ddl = SG21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
SG21b.models <- fit.SG21b.model()
SG21b.models
summary(SG21b.models[[1]], se=TRUE)
SG21b.models[[1]]$results$derived

##### TG
# based on monthly averages
# upload and manipulate data
TG21b=read.delim("TG21b.txt",colClass=c("character","character"))
TG21bmut <- mutate(TG21b, across(c(ID), as.factor))
str(TG21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TG21b.matrix <- TG21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TG21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TG21b.matrix, rep(1,nrow(TG21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
TG21b.proc <- process.data(TG21bmut, model = "POPAN")
# Smake design data (from processed data)
TG21b.dd <- make.design.data(TG21b.proc)
fit.TG21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TG21b.proc, ddl = TG21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TG21b.models <- fit.TG21b.model()
TG21b.models
summary(TG21b.models[[1]], se=TRUE)
TG21b.models[[1]]$results$derived

##### TT
# based on monthly averages
# upload and manipulate data
TT21b=read.delim("TT21b.txt",colClass=c("character","character"))
TT21bmut <- mutate(TT21b, across(c(ID), as.factor))
str(TT21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TT21b.matrix <- TT21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TT21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TT21b.matrix, rep(1,nrow(TT21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
TT21b.proc <- process.data(TT21bmut, model = "POPAN")
# Smake design data (from processed data)
TT21b.dd <- make.design.data(TT21b.proc)
fit.TT21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TT21b.proc, ddl = TT21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TT21b.models <- fit.TT21b.model()
TT21b.models
summary(TT21b.models[[2]], se=TRUE)
TT21b.models[[2]]$results$derived

#########+ streams
##### KB
# based on monthly averages
# upload and manipulate data
KB21b=read.delim("KB21b.txt",colClass=c("character","character"))
KB21bmut <- mutate(KB21b, across(c(ID), as.factor))
str(KB21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KB21b.matrix <- KB21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KB21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KB21b.matrix, rep(1,nrow(KB21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KB21b.proc <- process.data(KB21bmut, model = "POPAN")
# Smake design data (from processed data)
KB21b.dd <- make.design.data(KB21b.proc)
fit.KB21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KB21b.proc, ddl = KB21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KB21b.models <- fit.KB21b.model()
KB21b.models
summary(KB21b.models[[1]], se=TRUE)
KB21b.models[[1]]$results$derived

##### KoB
# based on monthly averages
# upload and manipulate data
KoB21b=read.delim("KoB21b.txt",colClass=c("character","character"))
KoB21bmut <- mutate(KoB21b, across(c(ID), as.factor))
str(KoB21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoB21b.matrix <- KoB21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoB21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoB21b.matrix, rep(1,nrow(KoB21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoB21b.proc <- process.data(KoB21bmut, model = "POPAN")
# Smake design data (from processed data)
KoB21b.dd <- make.design.data(KoB21b.proc)
fit.KoB21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoB21b.proc, ddl = KoB21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoB21b.models <- fit.KoB21b.model()
KoB21b.models
summary(KoB21b.models[[1]], se=TRUE)
KoB21b.models[[1]]$results$derived

##### MB
# based on monthly averages
# upload and manipulate data
MB21b=read.delim("MB21b.txt",colClass=c("character","character"))
MB21bmut <- mutate(MB21b, across(c(ID), as.factor))
str(MB21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
MB21b.matrix <- MB21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(MB21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(MB21b.matrix, rep(1,nrow(MB21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
MB21b.proc <- process.data(MB21bmut, model = "POPAN")
# Smake design data (from processed data)
MB21b.dd <- make.design.data(MB21b.proc)
fit.MB21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = MB21b.proc, ddl = MB21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
MB21b.models <- fit.MB21b.model()
MB21b.models
summary(MB21b.models[[2]], se=TRUE)
MB21b.models[[2]]$results$derived

##### VB
# based on monthly averages
# upload and manipulate data
VB21b=read.delim("VB21b.txt",colClass=c("character","character"))
VB21bmut <- mutate(VB21b, across(c(ID), as.factor))
str(VB21bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
VB21b.matrix <- VB21bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(VB21bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(VB21b.matrix, rep(1,nrow(VB21bmut)))
# if p>0.05 no evidence for lack of fit
# process data
VB21b.proc <- process.data(VB21bmut, model = "POPAN")
# Smake design data (from processed data)
VB21b.dd <- make.design.data(VB21b.proc)
fit.VB21b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = VB21b.proc, ddl = VB21b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
VB21b.models <- fit.VB21b.model()
VB21b.models
summary(VB21b.models[[1]], se=TRUE)
VB21b.models[[1]]$results$derived




########## ponds 2022
##### KoVK 
# based on weekly averages
# upload and manipulate data
KoVK22=read.delim("KoVK22.txt",colClass=c("character","character"))
KoVK22mut <- mutate(KoVK22, across(c(ID), as.factor))
str(KoVK22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK22.matrix <- KoVK22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVK22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK22.matrix, rep(1,nrow(KoVK22mut)))
# if p>0.05 no evidence for lack of fit
# process data
KoVK22.proc <- process.data(KoVK22mut, model = "POPAN")
# Smake design data (from processed data)
KoVK22.dd <- make.design.data(KoVK22.proc)
fit.KoVK22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoVK22.proc, ddl = KoVK22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoVK22.models <- fit.KoVK22.model()
KoVK22.models
summary(KoVK22.models[[1]], se=TRUE)
KoVK22.models[[1]]$results$derived

##### SG
# based on weekly averages
# upload and manipulate data
SG22=read.delim("SG22.txt",colClass=c("character","character"))
SG22mut <- mutate(SG22, across(c(ID), as.factor))
str(SG22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
SG22.matrix <- SG22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(SG22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(SG22.matrix, rep(1,nrow(SG22mut)))
# if p>0.05 no evidence for lack of fit
# process data
SG22.proc <- process.data(SG22mut, model = "POPAN")
# Smake design data (from processed data)
SG22.dd <- make.design.data(SG22.proc)
fit.SG22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = SG22.proc, ddl = SG22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
SG22.models <- fit.SG22.model()
SG22.models
summary(SG22.models[[1]], se=TRUE)
SG22.models[[1]]$results$derived

##### TG
# based on weekly averages
# upload and manipulate data
WT22=read.delim("WT22.txt",colClass=c("character","character"))
WT22mut <- mutate(WT22, across(c(ID), as.factor))
str(WT22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
WT22.matrix <- WT22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(WT22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(WT22.matrix, rep(1,nrow(WT22mut)))
# if p>0.05 no evidence for lack of fit
# process data
WT22.proc <- process.data(WT22mut, model = "POPAN")
# Smake design data (from processed data)
WT22.dd <- make.design.data(WT22.proc)
fit.WT22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = WT22.proc, ddl = WT22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
WT22.models <- fit.WT22.model()
WT22.models
summary(WT22.models[[4]], se=TRUE)
WT22.models[[4]]$results$derived

##### TT
# based on weekly averages
# upload and manipulate data
TT22=read.delim("TT22.txt",colClass=c("character","character"))
TT22mut <- mutate(TT22, across(c(ID), as.factor))
str(TT22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TT22.matrix <- TT22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TT22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TT22.matrix, rep(1,nrow(TT22mut)))
# if p>0.05 no evidence for lack of fit
# process data
TT22.proc <- process.data(TT22mut, model = "POPAN")
# Smake design data (from processed data)
TT22.dd <- make.design.data(TT22.proc)
fit.TT22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TT22.proc, ddl = TT22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TT22.models <- fit.TT22.model()
TT22.models
summary(TT22.models[[4]], se=TRUE)
TT22.models[[4]]$results$derived

#########+ streams
##### KB
# based on weekly averages
# upload and manipulate data
KB22=read.delim("KB22.txt",colClass=c("character","character"))
KB22mut <- mutate(KB22, across(c(ID), as.factor))
str(KB22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KB22.matrix <- KB22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KB22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KB22.matrix, rep(1,nrow(KB22mut)))
# if p>0.05 no evidence for lack of fit
# process data
KB22.proc <- process.data(KB22mut, model = "POPAN")
# Smake design data (from processed data)
KB22.dd <- make.design.data(KB22.proc)
fit.KB22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KB22.proc, ddl = KB22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KB22.models <- fit.KB22.model()
KB22.models
summary(KB22.models[[4]], se=TRUE)
KB22.models[[4]]$results$derived

##### KoB
# based on weekly averages
# upload and manipulate data
KoB22=read.delim("KoB22.txt",colClass=c("character","character"))
KoB22mut <- mutate(KoB22, across(c(ID), as.factor))
str(KoB22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoB22.matrix <- KoB22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoB22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoB22.matrix, rep(1,nrow(KoB22mut)))
# if p>0.05 no evidence for lack of fit
# process data
KoB22.proc <- process.data(KoB22mut, model = "POPAN")
# Smake design data (from processed data)
KoB22.dd <- make.design.data(KoB22.proc)
fit.KoB22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoB22.proc, ddl = KoB22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoB22.models <- fit.KoB22.model()
KoB22.models
summary(KoB22.models[[4]], se=TRUE)
KoB22.models[[4]]$results$derived

##### MB
# based on weekly averages
# upload and manipulate data
MB22=read.delim("MB22.txt",colClass=c("character","character"))
MB22mut <- mutate(MB22, across(c(ID), as.factor))
str(MB22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
MB22.matrix <- MB22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(MB22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(MB22.matrix, rep(1,nrow(MB22mut)))
# if p>0.05 no evidence for lack of fit
# process data
MB22.proc <- process.data(MB22mut, model = "POPAN")
# Smake design data (from processed data)
MB22.dd <- make.design.data(MB22.proc)
fit.MB22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = MB22.proc, ddl = MB22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
MB22.models <- fit.MB22.model()
MB22.models
summary(MB22.models[[3]], se=TRUE)
MB22.models[[3]]$results$derived

##### VB
# based on weekly averages
# upload and manipulate data
VB22=read.delim("VB22.txt",colClass=c("character","character"))
VB22mut <- mutate(VB22, across(c(ID), as.factor))
str(VB22mut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
VB22.matrix <- VB22mut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(VB22mut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(VB22.matrix, rep(1,nrow(VB22mut)))
# if p>0.05 no evidence for lack of fit
# process data
VB22.proc <- process.data(VB22mut, model = "POPAN")
# Smake design data (from processed data)
VB22.dd <- make.design.data(VB22.proc)
fit.VB22.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = VB22.proc, ddl = VB22.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
VB22.models <- fit.VB22.model()
VB22.models
summary(VB22.models[[4]], se=TRUE)
VB22.models[[4]]$results$derived



########## ponds 2022
##### KoVK 
# based on monthly averages
# upload and manipulate data
KoVK22b=read.delim("KoVK22b.txt",colClass=c("character","character"))
KoVK22bmut <- mutate(KoVK22b, across(c(ID), as.factor))
str(KoVK22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoVK22b.matrix <- KoVK22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoVK22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoVK22b.matrix, rep(1,nrow(KoVK22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoVK22b.proc <- process.data(KoVK22bmut, model = "POPAN")
# Smake design data (from processed data)
KoVK22b.dd <- make.design.data(KoVK22b.proc)
fit.KoVK22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoVK22b.proc, ddl = KoVK22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoVK22b.models <- fit.KoVK22b.model()
KoVK22b.models
summary(KoVK22b.models[[2]], se=TRUE)
KoVK22b.models[[2]]$results$derived

##### SG
# based on monthly averages
# upload and manipulate data
SG22b=read.delim("SG22b.txt",colClass=c("character","character"))
SG22bmut <- mutate(SG22b, across(c(ID), as.factor))
str(SG22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
SG22b.matrix <- SG22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(SG22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(SG22b.matrix, rep(1,nrow(SG22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
SG22b.proc <- process.data(SG22bmut, model = "POPAN")
# Smake design data (from processed data)
SG22b.dd <- make.design.data(SG22b.proc)
fit.SG22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = SG22b.proc, ddl = SG22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
SG22b.models <- fit.SG22b.model()
SG22b.models
summary(SG22b.models[[2]], se=TRUE)
SG22b.models[[2]]$results$derived

##### TG
# based on monthly averages
# upload and manipulate data
WT22b=read.delim("WT22b.txt",colClass=c("character","character"))
WT22bmut <- mutate(WT22b, across(c(ID), as.factor))
str(WT22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
WT22b.matrix <- WT22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(WT22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(WT22b.matrix, rep(1,nrow(WT22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
WT22b.proc <- process.data(WT22bmut, model = "POPAN")
# Smake design data (from processed data)
WT22b.dd <- make.design.data(WT22b.proc)
fit.WT22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = WT22b.proc, ddl = WT22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
WT22b.models <- fit.WT22b.model()
WT22b.models
summary(WT22b.models[[4]], se=TRUE)
WT22b.models[[4]]$results$derived

##### TT
# based on monthly averages
# upload and manipulate data
TT22b=read.delim("TT22b.txt",colClass=c("character","character"))
TT22bmut <- mutate(TT22b, across(c(ID), as.factor))
str(TT22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
TT22b.matrix <- TT22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(TT22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(TT22b.matrix, rep(1,nrow(TT22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
TT22b.proc <- process.data(TT22bmut, model = "POPAN")
# Smake design data (from processed data)
TT22b.dd <- make.design.data(TT22b.proc)
fit.TT22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = TT22b.proc, ddl = TT22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
TT22b.models <- fit.TT22b.model()
TT22b.models
summary(TT22b.models[[4]], se=TRUE)
TT22b.models[[4]]$results$derived

#########+ streams
##### KB
# based on monthly averages
# upload and manipulate data
KB22b=read.delim("KB22b.txt",colClass=c("character","character"))
KB22bmut <- mutate(KB22b, across(c(ID), as.factor))
str(KB22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KB22b.matrix <- KB22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KB22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KB22b.matrix, rep(1,nrow(KB22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KB22b.proc <- process.data(KB22bmut, model = "POPAN")
# Smake design data (from processed data)
KB22b.dd <- make.design.data(KB22b.proc)
fit.KB22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KB22b.proc, ddl = KB22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KB22b.models <- fit.KB22b.model()
KB22b.models
summary(KB22b.models[[4]], se=TRUE)
KB22b.models[[4]]$results$derived

##### KoB
# based on monthly averages
# upload and manipulate data
KoB22b=read.delim("KoB22b.txt",colClass=c("character","character"))
KoB22bmut <- mutate(KoB22b, across(c(ID), as.factor))
str(KoB22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
KoB22b.matrix <- KoB22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(KoB22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(KoB22b.matrix, rep(1,nrow(KoB22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
KoB22b.proc <- process.data(KoB22bmut, model = "POPAN")
# Smake design data (from processed data)
KoB22b.dd <- make.design.data(KoB22b.proc)
fit.KoB22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = KoB22b.proc, ddl = KoB22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
KoB22b.models <- fit.KoB22b.model()
KoB22b.models
summary(KoB22b.models[[4]], se=TRUE)
KoB22b.models[[4]]$results$derived

##### MB
# based on monthly averages
# upload and manipulate data
MB22b=read.delim("MB22b.txt",colClass=c("character","character"))
MB22bmut <- mutate(MB22b, across(c(ID), as.factor))
str(MB22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
MB22b.matrix <- MB22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(MB22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(MB22b.matrix, rep(1,nrow(MB22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
MB22b.proc <- process.data(MB22bmut, model = "POPAN")
# Smake design data (from processed data)
MB22b.dd <- make.design.data(MB22b.proc)
fit.MB22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = MB22b.proc, ddl = MB22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
MB22b.models <- fit.MB22b.model()
MB22b.models
summary(MB22b.models[[4]], se=TRUE)
MB22b.models[[4]]$results$derived

##### VB
# based on monthly averages
# upload and manipulate data
VB22b=read.delim("VB22b.txt",colClass=c("character","character"))
VB22bmut <- mutate(VB22b, across(c(ID), as.factor))
str(VB22bmut)
# first we have to turn our data into a matrix that is needed to work with R2ucare
VB22b.matrix <- VB22bmut$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(VB22bmut))
# now have a look at the goodness of fit, i.e. if the assumptions of equal capture probabilities and survival are correct
overall_CJS(VB22b.matrix, rep(1,nrow(VB22bmut)))
# if p>0.05 no evidence for lack of fit
# process data
VB22b.proc <- process.data(VB22bmut, model = "POPAN")
# Smake design data (from processed data)
VB22b.dd <- make.design.data(VB22b.proc)
fit.VB22b.model <- function(){
  # Phi formulas
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  # p formulas
  p.dot <- list(formula=~1)
  # pent formulas
  pent.time <- list(formula=~-1+time)
  pent.dot <- list(formula=~1)
  # Nsuper formulas
  N.dot <- list(formula=~1)
  cml <- create.model.list("POPAN")
  results <- mark.wrapper(cml, data = VB22b.proc, ddl = VB22b.dd,
                          external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)}
# set directory where Mark-ouptut is saved
setwd("D:/Mark_output")
# Run functions
VB22b.models <- fit.VB22b.model()
VB22b.models
summary(VB22b.models[[4]], se=TRUE)
VB22b.models[[4]]$results$derived




####################################################################################################################################################################################################

# 2. TEMPERATURE REGIMES IN PONDS AND STREAMS 
## import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2022_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

## prepare and inspect data for models
water.sub<- subset(overall.dat, sample.site!="KoB") # exclude KoB, because it's not really a stream nor a pond
str(water.sub)
hist(water.sub$n.total)

## check for normal distribution
plot(water.sub$habitat, water.sub$water.temp)
hist(water.sub$water.temp)
shapiro.test(water.sub$water.temp) # if > 0.05, i.e. NOT significant -> good, normal distribution
var.test(water.sub$water.temp ~ water.sub$habitat) # if > 0.05, i.e. NOT significant -> good, assumption of homogeneity fulfilled

## if data is not normally distributed, try to transform data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_water <- arcsinh_x(water.sub$water.temp))
(boxcox_water <- boxcox(water.sub$water.temp))
(centerscale_water <- center_scale(water.sub$water.temp))
(orderNorm_water <- orderNorm(water.sub$water.temp))
(yeojohnson_water <- yeojohnson(water.sub$water.temp))
(sqrt_water <- sqrt(water.sub$water.temp))
(log_water <- log(water.sub$water.temp))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(water.sub$water.temp)
MASS::truehist(arcsinh_water$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_water$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_water$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_water$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_water$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_water, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_water, main = "log transformation", nbins = 12)

# let R suggest best transformation
bn.water<-bestNormalize(water.sub$water.temp, out_of_sample = FALSE) # out_of_sample = do not perform repeated cross-validation; otherwise you'll get different results every time you run this code
bn.water 

## create an object for the best transformation using the following expression (e.g.) any.name <- orderNorm$x.t  # (i.e result of best normalisation)
(orderNorm_water <- orderNorm(water.sub$water.temp))
on.water <- orderNorm_water$x.t

## add the new object as column to your data frame
water.transformed <- cbind(water.sub, on.water)
hist(water.transformed$on.water)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(water.transformed$on.water)
var.test(water.transformed$on.water ~ water.transformed$habitat)

# run model and investigate model output
library(lme4)
library(lmerTest)
library(performance)
modwat<-lmer(on.water ~ habitat + (1|sample.site/year) + (1|session/year), data=water.transformed)
modwat1<-lmer(on.water ~ habitat + year + (1|sample.site/year) + (1|session/year), data=water.transformed)
modwat2<-lmer(on.water ~ sample.site + (1|session/year), data=water.transformed)
summary(modwat)
summary(modwat1)
summary(modwat2)
AIC(modwat,modwat1,modwat2) # modwat most supported
compare_performance(modwat, modwat1,modwat2,rank=T) # modwat most supported

# check model
check_model(modwat)

# plot as boxplot
library(plyr)
library(ggpubr) 
count(water.sub, "habitat")
water.sub19<- subset(water.sub, year=="2019") 
water.sub20<- subset(water.sub, year=="2020") 
water.sub21<- subset(water.sub, year=="2021") 
water.sub22<- subset(water.sub, year=="2022") 
wilcox.test(water.sub19$water.temp ~water.sub19$habitat,exact=FALSE)
wilcox.test(water.sub20$water.temp ~water.sub20$habitat,exact=FALSE)
wilcox.test(water.sub21$water.temp ~water.sub21$habitat,exact=FALSE)
wilcox.test(water.sub22$water.temp ~water.sub22$habitat,exact=FALSE)

setwd("D:/Plots/Mark_recapture")
png("watertemp.png", height=150, width=200, units="mm", res=300);print(watertemp) #save plot on local harddrive
watertemp<-
  ggplot(data=water.sub, aes(habitat,water.temp), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21,  width = 0.365)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  annotate("text", x = 1.5, y =25, label = "ns", size=3)+
  scale_x_discrete(labels=c("Pond","Stream"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Water temperature (°C)")+
  scale_y_continuous(breaks=seq(0,25,5), limits = c(0, 25))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()




####################################################################################################################################################################################################

# 3. NUMBER OF LARVAE

## import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2022_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

## prepare and inspect data for models
count.sub<- subset(overall.dat, sample.site!="KoB") # exclude KoB, because it's not really a stream nor a pond
str(count.sub)
hist(count.sub$n.total)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_count <- arcsinh_x(count.sub$n.total))
(boxcox_count <- boxcox(count.sub$n.total))
(centerscale_count <- center_scale(count.sub$n.total))
(orderNorm_count <- orderNorm(count.sub$n.total))
(yeojohnson_count <- yeojohnson(count.sub$n.total))
(sqrt_count <- sqrt(count.sub$n.total))
(log_count <- log(count.sub$n.total))
# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(count.sub$n.total)
MASS::truehist(arcsinh_count$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_count$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_count$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_count$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_count$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_count, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_count, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.count<-bestNormalize(count.sub$n.total, out_of_sample = FALSE) 
bn.count 

## create an object for the best transformation
on.count <- orderNorm_count$x.t # squareroot transformation

## add the new object as column to your data frame
count.transformed <- cbind(count.sub, on.count)
str(count.transformed)
hist(count.transformed$on.count)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(count.transformed$on.count)

# run model and investigate model output
library(lme4)
library(lmerTest)
library(performance)
modcount<-lmer(on.count ~ year + (1|sample.site/year), data=count.transformed)
modcount1<-lmer(on.count ~ year + habitat + (1|sample.site/year), data=count.transformed)
modcount2<-lmer(on.count ~ year + (1|sample.site/year) + (1|session/year), data=count.transformed)
modcount3<-lmer(on.count ~ year + habitat+ (1|sample.site/year) + (1|session/year), data=count.transformed)
summary(modcount)
summary(modcount1)
summary(modcount2)
summary(modcount3)
AIC(modcount,modcount1,modcount2,modcount3) # modcount2 most supported
compare_performance(modcount, modcount1,modcount2,modcount3,rank=T) # modcount2 most supported

# check model
check_model(modcount2)



# plot number of larvae as boxplot just for inspection
library(plyr)
library(ggpubr) 
overall.dat <- mutate(overall., across(c(sample.site:habitat, session:observer), as.factor))
str(overall.dat)
count(count.transformed, "habitat")
hist(count.transformed$n.total)
shapiro.test(count.transformed$n.total) # non-normal

count.tab <- ddply(count.sub, c("year"),summarise, 
                  N    = length(!is.na(n.total)),
                  mean = mean(n.total,na.rm=TRUE),
                  sd   = sd(n.total,na.rm=TRUE),
                  max = max(n.total,na.rm=TRUE),
                  min = min (n.total,na.rm=TRUE))

setwd("D:/Plots/Mark_recapture")
png("countboxplot.png", height=150, width=200, units="mm", res=300);print(countplot)
countplot<-
  ggplot(data=count.sub, aes(year,n.total), fill=year)+
  geom_boxplot(aes(fill=year), outlier.shape = NA)+
  geom_jitter(aes(fill=year), shape=21)+
  scale_fill_manual(values=c("steelblue4", "darkseagreen4", "goldenrod", "firebrick1"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("2019","2020", "2021", "2022"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Year", y="Observed number of larvae")+
  scale_y_continuous(breaks=seq(0,200,20), limits = c(0, 200))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()

#get sample sizes
xtabs(n.total~year, overall.dat) 
xtabs(n.total~year+habitat, overall.dat) 


## Plot graphs for each sample site
library(ggplot2)

count.KB<-subset(overall.dat, sample.site=="KB")
str(count.KB)
p1<-ggplot(count.KB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_shape_manual(values=c(16,4,5, 8))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Stream KB")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

count.VB<-subset(overall.dat, sample.site=="VB")
p2<-ggplot(count.VB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Stream VB")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)

count.MB<-subset(overall.dat, sample.site=="MB")
p3<-ggplot(count.MB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4", "goldenrod","firebrick1"))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Observed number of larvae")+
  ggtitle("Stream MB")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

count.KoB<-subset(overall.dat, sample.site=="KoB")
p4<-ggplot(count.KoB, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod","firebrick1"))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Observed number of larvae")+
  ggtitle("(Stream) KoB")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)

count.KoVK<-subset(overall.dat, sample.site=="KoVK")
p5<-ggplot(count.KoVK, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond KoVK")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod","firebrick1"))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

count.TG<-subset(overall.dat, sample.site=="TG")
p6<-ggplot(count.TG, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond TG")+geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod","firebrick1"))+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Pond TG")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

count.SG<-subset(overall.dat, sample.site=="SG")
p7<-ggplot(count.SG, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond SG")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod","firebrick1"))+
  scale_y_continuous(breaks=seq(0,200,50), limits = c(0, 200))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Pond SG")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)

count.TT<-subset(overall.dat, sample.site=="TT")
p8<-ggplot(count.TT, aes(x=date.simplified, y=n.total, group=year, colour=year, shape=year)) +
  ggtitle("Pond TT")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("steelblue4", "darkseagreen4","goldenrod","firebrick1"))+
  scale_y_continuous(breaks=seq(0,200,50), limits = c(0, 200))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Pond TT")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        legend.position="bottom",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.title =element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_blank(),
        legend.text = element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_blank(),)

count.WT<-subset(overall.dat, sample.site=="WT")
p9<-ggplot(count.WT, aes(x=date.simplified, y=n.total)) +
  ggtitle("Pond WT")+
  geom_line(colour="firebrick1",size=0.5)+
  geom_point(shape=8, colour="firebrick1")+
  scale_y_continuous(breaks=seq(0,200,50), limits = c(0, 200))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Pond WT")+
  labs(y="Observed number of larvae")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

# extract legend for multiplot 
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend1<- get_only_legend(p8)


# remove legend from plot 8 

fig8<-p8+
  theme(legend.position = "none")


# create multiplot
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot1<-grid.arrange(p1,p2,p3,p4,p5,p6,p7,fig8,p9, ncol=3)
png("multiplot-count.png", height=200, width=200, units="mm", res=300);print(Multiplot1)
Multiplot1a<-grid.arrange(Multiplot1, legend1, ncol=1, heights = c(10, 1))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.085, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()


####################################################################################################################################################################################################

# 4. MEAN LARVAL SIZES

# import rawdata
Sys.setlocale("LC_TIME", "English") # this helps R to read the right date format from your raw data, if formatted as yyyy-mm-dd
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site, session:observer), as.factor))
str(overall.dat)

# prepare and inspect data
mean.sub<-subset(overall.dat, sample.site!="KoB")
# no need to remove "ssfs", because this was already considered in the raw-data, mean size was calculated without "ssfs"
str(mean.sub)
hist(mean.sub$mean.size)
size.tab <- ddply(mean.sub, c("year","habitat"),summarise, 
            N    = length(!is.na(mean.size)),
            mean = mean(mean.size,na.rm=TRUE),
            sd   = sd(mean.size,na.rm=TRUE),
            max = max(mean.size,na.rm=TRUE),
            min = min (mean.size,na.rm=TRUE))


## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_mean <- arcsinh_x(mean.sub$mean.size))
(boxcox_mean <- boxcox(mean.sub$mean.size))
(centerscale_mean <- center_scale(mean.sub$mean.size))
(orderNorm_mean <- orderNorm(mean.sub$mean.size))
(yeojohnson_mean <- yeojohnson(mean.sub$mean.size))
(sqrt_mean <- sqrt(mean.sub$mean.size))
(log_mean <- log(mean.sub$mean.size))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(mean.sub$mean.size)
MASS::truehist(arcsinh_mean$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_mean$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_mean$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_mean$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_mean$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_mean, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_mean, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.mean<-bestNormalize(mean.sub$mean.size, out_of_sample = FALSE) 
bn.mean 

## create an object for the best transformation
on.mean <- orderNorm_mean$x.t # ordernorm transformation

## add the new object as column to your data frame
mean.transformed <- cbind(mean.sub, on.mean)
str(mean.transformed)
hist(mean.transformed$on.mean)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(mean.transformed$on.mean) 
var.test(mean.transformed$on.mean ~ mean.transformed$habitat) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

msize0<-lm(on.mean~habitat, data=mean.transformed)
msize1<-lm(on.mean~habitat + year, data=mean.transformed)
msize2<-lm(on.mean~habitat +year + water.temp, data=mean.transformed)
msize3<-lmer(on.mean~habitat+year + water.temp + (1|sample.site), data=mean.transformed)
msize4<-lmer(on.mean~habitat+year + water.temp + (1|sample.site)+(1|session/year), data=mean.transformed)
summary(msize0)
summary(msize1)
summary(msize2)
summary(msize3)
summary(msize4)

# check models
AIC(msize0,msize1, msize2,msize3,msize4) # msize4 has lowest AIC
compare_performance(msize0,msize1, msize2,msize3,msize4, rank = T) # msize4 best performance score
check_model(msize4)

# plot mean larval size as boxplot over all years and all sample sites
library(plyr)
library(ggpubr) 
count(mean.sub, "habitat")
hist(mean.sub$mean.size)
shapiro.test(mean.sub$mean.size) # non-normal
var.test(mean.sub$mean.size ~ mean.sub$habitat) #okay

setwd("D:/Plots/Mark_recapture")
png("sizeplot.png", height=150, width=200, units="mm", res=300);print(sizeplot)
sizeplot<-
  ggplot(data=mean.sub, aes(habitat,mean.size), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  annotate("text", x = 1.5, y =5, label = "n.s.")+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Mean larval size (cm)")+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()


## plot graphs for each sample site
mean.KB<-subset(overall.dat, sample.site=="KB")
ps1<-ggplot(mean.KB, aes(x=date.simplified, y=mean.size, group=year, colour=year,shape=year)) +
  ggtitle("Stream KB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.VB<-subset(overall.dat, sample.site=="VB")
ps2<-ggplot(mean.VB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Stream VB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)

mean.MB<-subset(overall.dat, sample.site=="MB")
ps3<-ggplot(mean.MB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Stream MB")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.KoB<-subset(overall.dat, sample.site=="KoB")
ps4<-ggplot(mean.KoB, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("(Stream KoB)")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.KoVK<-subset(overall.dat, sample.site=="KoVK")
ps5<-ggplot(mean.KoVK, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond KoVK")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.TG<-subset(overall.dat, sample.site=="TG")
ps6<-ggplot(mean.TG, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond TG")+geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.SG<-subset(overall.dat, sample.site=="SG")
ps7<-ggplot(mean.SG, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond SG")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)

mean.TT<-subset(overall.dat, sample.site=="TT")
ps8<-ggplot(mean.TT, aes(x=date.simplified, y=mean.size, group=year, colour=year, shape=year)) +
  ggtitle("Pond TT")+
  geom_line(aes(color=year), size=0.5)+
  geom_point(aes(color=year))+
  scale_shape_manual(values=c(16,4,5,8))+
  scale_color_manual(values=c("lightsteelblue4", "darkseagreen4","goldenrod", "firebrick1"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        legend.position="bottom",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.title =element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_blank(),
        legend.text = element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_blank(),)

mean.WT<-subset(overall.dat, sample.site=="WT")
ps9<-ggplot(count.WT, aes(x=date.simplified, y=mean.size)) +
  ggtitle("Pond WT")+
  geom_line(colour="firebrick1",size=0.5)+
  geom_point(shape=8, colour="firebrick1")+
  scale_y_continuous(breaks=seq(0,2,1), limits = c(2, 5))+
  theme_classic(base_size=12, base_family="Arial")+
  ggtitle("Pond WT")+
  labs(y="Mean larval size")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),)


# extract legend for multiplot
legend2 <- get_only_legend(ps8)


# arrange margins of plots 
figps8<-ps8+
  theme(legend.position = "none")



# create multiplot
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot2<-grid.arrange(ps1,ps2,ps3,ps4,ps5,ps6,ps7,figps8,ps9, ncol=3)
png("multiplot-meansize.png", height=200, width=200, units="mm", res=300);print(Multiplot2)
Multiplot2a<-grid.arrange(Multiplot2, legend2, ncol=1, heights = c(10, 1))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.085, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()


# create another plot for the mean sizes in ponds and streams per year
library(ggplot2)
detach("package:plyr", unload = TRUE)
library(tidyverse)
df.mean1<-aggregate(x=mean.sub$mean.size,na.rm=TRUE,
                    by=list(mean.sub$habitat,mean.sub$session, mean.sub$year),
                    FUN=mean)

# rename columns
df.mean<-df.mean1 %>% 
  rename(habitat = Group.1,
    session = Group.2,
    year =Group.3,
    mean.size = x)
str(df.mean)


setwd("D:/Plots/Mark_recapture")
png("meansizeperyear.png", height=150, width=200, units="mm", res=300);print(meanyear)
meanyear<-
  ggplot(data=df.mean, aes(x=factor(session,level = c('1', '2', '3',"4","5", "6","7","8", "9","10","11","12", "13")), 
                           y=mean.size, group=habitat, colour=habitat, shape=habitat)) +
  geom_line(aes(color=habitat), size=0.7)+
  geom_point(aes(color=habitat))+
  facet_wrap(~year)+
  scale_shape_manual(values=c(16,4,5))+
  scale_color_manual(values=c("steelblue4", "lightsteelblue3"))+
  scale_y_continuous(breaks=seq(2,5,1), limits = c(2, 5))+
  scale_fill_discrete(name = "habitat")+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Mean larval size (cm)", x="Monitoring event")+
  theme(axis.text.y=element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_text(family="Arial", size=12, color="black"),
        axis.text.x=element_text(family="Arial", size=12, color="black"),
        axis.title.x=element_text(family="Arial", size=12, color="black"),
        legend.position = "bottom",
        legend.title =element_text(family="Arial", size=12, color="black"),
        legend.text = element_text(family="Arial", size=12, color="black"),)
dev.off()



####################################################################################################################################################################################################

# 5. INDIVIDUAL GROWTH 
# import rawdata
library(readxl)
ind.dat <- read_excel("Ind-sizes.xlsx", na="NA",
                       col_types = c("text", "text", "text","numeric","date","numeric","text","numeric",
                                     "numeric","numeric","numeric","text")) 

## set categories
library(dplyr)
ind.size <- mutate(ind.dat, across(c(ID, sample.site:session, observer, note), as.factor))
str(ind.size)
    
# prepare and inspect data
ind.size1<-subset(ind.size, sample.site!="KoB")
ind.sub<-subset(ind.size1, note!="ssf")
hist(ind.sub$daily.growth)
ind.tab <- ddply(ind.sub, c("year","habitat"),summarise, 
                    N    = length(!is.na(daily.growth)),
                    mean = mean(daily.growth,na.rm=TRUE),
                    sd   = sd(daily.growth,na.rm=TRUE),
                    max = max(daily.growth,na.rm=TRUE),
                    min = min (daily.growth,na.rm=TRUE))

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_ind <- arcsinh_x(ind.sub$daily.growth))
(boxcox_ind <- boxcox(ind.sub$daily.growth))
(centerscale_ind <- center_scale(ind.sub$daily.growth))
(orderNorm_ind <- orderNorm(ind.sub$daily.growth))
(yeojohnson_ind <- yeojohnson(ind.sub$daily.growth))
(sqrt_ind <- sqrt(ind.sub$daily.growth))
(log_ind <- log(ind.sub$daily.growth))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(ind.sub$daily.growth)
MASS::truehist(arcsinh_ind$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_ind$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_ind$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_ind$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_ind$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_ind, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_ind, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.ind<-bestNormalize(ind.sub$daily.growth, out_of_sample = FALSE) 
bn.ind 

## create an object for the best transformation
on.ind <- orderNorm_ind$x.t # ordernorm transformation

## add the new object as column to your data frame
ind.transformed <- cbind(ind.sub, on.ind)
str(ind.transformed)
hist(ind.transformed$on.ind)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(ind.transformed$on.ind) 
var.test(ind.transformed$on.ind ~ ind.transformed$habitat) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

mind0<-lmer(on.ind~habitat+(1|ID)+(1|timespan),data=ind.transformed)
mind1<-lmer(on.ind~habitat+year+(1|ID)+(1|timespan),data=ind.transformed)
mind2<-lmer(on.ind~habitat+year+water.temp+(1|ID)+(1|timespan),data=ind.transformed)
mind3<-lmer(on.ind~habitat+year+water.temp+(1|ID)+(1|timespan)+(1|sample.site),data=ind.transformed)
mind4<-lmer(on.ind~habitat++year+(1|ID)+(1|timespan)+(1|sample.site),data=ind.transformed)
mind5<-lmer(on.ind~habitat+year+water.temp+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind.transformed)
mind6<-lmer(on.ind~habitat+year+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind.transformed)

# warning boundary fit is singular, it might mean that the variances of the random effects are near zero, that is not a problem for the models
# you can check the variances with glmmTMB
# if it's not about the variances there might be another/bigger problem
library(glmmTMB)
glmmTMB(on.ind~habitat+water.temp+(1|ID)+(1|timespan),data=ind.transformed)

AIC(mind0,mind1,mind2,mind3,mind4,mind5,mind6) # mind2 best supported
compare_performance(mind0,mind1,mind2,mind3,mind4,mind5,mind6, rank = T) # mind0,1, 3 and 5 equally supported

check_model(mind2) # looks okay and supported by both methods, take this
summary(mind2)


# look at each year separately
#2021
ind.21<-subset(ind.sub, year=="2021")
hist(ind.21$daily.growth)
shapiro.test(ind.21$daily.growth) 
var.test(ind.21$daily.growth ~ ind.21$habitat)

# find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_ind21 <- arcsinh_x(ind.21$daily.growth))
(boxcox_ind21 <- boxcox(ind.21$daily.growth))
(centerscale_ind21 <- center_scale(ind.21$daily.growth))
(orderNorm_ind21 <- orderNorm(ind.21$daily.growth))
(yeojohnson_ind21 <- yeojohnson(ind.21$daily.growth))
(sqrt_ind21 <- sqrt(ind.21$daily.growth))
(log_ind21 <- log(ind.21$daily.growth))


# let R recommend the most suitable transformation method
bn.ind21<-bestNormalize(ind.21$daily.growth, out_of_sample = FALSE) 
bn.ind21

## create an object for the best transformation
on.ind21 <- orderNorm_ind21$x.t # ordernorm transformation

## add the new object as column to your data frame
ind21.transformed <- cbind(ind.21, on.ind21)
str(ind21.transformed)
hist(ind21.transformed$on.ind21)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(ind21.transformed$on.ind21) 
var.test(ind21.transformed$on.ind21 ~ ind21.transformed$habitat) 

library(lme4)
library(lmerTest)
library(performance)

mind0.21<-lmer(on.ind21~habitat+(1|ID)+(1|timespan),data=ind21.transformed)
mind1.21<-lmer(on.ind21~habitat+water.temp+(1|ID)+(1|timespan),data=ind21.transformed)
mind2.21<-lmer(on.ind21~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site),data=ind21.transformed)
mind3.21<-lmer(on.ind21~habitat+(1|ID)+(1|timespan)+(1|sample.site),data=ind21.transformed)
mind4.21<-lmer(on.ind21~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind21.transformed)
mind5.21<-lmer(on.ind21~habitat+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind21.transformed)

glmmTMB(on.ind21~habitat+water.temp+(1|ID)+(1|timespan),data=ind21.transformed)

AIC(mind0.21,mind1.21,mind2.21,mind3.21,mind4.21,mind5.21) # mind1.21 best supported
compare_performance(mind0.21,mind1.21,mind2.21,mind3.21,mind4.21,mind5.21, rank = T) # mind1.21, and 5.21 equally supported

check_model(mind1.21) # looks okay and supported by both methods, take this
summary(mind1.21)


#2022
ind.22<-subset(ind.sub, year=="2022")
hist(ind.22$daily.growth)
shapiro.test(ind.22$daily.growth) 
var.test(ind.22$daily.growth ~ ind.22$habitat)

# find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_ind22 <- arcsinh_x(ind.22$daily.growth))
(boxcox_ind22 <- boxcox(ind.22$daily.growth))
(centerscale_ind22 <- center_scale(ind.22$daily.growth))
(orderNorm_ind22 <- orderNorm(ind.22$daily.growth))
(yeojohnson_ind22 <- yeojohnson(ind.22$daily.growth))
(sqrt_ind22 <- sqrt(ind.22$daily.growth))
(log_ind22 <- log(ind.22$daily.growth))


# let R recommend the most suitable transformation method
bn.ind22<-bestNormalize(ind.22$daily.growth, out_of_sample = FALSE) 
bn.ind22

## create an object for the best transformation
on.ind22 <- orderNorm_ind22$x.t # ordernorm transformation

## add the new object as column to your data frame
ind22.transformed <- cbind(ind.22, on.ind22)
str(ind22.transformed)
hist(ind22.transformed$on.ind22)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(ind22.transformed$on.ind22) 
var.test(ind22.transformed$on.ind22 ~ ind22.transformed$habitat) 

library(lme4)
library(lmerTest)
library(performance)

mind0.22<-lmer(on.ind22~habitat+(1|ID)+(1|timespan),data=ind22.transformed)
mind1.22<-lmer(on.ind22~habitat+water.temp+(1|ID)+(1|timespan),data=ind22.transformed)
mind2.22<-lmer(on.ind22~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site),data=ind22.transformed)
mind3.22<-lmer(on.ind22~habitat+(1|ID)+(1|timespan)+(1|sample.site),data=ind22.transformed)
mind4.22<-lmer(on.ind22~habitat+water.temp+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind22.transformed)
mind5.22<-lmer(on.ind22~habitat+(1|ID)+(1|timespan)+(1|sample.site)+(1|session),data=ind22.transformed)

glmmTMB(on.ind22~habitat+water.temp+(1|ID)+(1|timespan),data=ind22.transformed)

AIC(mind0.22,mind1.22,mind2.22,mind3.22,mind4.22,mind5.22) # mind0.22 best supported
compare_performance(mind0.22,mind1.22,mind2.22,mind3.22,mind4.22,mind5.22, rank = T) # mind0.22 best supported

check_model(mind0.22) # looks okay and supported by both methods, take this
summary(mind0.22)

# plot individual growth as boxplot
library(readxl)
ind.dat.2 <- read_excel("Individual_growth.xlsx", 
                         col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric", 
                                       "numeric","numeric", "numeric", "numeric","numeric", "numeric","numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","text")) 

library(plyr)
library(ggpubr) 
count(ind.dat.2, "notes")
growtha<-subset(ind.dat.2, notes!="ssf"| is.na(notes))
growth<-subset(growtha, sample.site!="KoB") # remove KoB, since it's in between pond and stream
count(growth, "habitat")
aggregate(growth$daily.growth.rate, by = list(growth$habitat), max)
aggregate(growth$daily.growth.rate, by = list(growth$habitat), min)
aggregate(growth$daily.growth.rate, by = list(growth$habitat), mean, na.rm = TRUE)

hist(growth$daily.growth.rate)
shapiro.test(growth$daily.growth.rate) # non-normal
var.test(growth$daily.growth.rate ~ growth$habitat) # not okay

growthplot<-
  ggplot(data=growth, aes(habitat,daily.growth.rate), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=4, size=1, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond","Stream"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Daily growth rate (cm)")+
  scale_y_continuous(breaks=seq(-0.025,0.05,0.025), limits = c(-0.025, 0.065))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

setwd("D:/Plots/Mark_recapture")
library(gridExtra)
library(grid)
png("growthperyear.png", height=150, width=200, units="mm", res=300);print(growthplot)
growthplot
grid.text("*", x = unit(0.324, "npc"), y = unit(0.92, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
grid.text("ns", x = unit(0.772, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
dev.off()


####################################################################################################################################################################################################

# 6. PERCENTAGE OF INJURED LARVAE
# import rawdata
library(readxl)
overall. <- read_excel("2021_Meandata-Mastertable.xlsx", na="NA",
                       col_types = c("text", "text", "numeric", "date","date", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

## set categories
library(dplyr)
overall.dat <- mutate(overall., across(c(sample.site:year, session:observer), as.factor))
str(overall.dat)

# prepare and inspect data
inj.sub<-subset(overall.dat, sample.site!="KoB")
hist(inj.sub$perc.injured)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_inj <- arcsinh_x(inj.sub$perc.injured))
(boxcox_inj <- boxcox(inj.sub$perc.injured))
(centerscale_inj <- center_scale(inj.sub$perc.injured))
(orderNorm_inj <- orderNorm(inj.sub$perc.injured))
(yeojohnson_inj <- yeojohnson(inj.sub$perc.injured))
(sqrt_inj <- sqrt(inj.sub$perc.injured))
(log_inj <- log(inj.sub$perc.injured))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(inj.sub$perc.injured)
MASS::truehist(arcsinh_inj$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_inj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_inj$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_inj$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_inj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_inj, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_inj, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.inj<-bestNormalize(inj.sub$perc.injured, out_of_sample = FALSE) 
bn.inj 

## create an object for the best transformation
on.inj <- orderNorm_inj$x.t # centerscale transformation

## add the new object as column to your data frame
inj.transformed <- cbind(inj.sub, on.inj)
str(inj.transformed)
hist(inj.transformed$on.inj)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(inj.transformed$on.inj) 
var.test(inj.transformed$on.inj ~ inj.transformed$habitat) 

# transformation did not normalise data
# use other distribution?
library(performance)
mod1<-glm(perc.injured~habitat, data=inj.sub, family=poisson(link="log"))
mod2<-lm(perc.injured~habitat, data=inj.sub)
check_model(mod1)
check_model(mod2)

# Check Model Residuals
library(DHARMa)
mod1.res<- simulateResiduals(mod1)
plot(mod1.res) 
testDispersion(mod1) 
mod2.res<- simulateResiduals(mod2)
plot(mod2.res)
testDispersion(mod2) 

# still doesn't look good, use simple non-parametric test (Wilcoxon)
wilcox.test(inj.sub$perc.injured ~inj.sub$habitat)
kruskal.test(perc.injured ~ year, data = inj.sub)

# plot percentage of injured larvae across all years and sample sites

library(plyr)
library(ggplot2)
library(ggpubr)
injury.tab <- ddply(inj.sub, c("habitat"),summarise, 
                    N    = length(!is.na(perc.injured)),
                    mean = mean(perc.injured,na.rm=TRUE),
                    sd   = sd(perc.injured,na.rm=TRUE),
                    max = max(perc.injured,na.rm=TRUE),
                    min = min (perc.injured,na.rm=TRUE))
injury.tab1 <- ddply(inj.sub, c("year","habitat"),summarise, 
                  N    = length(!is.na(perc.injured)),
                  mean = mean(perc.injured,na.rm=TRUE),
                  sd   = sd(perc.injured,na.rm=TRUE),
                  max = max(perc.injured,na.rm=TRUE),
                  min = min (perc.injured,na.rm=TRUE))

setwd("D:/Plots/Mark_recapture")
png("injuries.png", height=150, width=200, units="mm", res=300);print(injuries)
injuries<-
  ggplot(data=inj.sub, aes(habitat,perc.injured), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21, width=0.365)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=4, size=1, show.legend=FALSE) +
  stat_compare_means(label = "p.signif",label.x = 1.5,method = "wilcox.test", paired = FALSE)+
  scale_x_discrete(labels=c("Pond (N=88)","Stream (N=66)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Percentage of injured larvae")+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()

## models per year
inj2019<-subset(inj.sub, year=="2019")
inj2020<-subset(inj.sub, year=="2020")
inj2021<-subset(inj.sub, year=="2021")
inj2022<-subset(inj.sub, year=="2022")
hist(inj2019$perc.injured)
hist(inj2020$perc.injured)
hist(inj2021$perc.injured)
hist(inj2022$perc.injured)


# find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_inj19 <- arcsinh_x(inj2019$perc.injured))
(arcsinh_inj20 <- arcsinh_x(inj2020$perc.injured))
(arcsinh_inj21 <- arcsinh_x(inj2021$perc.injured))
(arcsinh_inj22 <- arcsinh_x(inj2022$perc.injured))
(boxcox_inj19 <- boxcox(inj2019$perc.injured))
(boxcox_inj20 <- boxcox(inj2020$perc.injured))
(boxcox_inj21 <- boxcox(inj2021$perc.injured))
(boxcox_inj22 <- boxcox(inj2022$perc.injured))
(centerscale_inj19 <- center_scale(inj2019$perc.injured))
(centerscale_inj20 <- center_scale(inj2020$perc.injured))
(centerscale_inj21 <- center_scale(inj2021$perc.injured))
(centerscale_inj22 <- center_scale(inj2022$perc.injured))
(orderNorm_inj19 <- orderNorm(inj2019$perc.injured))
(orderNorm_inj20 <- orderNorm(inj2020$perc.injured))
(orderNorm_inj21 <- orderNorm(inj2021$perc.injured))
(orderNorm_inj22 <- orderNorm(inj2022$perc.injured))
(yeojohnson_inj19 <- yeojohnson(inj2019$perc.injured))
(yeojohnson_inj20 <- yeojohnson(inj2020$perc.injured))
(yeojohnson_inj21 <- yeojohnson(inj2021$perc.injured))
(yeojohnson_inj22 <- yeojohnson(inj2022$perc.injured))
(sqrt_inj19 <- sqrt(inj2019$perc.injured))
(sqrt_inj20 <- sqrt(inj2020$perc.injured))
(sqrt_inj21 <- sqrt(inj2021$perc.injured))
(sqrt_inj22 <- sqrt(inj2022$perc.injured))
(log_inj19 <- log(inj2019$perc.injured))
(log_inj20 <- log(inj2020$perc.injured))
(log_inj21 <- log(inj2021$perc.injured))
(log_inj22 <- log(inj2022$perc.injured))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(inj2019$perc.injured)
hist(inj2020$perc.injured)
hist(inj2021$perc.injured)
hist(inj2022$perc.injured)
MASS::truehist(arcsinh_inj19$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(arcsinh_inj20$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(arcsinh_inj21$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(arcsinh_inj22$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_inj19$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(boxcox_inj20$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(boxcox_inj21$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(boxcox_inj22$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_inj19$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(centerscale_inj20$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(centerscale_inj21$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(centerscale_inj22$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_inj19$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(orderNorm_inj20$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(orderNorm_inj21$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(orderNorm_inj22$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_inj19$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(yeojohnson_inj20$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(yeojohnson_inj21$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(yeojohnson_inj22$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_inj19, main = "squareroot transformation", nbins = 12)
MASS::truehist(sqrt_inj20, main = "squareroot transformation", nbins = 12)
MASS::truehist(sqrt_inj21, main = "squareroot transformation", nbins = 12)
MASS::truehist(sqrt_inj22, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_inj19, main = "log transformation", nbins = 12)
MASS::truehist(log_inj20, main = "log transformation", nbins = 12)
MASS::truehist(log_inj21, main = "log transformation", nbins = 12)
MASS::truehist(log_inj22, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.inj19<-bestNormalize(inj2019$perc.injured, out_of_sample = FALSE) 
bn.inj20<-bestNormalize(inj2020$perc.injured, out_of_sample = FALSE) 
bn.inj21<-bestNormalize(inj2021$perc.injured, out_of_sample = FALSE) 
bn.inj22<-bestNormalize(inj2022$perc.injured, out_of_sample = FALSE) 
bn.inj19 
bn.inj20 
bn.inj21 
bn.inj22 


## create an object for the best transformation
on.inj19 <- orderNorm_inj19$x.t # centerscale transformation
on.inj20 <- orderNorm_inj20$x.t
on.inj21 <- orderNorm_inj21$x.t
on.inj22 <- orderNorm_inj22$x.t

## add the new object as column to your data frame
inj.transformed19 <- cbind(inj2019, on.inj19)
inj.transformed20 <- cbind(inj2020, on.inj20)
inj.transformed21 <- cbind(inj2021, on.inj21)
inj.transformed22 <- cbind(inj2022, on.inj22)
str(inj.transformed19)
str(inj.transformed20)
str(inj.transformed21)
str(inj.transformed22)
hist(inj.transformed19$on.inj19)
hist(inj.transformed20$on.inj20)
hist(inj.transformed21$on.inj21)
hist(inj.transformed22$on.inj22)


# test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(inj.transformed19$on.inj19) 
var.test(inj.transformed19$on.inj19 ~ inj.transformed19$habitat) #shapiro not good, variances okay
shapiro.test(inj.transformed20$on.inj20) 
var.test(inj.transformed20$on.inj20 ~ inj.transformed20$habitat) #both good
shapiro.test(inj.transformed21$on.inj21) 
var.test(inj.transformed21$on.inj21 ~ inj.transformed21$habitat) #shapiro not good, variances okay
shapiro.test(inj.transformed22$on.inj22) 
var.test(inj.transformed22$on.inj22 ~ inj.transformed22$habitat) # both not good

# good for 2020, LME
# set up models
library(lme4)
library(lmerTest)
library(performance)

minj0<-lm(on.inj20~habitat, data=inj.transformed20)
minj1<-lmer(on.inj20~habitat+(1|sample.site), data=inj.transformed20)
minj2<-lmer(on.inj20~habitat+(1|sample.site)+(1|session), data=inj.transformed20)
summary(minj0)
summary(minj1)
summary(minj2)

# check models
AIC(minj0,minj1,minj2) # minj0 has lowest AIC
check_model(minj0) 
compare_performance(minj0,minj1, minj2,rank = T) # minj0 and minj2 both okay


# transformation did not normalise data for 2019, 2021 and 2022
# use simple non-parametric test (Wilcoxon)
wilcox.test(inj2019$perc.injured ~inj2019$habitat,exact=FALSE)
wilcox.test(inj2021$perc.injured ~inj2021$habitat,exact=FALSE)
wilcox.test(inj2022$perc.injured ~inj2022$habitat,exact=FALSE)

# create another plot for the mean sizes in ponds and streams per year
library(ggplot2)
detach("package:plyr", unload = TRUE)
library(tidyverse)
df.inj1<-aggregate(x=inj.sub$perc.injured,na.rm=TRUE,
                    by=list(inj.sub$habitat,inj.sub$session, inj.sub$year),
                    FUN=mean)

# rename columns
df.inj<-df.inj1 %>% 
  rename(habitat = Group.1,
         session = Group.2,
         year =Group.3,
         perc.inj = x)
str(df.inj)

#create boxplot
injuriesyear<-
  ggplot(data=df.inj, aes(habitat,perc.inj), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond","Stream"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Percentage of injured larvae")+
  scale_y_continuous(breaks=seq(0,100,20), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

library(gridExtra)
library(grid)
png("injuriesperyear.png", height=150, width=200, units="mm", res=300);print(injuriesyear)
injuriesyear
grid.text("ns", x = unit(0.307, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
grid.text("ns", x = unit(0.765, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
grid.text("*", x = unit(0.305, "npc"), y = unit(0.465, "npc"), gp=gpar(fontfamily="Arial", fontsize=11))
grid.text("*", x = unit(0.765, "npc"), y = unit(0.465, "npc"), gp=gpar(fontfamily="Arial", fontsize=11))
dev.off()

####################################################################################################################################################################################################

# 7. RECAPTURE RATES

# import rawdata
library(readxl)
recap.surv <- read_excel("recapture<-survival.xlsx", na="NA",
                         col_types = c("text", "text", "numeric", "date", "numeric", "numeric","numeric", "numeric","numeric",
                                       "numeric", "numeric"))

## set categories
library(dplyr)
recap. <- mutate(recap.surv, across(c(sample.site:year), as.factor))
str(recap.)

# prepare and inspect data
recap.sub<-subset(recap., sample.site!="KoB")
hist(recap.sub$r)

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_recap <- arcsinh_x(recap.sub$r))
(boxcox_recap <- boxcox(recap.sub$r))
(centerscale_recap <- center_scale(recap.sub$r))
(orderNorm_recap <- orderNorm(recap.sub$r))
(yeojohnson_recap <- yeojohnson(recap.sub$r))
(sqrt_recap <- sqrt(recap.sub$r))
(log_recap <- log(recap.sub$r))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(recap.sub$r)
MASS::truehist(arcsinh_recap$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_recap$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_recap$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_recap$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_recap$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_recap, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_recap, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.recap<-bestNormalize(recap.sub$r, out_of_sample = FALSE) 
bn.recap

## create an object for the best transformation
cs.recap <- centerscale_recap$x.t # centerscale transformation

## add the new object as column to your data frame
recap.transformed <- cbind(recap.sub, cs.recap)
str(recap.transformed)
hist(recap.transformed$cs.recap)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(recap.transformed$cs.recap) 
var.test(recap.transformed$cs.recap ~ recap.transformed$habitat) 

# transformation did not normalise data
# use other distribution?
library(performance)
library(lme4)
library(lmerTest)

mod3<-lm(r~habitat, data=recap.sub)
mod4<-lm(r~habitat+year, data=recap.sub)
check_model(mod3)
check_model(mod4)

# Check Model Residuals
library(DHARMa)
mod3.res<- simulateResiduals(mod3)
plot(mod3.res) 
testDispersion(mod3) # not good
mod4.res<- simulateResiduals(mod4)
plot(mod4.res)
testDispersion(mod4) # okay

# still not okay, go with non-parametric Wilcoxon test
wilcox.test(recap.sub$r ~recap.sub$habitat,exact=FALSE)
wilcox.test(recap.sub$r ~recap.sub$year,exact=FALSE)
recap.21<-subset(recap.sub, year=="2021")
recap.22<-subset(recap.sub, year=="2022")
shapiro.test(recap.21$r)
var.test(recap.21$r~ recap.21$habitat)
shapiro.test(recap.22$r)
var.test(recap.22$r~ recap.22$habitat)
wilcox.test(recap.21$r ~recap.21$habitat,exact=FALSE)
wilcox.test(recap.22$r ~recap.22$habitat,exact=FALSE)




# plot recapture rates per habitat
library(plyr)
library(ggplot2)
library(ggpubr) 

setwd("D:/Plots/Mark_recapture")
recapsyear<-
  ggplot(data=recap.sub, aes(habitat,(r*100)), fill=habitat)+ # r*100 -> percentage
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond","Stream"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Recapture rate (%)")+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

library(gridExtra)
library(grid)
png("recapturesperyear.png", height=150, width=200, units="mm", res=300);print(recapsyear)
recapsyear
grid.text("ns", x = unit(0.305, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
grid.text("ns", x = unit(0.765, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
dev.off()

####################################################################################################################################################################################################

# 8. SURVIVAL RATES

# import rawdata
library(readxl)
recap.surv <- read_excel("recapture-survival.xlsx", na="NA",
                         col_types = c("text", "text", "numeric", "date", "numeric", "numeric","numeric", "numeric","numeric",
                                       "numeric", "numeric"))

## set categories
library(dplyr)
surv. <- mutate(recap.surv, across(c(sample.site:habitat), as.factor))
str(surv.)

# prepare and inspect data
surv.sub<-subset(surv., sample.site!="KoB")
hist(surv.sub$phi)
library(plyr)
surv.tab <- ddply(surv.sub, c("year","habitat"),summarise, 
                  N    = length(!is.na(phi)),
                  mean = mean(phi,na.rm=TRUE),
                  sd   = sd(phi,na.rm=TRUE),
                  max = max(phi,na.rm=TRUE),
                  min = min (phi,na.rm=TRUE))

## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_surv <- arcsinh_x(surv.sub$phi))
(boxcox_surv <- boxcox(surv.sub$phi))
(centerscale_surv <- center_scale(surv.sub$phi))
(orderNorm_surv <- orderNorm(surv.sub$phi))
(yeojohnson_surv <- yeojohnson(surv.sub$phi))
(sqrt_surv <- sqrt(surv.sub$phi))
(log_surv <- log(surv.sub$phi))
(exp_surv<-exp_x(surv.sub$phi, standardize = TRUE))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(surv.sub$phi)
MASS::truehist(arcsinh_surv$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_surv$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_surv$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_surv$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_surv$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_surv, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_surv, main = "log transformation", nbins = 12)

# let R recommend the most suitable transformation method
bn.surv<-bestNormalize(surv.sub$phi, out_of_sample = FALSE) 
bn.surv

## create an object for the best transformation
on.surv <- orderNorm_surv$x.t # exponential transformation

## add the new object as column to your data frame
surv.transformed <- cbind(surv.sub, on.surv)
str(surv.transformed)
hist(surv.transformed$on.surv)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(surv.transformed$on.surv)
var.test(surv.transformed$phi~ surv.transformed$habitat) 
var.test(surv.transformed$phi~ surv.transformed$year) 

# transformation did not normalise data
# use other distribution?
library(performance)
library(lme4)
library(lmerTest)

mod5<-lm(phi~year, data=surv.sub)
mod6<-lm(phi~habitat, data=surv.sub)
check_model(mod5)
check_model(mod6) # this model looks quite okay and way better than mod5, take this

# Check Model Residuals
library(DHARMa)
mod5.res<- simulateResiduals(mod5)
plot(mod5.res) 
testDispersion(mod5) # not good
mod6.res<- simulateResiduals(mod6)
plot(mod6.res)
testDispersion(mod6) #  QQ-Plot okay, but homogeneity of variances not good

# better go for non-parametric comparison of means 
wilcox.test(surv.sub$phi ~surv.sub$habitat,exact=FALSE)
wilcox.test(surv.sub$phi ~surv.sub$year,exact=FALSE)
surv.21<-subset(surv.sub, year=="2021")
surv.22<-subset(surv.sub, year=="2022")
shapiro.test(surv.21$phi)
var.test(surv.21$phi~ surv.21$habitat)
shapiro.test(surv.22$phi)
var.test(surv.22$phi~ surv.22$habitat)
wilcox.test(surv.21$phi ~surv.21$habitat,exact=FALSE)
wilcox.test(surv.22$phi ~surv.22$habitat,exact=FALSE)

# plot survival per habitat
library(plyr)
library(ggplot2)
library(ggpubr) 

setwd("D:/Plots/Mark_recapture")
survivalyear<-
  ggplot(data=surv.sub, aes(habitat,(phi*100)), fill=habitat)+ # phi*100 -> percentage
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21)+
  facet_wrap(~year)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond","Stream"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Apparent survival rate (%)")+
  scale_y_continuous(breaks=seq(0,100,25), limits = c(0, 100))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")

library(gridExtra)
library(grid)
png("survivalperyear.png", height=150, width=200, units="mm", res=300);print(survivalyear)
survivalyear
grid.text("*", x = unit(0.285, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
grid.text("ns", x = unit(0.758, "npc"), y = unit(0.925, "npc"), gp=gpar(fontfamily="Arial", fontsize=10))
dev.off()



####################################################################################################################################################################################################

# 9. ESTIMATED POPULATION SIZE

#load data
library(readxl)
recap.surv <- read_excel("recapture-survival.xlsx", na="NA")

### set categories
library(dplyr)
nest. <- mutate(recap.surv, across(c(sample.site:year), as.factor))
str(nest.)

# prepare and inspect data
nest.sub<-subset(nest., sample.site!="KoB")
# no need to remove "ssfs", because this was already considered in the raw-data, mean size was calculated without "ssfs"
str(nest.sub)
hist(nest.sub$Nest)
library(plyr)
nest.tab <- ddply(nest.sub, c("year","habitat"),summarise, 
                  N    = length(!is.na(Nest)),
                  mean = mean(Nest,na.rm=TRUE),
                  sd   = sd(Nest,na.rm=TRUE),
                  max = max(Nest,na.rm=TRUE),
                  min = min (Nest,na.rm=TRUE))


## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_nest <- arcsinh_x(nest.sub$Nest))
(boxcox_nest <- boxcox(nest.sub$Nest))
(centerscale_nest <- center_scale(nest.sub$Nest))
(orderNorm_nest <- orderNorm(nest.sub$Nest))
(yeojohnson_nest <- yeojohnson(nest.sub$Nest))
(sqrt_nest <- sqrt(nest.sub$Nest))
(log_nest <- log(nest.sub$Nest))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(nest.sub$Nest)
MASS::truehist(arcsinh_nest$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_nest$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_nest$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_nest$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_nest$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_nest, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_nest, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.nest<-bestNormalize(nest.sub$Nest, out_of_sample = FALSE) 
bn.nest

## create an object for the best transformation
on.nest <- orderNorm_mean$x.t # ordernorm transformation

## add the new object as column to your data frame
nest.transformed <- cbind(nest.sub, on.nest)
str(nest.transformed)
hist(nest.transformed$on.nest)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(nest.transformed$on.nest) 
var.test(nest.transformed$on.nest ~ nest.transformed$year) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

mnest0<-lm(on.nest~year, data=nest.transformed)
mnest1<-lm(on.nest~year+habitat, data=nest.transformed)
mnest2<-lmer(on.nest~year+habitat+(1|sample.site), data=nest.transformed)
mnest3<-lmer(on.nest~year+ habitat+(1|sample.site)+(1|occasion/year), data=nest.transformed)
summary(mnest0)
summary(mnest1)
summary(mnest2)
summary(mnest3)

# check models
AIC(mnest0,mnest1, mnest2, mnest3) # mnest 3 has lowest AIC
check_model(mnest3) 
compare_performance(mnest0,mnest1, mnest2,mnest3, rank = T) # mnest2 best performance score


# plot larval population size as boxplot over all sample sites per year
library(plyr)
library(ggpubr) 
setwd("D:/Plots/Mark_recapture")
png("nestplot.png", height=150, width=200, units="mm", res=300);print(nestplot)
nestplot<-
  ggplot(data=nest.sub, aes(year,Nest), fill=year)+
  geom_boxplot(aes(fill=year), outlier.shape = NA)+
  geom_jitter(aes(fill=year), shape=21)+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  annotate("text", x = 1.5, y =1500, label = "*", size=5.5)+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Year", y="Estimated population size")+
  scale_y_continuous(breaks=seq(0,1500,500), limits = c(0, 1500))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()




###### plot single plots for each sample site
# comparison weekly and monthly 2021
library(readr)
library(ggplot2)
pop.est21 <- read_delim("pop-est-larvae_allinone.csv", delim = ";")

pop.est.1<-subset(pop.est21, KB.week.Nest!="NA")
pop.est.2<-subset(pop.est21, KB.avr.Nest!="NA")
e1<-ggplot(pop.est21, aes(x=date), y=KB.week.Nest,) + ggtitle("Stream KB")+
  geom_ribbon(data=pop.est.1,aes(x=date, ymax=(KB.week.Nest+KB.week.Nest.se), ymin=(KB.week.Nest-KB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.2,aes(x=date, ymax=(KB.avr.Nest+KB.avr.Nest.se),  ymin=(KB.avr.Nest-KB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.1,aes(y=KB.week.Nest),color = "black")+
  geom_path(data=pop.est.1,aes(y=KB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.1,aes(y=(KB.week.Nest+KB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.1,aes(y=(KB.week.Nest-KB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.2,aes(y=KB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.2,aes(y=KB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.2,aes(y=(KB.avr.Nest+KB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.2,aes(y=(KB.avr.Nest-KB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
e1


pop.est.3<-subset(pop.est21, VB.week.Nest!="NA")
pop.est.4<-subset(pop.est21, VB.avr.Nest!="NA")
e2<-ggplot(pop.est21, aes(x=date), y=VB.week.Nest,) +
  ggtitle("Stream VB")+
  geom_ribbon(data=pop.est.3,aes(x=date, ymax=(VB.week.Nest+VB.week.Nest.se), 
                                 ymin=(VB.week.Nest-VB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.4,aes(x=date, ymax=(VB.avr.Nest+VB.avr.Nest.se), 
                                 ymin=(VB.avr.Nest-VB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.3,aes(y=VB.week.Nest),color = "black")+
  geom_path(data=pop.est.3,aes(y=VB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.3,aes(y=(VB.week.Nest+VB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.3,aes(y=(VB.week.Nest-VB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.4,aes(y=VB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.4,aes(y=VB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.4,aes(y=(VB.avr.Nest+VB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.4,aes(y=(VB.avr.Nest-VB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)
e2



pop.est.5<-subset(pop.est21, MB.week.Nest!="NA")
pop.est.6<-subset(pop.est21, MB.avr.Nest!="NA")
e3<-ggplot(pop.est21, aes(x=date), y=MB.week.Nest,) +
  ggtitle("Stream MB")+
  geom_ribbon(data=pop.est.5,aes(x=date, ymax=(MB.week.Nest+MB.week.Nest.se), 
                                 ymin=(MB.week.Nest-MB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.6,aes(x=date, ymax=(MB.avr.Nest+MB.avr.Nest.se), 
                                 ymin=(MB.avr.Nest-MB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.5,aes(y=MB.week.Nest),color = "black")+
  geom_path(data=pop.est.5,aes(y=MB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.5,aes(y=(MB.week.Nest+MB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.5,aes(y=(MB.week.Nest-MB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.6,aes(y=MB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.6,aes(y=MB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.6,aes(y=(MB.avr.Nest+MB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.6,aes(y=(MB.avr.Nest-MB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)
e3

pop.est.7<-subset(pop.est21, KoB.week.Nest!="NA")
pop.est.8<-subset(pop.est21, KoB.avr.Nest!="NA")
e4<-ggplot(pop.est21, aes(x=date), y=KoB.week.Nest,) +
  ggtitle("(Stream) KoB")+
  geom_ribbon(data=pop.est.7,aes(x=date, ymax=(KoB.week.Nest+KoB.week.Nest.se), 
                                 ymin=(KoB.week.Nest-KoB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.7,aes(x=date, ymax=(KoB.avr.Nest+KoB.avr.Nest.se), 
                                 ymin=(KoB.avr.Nest-KoB.avr.Nest.se), alpha=.3), fill="lightgrey")+ 
  geom_point(data=pop.est.7,aes(y=KoB.week.Nest),color = "black")+
  geom_path(data=pop.est.7,aes(y=KoB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.7,aes(y=(KoB.week.Nest+KoB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=(KoB.week.Nest-KoB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=KoB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.7,aes(y=KoB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.7,aes(y=(KoB.avr.Nest+KoB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.7,aes(y=(KoB.avr.Nest-KoB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
e4

pop.est.9<-subset(pop.est21, KoVK.week.Nest!="NA")
pop.est.10<-subset(pop.est21, KoVK.avr.Nest!="NA")
e5<-ggplot(pop.est21, aes(x=date), y=KoVK.week.Nest,) +
  ggtitle("Pond KoVK")+
  geom_ribbon(data=pop.est.9,aes(x=date, ymax=(KoVK.week.Nest+KoVK.week.Nest.se), 
                                 ymin=(KoVK.week.Nest-KoVK.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.10,aes(x=date, ymax=(KoVK.avr.Nest+KoVK.avr.Nest.se), 
                                  ymin=(KoVK.avr.Nest-KoVK.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.9,aes(y=KoVK.week.Nest),color = "black")+
  geom_path(data=pop.est.9,aes(y=KoVK.week.Nest),color = "black")+ 
  geom_path(data=pop.est.9,aes(y=(KoVK.week.Nest+KoVK.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.9,aes(y=(KoVK.week.Nest-KoVK.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.10,aes(y=KoVK.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.10,aes(y=KoVK.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.10,aes(y=(KoVK.avr.Nest+KoVK.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.10,aes(y=(KoVK.avr.Nest-KoVK.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
e5



pop.est.11<-subset(pop.est21, TG.week.Nest!="NA")
pop.est.12<-subset(pop.est21, TG.avr.Nest!="NA")
e6<-ggplot(pop.est21, aes(x=date), y=TG.week.Nest,) +
  ggtitle("Pond TG")+
  geom_ribbon(data=pop.est.11,aes(x=date, ymax=(TG.week.Nest+TG.week.Nest.se), 
                                  ymin=(TG.week.Nest-TG.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.12,aes(x=date, ymax=(TG.avr.Nest+TG.avr.Nest.se), 
                                  ymin=(TG.avr.Nest-TG.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.11,aes(y=TG.week.Nest),color = "black")+
  geom_path(data=pop.est.11,aes(y=TG.week.Nest),color = "black")+ 
  geom_path(data=pop.est.11,aes(y=(TG.week.Nest+TG.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.11,aes(y=(TG.week.Nest-TG.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.12,aes(y=TG.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.12,aes(y=TG.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.12,aes(y=(TG.avr.Nest+TG.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.12,aes(y=(TG.avr.Nest-TG.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
e6



pop.est.13<-subset(pop.est21, SG.week.Nest!="NA")
pop.est.14<-subset(pop.est21, SG.avr.Nest!="NA")
e7<-ggplot(pop.est21, aes(x=date), y=SG.week.Nest,) +
  ggtitle("Pond SG")+
  geom_ribbon(data=pop.est.13,aes(x=date, ymax=(SG.week.Nest+SG.week.Nest.se), 
                                  ymin=(SG.week.Nest-SG.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.14,aes(x=date, ymax=(SG.avr.Nest+SG.avr.Nest.se), 
                                  ymin=(SG.avr.Nest-SG.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.13,aes(y=SG.week.Nest),color = "black")+
  geom_path(data=pop.est.13,aes(y=SG.week.Nest),color = "black")+ 
  geom_path(data=pop.est.13,aes(y=(SG.week.Nest+SG.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.13,aes(y=(SG.week.Nest-SG.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.14,aes(y=SG.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.14,aes(y=SG.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.14,aes(y=(SG.avr.Nest+SG.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.14,aes(y=(SG.avr.Nest-SG.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
e7



pop.est.15<-subset(pop.est21, TT.week.Nest!="NA")
pop.est.16<-subset(pop.est21, TT.avr.Nest!="NA")
e8<-ggplot(pop.est21, aes(x=date), y=TT.week.Nest,) +
  ggtitle("Pond TT")+
  geom_ribbon(data=pop.est.15,aes(x=date, ymax=(TT.week.Nest+TT.week.Nest.se), 
                                  ymin=(TT.week.Nest-TT.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.16,aes(x=date, ymax=(TT.avr.Nest+TT.avr.Nest.se), 
                                  ymin=(TT.avr.Nest-TT.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.15,aes(y=TT.week.Nest),color = "black")+
  geom_path(data=pop.est.15,aes(y=TT.week.Nest),color = "black")+ 
  geom_path(data=pop.est.15,aes(y=(TT.week.Nest+TT.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.15,aes(y=(TT.week.Nest-TT.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.16,aes(y=TT.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.16,aes(y=TT.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.16,aes(y=(TT.avr.Nest+TT.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.16,aes(y=(TT.avr.Nest-TT.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
e8

# get legend
# make plot only for legend
legend5<-ggplot(pop.est21, aes(x=date), y=KoVK.week.n, group=as.factor(period), colour=as.factor(period),shape=as.factor(period))+
  ggtitle("only to grab legend")+ 
  geom_line(aes(y=KoVK.week.n,color=as.factor(period)), size=0.5)+
  geom_point(aes(y=KoVK.week.n,color=as.factor(period), shape=as.factor(period)))+ 
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("darkgrey", "black"))+
  theme_classic(base_size=12, base_family="Arial")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank(),)


get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend5a<- get_only_legend(legend5)


############### 2.1.3 CREATE MULTIPLOT
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot5<-grid.arrange(e1, e2,e3,e4,e5,e6,e7,e8, ncol=4)
png("multiplot-popest21.png", height=175, width=235, units="mm", res=300);print(Multiplot5)
Multiplot5a<-grid.arrange(Multiplot5, legend5a, ncol=1, heights = c(10, 1))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()



# comparison weekly and monthly 2022
library(readr)
library(ggplot2)
pop.est22 <- read_delim("pop-est-larvae_allinone.csv", delim = ";")

pop.est.1b<-subset(pop.est22, KB.week.Nest!="NA")
pop.est.2b<-subset(pop.est22, KB.avr.Nest!="NA")
f1<-ggplot(pop.est22, aes(x=date), y=KB.week.Nest,) + ggtitle("Stream KB")+
  geom_ribbon(data=pop.est.1b,aes(x=date, ymax=(KB.week.Nest+KB.week.Nest.se), ymin=(KB.week.Nest-KB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.2b,aes(x=date, ymax=(KB.avr.Nest+KB.avr.Nest.se),  ymin=(KB.avr.Nest-KB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.1b,aes(y=KB.week.Nest),color = "black")+
  geom_path(data=pop.est.1b,aes(y=KB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.1b,aes(y=(KB.week.Nest+KB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.1b,aes(y=(KB.week.Nest-KB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.2b,aes(y=KB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.2b,aes(y=KB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.2b,aes(y=(KB.avr.Nest+KB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.2b,aes(y=(KB.avr.Nest-KB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
f1


pop.est.3b<-subset(pop.est22, VB.week.Nest!="NA")
pop.est.4b<-subset(pop.est22, VB.avr.Nest!="NA")
f2<-ggplot(pop.est22, aes(x=date), y=VB.week.Nest,) +
  ggtitle("Stream VB")+
  geom_ribbon(data=pop.est.3b,aes(x=date, ymax=(VB.week.Nest+VB.week.Nest.se), 
                                 ymin=(VB.week.Nest-VB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.4b,aes(x=date, ymax=(VB.avr.Nest+VB.avr.Nest.se), 
                                 ymin=(VB.avr.Nest-VB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.3b,aes(y=VB.week.Nest),color = "black")+
  geom_path(data=pop.est.3b,aes(y=VB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.3b,aes(y=(VB.week.Nest+VB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.3b,aes(y=(VB.week.Nest-VB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.4b,aes(y=VB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.4b,aes(y=VB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.4b,aes(y=(VB.avr.Nest+VB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.4b,aes(y=(VB.avr.Nest-VB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)
f2



pop.est.5b<-subset(pop.est22, MB.week.Nest!="NA")
pop.est.6b<-subset(pop.est22, MB.avr.Nest!="NA")
f3<-ggplot(pop.est22, aes(x=date), y=MB.week.Nest,) +
  ggtitle("Stream MB")+
  geom_ribbon(data=pop.est.5b,aes(x=date, ymax=(MB.week.Nest+MB.week.Nest.se), 
                                 ymin=(MB.week.Nest-MB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.6b,aes(x=date, ymax=(MB.avr.Nest+MB.avr.Nest.se), 
                                 ymin=(MB.avr.Nest-MB.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.5b,aes(y=MB.week.Nest),color = "black")+
  geom_path(data=pop.est.5b,aes(y=MB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.5b,aes(y=(MB.week.Nest+MB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.5b,aes(y=(MB.week.Nest-MB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.6b,aes(y=MB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.6b,aes(y=MB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.6b,aes(y=(MB.avr.Nest+MB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.6b,aes(y=(MB.avr.Nest-MB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),)
f3

pop.est.7b<-subset(pop.est22, KoB.week.Nest!="NA")
pop.est.8b<-subset(pop.est22, KoB.avr.Nest!="NA")
f4<-ggplot(pop.est22, aes(x=date), y=KoB.week.Nest,) +
  ggtitle("(Stream) KoB")+
  geom_ribbon(data=pop.est.7b,aes(x=date, ymax=(KoB.week.Nest+KoB.week.Nest.se), 
                                 ymin=(KoB.week.Nest-KoB.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.8b,aes(x=date, ymax=(KoB.avr.Nest+KoB.avr.Nest.se), 
                                 ymin=(KoB.avr.Nest-KoB.avr.Nest.se), alpha=.3), fill="lightgrey")+ 
  geom_point(data=pop.est.7b,aes(y=KoB.week.Nest),color = "black")+
  geom_path(data=pop.est.7b,aes(y=KoB.week.Nest),color = "black")+ 
  geom_path(data=pop.est.7b,aes(y=(KoB.week.Nest+KoB.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.7b,aes(y=(KoB.week.Nest-KoB.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.8b,aes(y=KoB.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.8b,aes(y=KoB.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.8b,aes(y=(KoB.avr.Nest+KoB.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.8b,aes(y=(KoB.avr.Nest-KoB.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,1000,250), limits = c(0, 1000))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
f4

pop.est.9b<-subset(pop.est22, KoVK.week.Nest!="NA")
pop.est.10b<-subset(pop.est22, KoVK.avr.Nest!="NA")
f5<-ggplot(pop.est22, aes(x=date), y=KoVK.week.Nest,) +
  ggtitle("Pond KoVK")+
  geom_ribbon(data=pop.est.9b,aes(x=date, ymax=(KoVK.week.Nest+KoVK.week.Nest.se), 
                                 ymin=(KoVK.week.Nest-KoVK.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.10b,aes(x=date, ymax=(KoVK.avr.Nest+KoVK.avr.Nest.se), 
                                  ymin=(KoVK.avr.Nest-KoVK.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.9b,aes(y=KoVK.week.Nest),color = "black")+
  geom_path(data=pop.est.9b,aes(y=KoVK.week.Nest),color = "black")+ 
  geom_path(data=pop.est.9b,aes(y=(KoVK.week.Nest+KoVK.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.9b,aes(y=(KoVK.week.Nest-KoVK.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.10b,aes(y=KoVK.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.10b,aes(y=KoVK.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.10b,aes(y=(KoVK.avr.Nest+KoVK.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.10b,aes(y=(KoVK.avr.Nest-KoVK.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        axis.title.y=element_text(family="Arial", size=12, color="black"),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
f5



pop.est.11b<-subset(pop.est22, WT.week.Nest!="NA")
pop.est.12b<-subset(pop.est22, WT.avr.Nest!="NA")
f6<-ggplot(pop.est22, aes(x=date), y=WT.week.Nest,) +
  ggtitle("Pond WT")+
  geom_ribbon(data=pop.est.11b,aes(x=date, ymax=(WT.week.Nest+WT.week.Nest.se), 
                                  ymin=(WT.week.Nest-WT.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.12b,aes(x=date, ymax=(WT.avr.Nest+WT.avr.Nest.se), 
                                  ymin=(WT.avr.Nest-WT.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.11b,aes(y=WT.week.Nest),color = "black")+
  geom_path(data=pop.est.11b,aes(y=WT.week.Nest),color = "black")+ 
  geom_path(data=pop.est.11b,aes(y=(WT.week.Nest+WT.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.11b,aes(y=(WT.week.Nest-WT.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.12b,aes(y=WT.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.12b,aes(y=WT.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.12b,aes(y=(WT.avr.Nest+WT.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.12b,aes(y=(WT.avr.Nest-WT.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
f6



pop.est.13b<-subset(pop.est22, SG.week.Nest!="NA")
pop.est.14b<-subset(pop.est22, SG.avr.Nest!="NA")
f7<-ggplot(pop.est22, aes(x=date), y=SG.week.Nest,) +
  ggtitle("Pond SG")+
  geom_ribbon(data=pop.est.13b,aes(x=date, ymax=(SG.week.Nest+SG.week.Nest.se), 
                                  ymin=(SG.week.Nest-SG.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.14b,aes(x=date, ymax=(SG.avr.Nest+SG.avr.Nest.se), 
                                  ymin=(SG.avr.Nest-SG.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.13b,aes(y=SG.week.Nest),color = "black")+
  geom_path(data=pop.est.13b,aes(y=SG.week.Nest),color = "black")+ 
  geom_path(data=pop.est.13b,aes(y=(SG.week.Nest+SG.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.13b,aes(y=(SG.week.Nest-SG.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.14b,aes(y=SG.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.14b,aes(y=SG.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.14b,aes(y=(SG.avr.Nest+SG.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.14b,aes(y=(SG.avr.Nest-SG.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
f7



pop.est.15b<-subset(pop.est22, TT.week.Nest!="NA")
pop.est.16b<-subset(pop.est22, TT.avr.Nest!="NA")
f8<-ggplot(pop.est22, aes(x=date), y=TT.week.Nest,) +
  ggtitle("Pond TT")+
  geom_ribbon(data=pop.est.15b,aes(x=date, ymax=(TT.week.Nest+TT.week.Nest.se), 
                                  ymin=(TT.week.Nest-TT.week.Nest.se), alpha=.3), fill="black")+ 
  geom_ribbon(data=pop.est.16b,aes(x=date, ymax=(TT.avr.Nest+TT.avr.Nest.se), 
                                  ymin=(TT.avr.Nest-TT.avr.Nest.se), alpha=.3), fill="darkgrey")+ 
  geom_point(data=pop.est.15b,aes(y=TT.week.Nest),color = "black")+
  geom_path(data=pop.est.15b,aes(y=TT.week.Nest),color = "black")+ 
  geom_path(data=pop.est.15b,aes(y=(TT.week.Nest+TT.week.Nest.se)),color = "black", linetype="dashed")+
  geom_path(data=pop.est.15b,aes(y=(TT.week.Nest-TT.week.Nest.se)),color = "black",linetype="dashed")+
  geom_path(data=pop.est.16b,aes(y=TT.avr.Nest),color = "darkgrey")+
  geom_point(data=pop.est.16b,aes(y=TT.avr.Nest),color = "darkgrey", shape=2)+
  geom_path(data=pop.est.16b,aes(y=(TT.avr.Nest+TT.avr.Nest.se)),color = "darkgrey", linetype="dashed")+
  geom_path(data=pop.est.16b,aes(y=(TT.avr.Nest-TT.avr.Nest.se)),color = "darkgrey",linetype="dashed")+
  scale_y_continuous(breaks=seq(0,2100,500), limits = c(0, 2100))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-15"), as.Date("2021-08-01")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank(),)
f8

# get legend
# make plot only for legend
legend6<-ggplot(pop.est22, aes(x=date), y=KoVK.week.n, group=as.factor(period), colour=as.factor(period),shape=as.factor(period))+
  ggtitle("only to grab legend")+ 
  geom_line(aes(y=KoVK.week.n,color=as.factor(period)), size=0.5)+
  geom_point(aes(y=KoVK.week.n,color=as.factor(period), shape=as.factor(period)))+ 
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("darkgrey", "black"))+
  theme_classic(base_size=12, base_family="Arial")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank(),)


get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend6a<- get_only_legend(legend6)


############### 2.1.3 CREATE MULTIPLOT
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot6<-grid.arrange(f1, f2,f3,f4,f5,f6,f7,f8, ncol=4)
png("multiplot-popest22.png", height=175, width=235, units="mm", res=300);print(Multiplot6)
Multiplot6a<-grid.arrange(Multiplot6, legend6a, ncol=1, heights = c(10, 1))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()




# comparison of weekly estimates from 2021 and 2022
library(readr)
library(ggplot2)
pop.est.both <- read_delim("pop-est-larvae_allinone.csv", delim = ";")
library(dplyr)
pop.est.both <- mutate(pop.est.both, across(c(year, month:period), as.factor))
str(pop.est.both)

pop.est.1c<-subset(pop.est.both, KB.week.Nest!="NA")
g1<-ggplot(pop.est.1c, aes(y=KB.week.Nest, x=date, group=year, colour=year,
          fill=year, shape=year))+ ggtitle("Stream KB")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(KB.week.Nest-KB.week.Nest.se), ymax=(KB.week.Nest+KB.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g1


pop.est.2c<-subset(pop.est.both, VB.week.Nest!="NA")
g2<-ggplot(pop.est.2c, aes(y=VB.week.Nest, x=date, group=year, colour=year,
                             fill=year, shape=year))+ ggtitle("Stream VB")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(VB.week.Nest-VB.week.Nest.se), ymax=(VB.week.Nest+VB.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g2



pop.est.3c<-subset(pop.est.both, MB.week.Nest!="NA")
g3<-ggplot(pop.est.3c, aes(y=MB.week.Nest, x=date, group=year, colour=year,
                             fill=year, shape=year))+ ggtitle("Stream MB")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(MB.week.Nest-MB.week.Nest.se), ymax=(MB.week.Nest+MB.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g3

pop.est.4c<-subset(pop.est.both, KoB.week.Nest!="NA")
g4<-ggplot(pop.est.4c, aes(y=KoB.week.Nest, x=date, group=year, colour=year,
                             fill=year, shape=year))+ ggtitle("Stream KoB")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(KoB.week.Nest-KoB.week.Nest.se), ymax=(KoB.week.Nest+KoB.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g4


pop.est.5c<-subset(pop.est.both, KoVK.week.Nest!="NA")
g5<-ggplot(pop.est.5c, aes(y=KoVK.week.Nest, x=date, group=year, colour=year,
                             fill=year, shape=year))+ ggtitle("Stream KoVK")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(KoVK.week.Nest-KoVK.week.Nest.se), ymax=(KoVK.week.Nest+KoVK.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g5



pop.est.6c<-subset(pop.est.both, TG.week.Nest!="NA")
g6<-ggplot(pop.est.6c, aes(y=TG.week.Nest, x=date, group=year, colour=year,
                           fill=year, shape=year))+ ggtitle("Stream TG")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(TG.week.Nest-TG.week.Nest.se), ymax=(TG.week.Nest+TG.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g6



pop.est.7c<-subset(pop.est.both, SG.week.Nest!="NA")
g7<-ggplot(pop.est.7c, aes(y=SG.week.Nest, x=date, group=year, colour=year,
                           fill=year, shape=year))+ ggtitle("Stream SG")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(SG.week.Nest-SG.week.Nest.se), ymax=(SG.week.Nest+SG.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g7



pop.est.8c<-subset(pop.est.both, TT.week.Nest!="NA")
g8<-ggplot(pop.est.8c, aes(y=TT.week.Nest, x=date, group=year, colour=year,
                           fill=year, shape=year))+ ggtitle("Stream TT")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(TT.week.Nest-TT.week.Nest.se), ymax=(TT.week.Nest+TT.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g8


pop.est.9c<-subset(pop.est.both, WT.week.Nest!="NA")
g9<-ggplot(pop.est.9c, aes(y=WT.week.Nest, x=date, group=year, colour=year,
                           fill=year, shape=year))+ ggtitle("Stream WT")+
  geom_point(aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_ribbon(aes(ymin=(WT.week.Nest-WT.week.Nest.se), ymax=(WT.week.Nest+WT.week.Nest.se)), alpha=.3, linetype=0)+
  scale_y_continuous(breaks=seq(0,1500,250), limits = c(0, 1500))+
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  scale_fill_manual(values=c("goldenrod", "firebrick1"))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b", limits=c(as.Date("2021-03-01"), as.Date("2021-05-15")))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(y="Estimated number of larvae", x="Month")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(family="Arial", size=12, color="black", hjust = 0.5),
        axis.title.x=element_blank(),)
g9


# get legend
# make plot only for legend
legend7<-ggplot(pop.est.both, aes(x=date), y=KoVK.week.n, group=year, colour=year,shape=year)+
  ggtitle("only to grab legend")+ 
  geom_line(aes(y=KoVK.week.Nest,color=year), size=0.5)+
  geom_point(aes(y=KoVK.week.Nest,color=year, shape=year))+ 
  scale_shape_manual(values=c(16,4))+
  scale_color_manual(values=c("goldenrod", "firebrick1"))+
  theme_classic(base_size=12, base_family="Arial")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank(),)


get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend7a<- get_only_legend(legend7)


############### 2.1.3 CREATE MULTIPLOT
library(gridExtra)
library(grid)
setwd("D:/Plots/Mark_recapture")
Multiplot7<-grid.arrange(g1, g2,g3,g4,g5,g6,g7,g8,g9, ncol=3)
png("multiplot-popestperyear.png", height=200, width=200, units="mm", res=300);print(Multiplot7)
Multiplot7a<-grid.arrange(Multiplot7, legend7a, ncol=1, heights = c(10, 1))
grid.text("Month", x = unit(0.51, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=12))
dev.off()

####### Population size estimate: superpopulation approach

# import rawdata
library(readxl)
super<-read_excel("Nsuper_1_8.xlsx", na="NA",
                  col_types = c("text", "text", "numeric", "numeric"))

## set categories
library(dplyr)
super. <- mutate(super, across(c(sample.site:habitat), as.factor))
str(super.)

# prepare and inspect data
super.sub<-subset(super., sample.site!="KoB")
hist(super.sub$Nsuper)


## find best transformation for data
# first, create objects with the transformations, e.g. logarithm etc.
library(bestNormalize)
(arcsinh_super <- arcsinh_x(super.sub$Nsuper))
(boxcox_super <- boxcox(super.sub$Nsuper))
(centerscale_super <- center_scale(super.sub$Nsuper))
(orderNorm_super <- orderNorm(super.sub$Nsuper))
(yeojohnson_super <- yeojohnson(super.sub$Nsuper))
(sqrt_super <- sqrt(super.sub$Nsuper))
(log_super <- log(super.sub$Nsuper))

# then have a look at the histograms of the different transformations for first impression
par(mfrow = c(2,4)) # display all histograms in one window
hist(super.sub$Nsuper)
MASS::truehist(arcsinh_super$x.t, main = "Arcsinh transformation", nbins = 12) # x.t stands for the transformed variable
MASS::truehist(boxcox_super$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(centerscale_super$x.t, main = "center_scale transformation", nbins = 12)
MASS::truehist(orderNorm_super$x.t, main = "orderNorm transformation", nbins = 12)
MASS::truehist(yeojohnson_super$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(sqrt_super, main = "squareroot transformation", nbins = 12)
MASS::truehist(log_super, main = "log transformation", nbins = 12)
# let R recommend the most suitable transformation method
bn.super<-bestNormalize(super.sub$Nsuper, out_of_sample = FALSE) 
bn.super

## create an object for the best transformation
on.super <- orderNorm_super$x.t # ordernorm transformation

## add the new object as column to your data frame
super.transformed <- cbind(super.sub, on.super)
str(super.transformed)
hist(super.transformed$on.super)

## test for normal distribution and homogeneity of variance of the transformed variable
shapiro.test(super.transformed$on.super) 
var.test(super.transformed$on.super ~ super.transformed$year) 

# set up models
library(lme4)
library(lmerTest)
library(performance)

msuper0<-lm(on.super~year, data=super.transformed)
msuper1<-lm(on.super~year+habitat, data=super.transformed)
msuper2<-lmer(on.super~year+habitat+(1|sample.site), data=super.transformed)
summary(msuper0)
summary(msuper1)
summary(msuper2)

# check models
AIC(msuper0,msuper1, msuper2) # msuper 0 has lowest AIC
check_model(msuper0) 
compare_performance(msuper0,msuper1, msuper2, rank = T) # msuper0 and msuper2 similar performance score



# create plot
library (ggplot2)
library(ggpubr)
setwd("D:/Plots/Mark_recapture")
png("superpopulation.png", height=150, width=200, units="mm", res=300);print(superpopulation) #save plot on local harddrive
superpopulation<-
  ggplot(data=super.sub, aes(habitat, Nsuper), fill=habitat)+
  geom_boxplot(aes(fill=habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=habitat), shape=21, width=0.25)+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"))+
  stat_compare_means(method="t.test", label="p.signif",  label.x = 1.5, label.y = 3300)+ # pvalue=0.034
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  scale_x_discrete(labels=c("Pond (N=4)","Stream (N=3)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat type", y="Superpopulation size")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()



