MyUtils::rmAll()
library(parallel)

sysname = Sys.info()["sysname"]

working.path = ifelse(sysname == "Linux",
                      "/home/lee/Rprojects/NLCtrl2",
                      "C:/Users/lee/OneDrive/Rprojects/NLCtrl2")
try(
  setwd(working.path), silent = T
)

# 使用绝对路径
if (Sys.info()['sysname'] == "Windows") {
  # input ====
  nl.path = "C:/Program Files/NetLogo 6.0.4/app"
  output.dir = "d:/NLCtrl2_output"
} else {
  nl.path = "/home/lee/NetLogo-6.0.1/app"
  output.dir = "/home/lee/Rprojects/NLCtrl2_output"
}

r.source.dir = file.path(working.path, "R")

# set cores 
cores = detectCores();cores
# logical_cores = detectCores()
# if (logical_cores > cores) {
#   cores = ceiling(cores * 1.2)
# };
# cores = 22


source("R/libTickData.R")
source("R/libVariable.R")
source("R/libParamsSet.R")
source("R/libMultiverseData.R")
source("R/libUniverseData.R")

source("R/libNLCtrl.R")
source("R/libRandomHelper.R")
source("R/libPlot.R")
source("R/libDataLoader.R")
source("R/libCheckParams.R")
source("R/libModelRunner.R")
#library(functional)

# starts here ===============================

num.expt = 100

if(num.expt < cores){cores = num.expt}

exptsList = makeExptsList(num.expt, function(expt) {
  # 0 ~ 19的整数，三角分布，不能等于V = 20
  expt$`v0` = (randomIntRangeTri(0, 19))
  
  # 20 ~ 100的整数
  expt$`init-wealth` = (randomIntRange(20, 100))
  
  # 0.01 ~ 0.99
  expt$`p_res` = (randomIntRangeTri(1, 99)) / 100
  
  # 常量/参数 ====
  expt$`misjudge-rate` = 0.05
  expt$`mutation-rate` = 0.001
  expt$`p_res` = 0.5
  expt$`init_pop` = 500
  expt$`max_alpha_beta` = 10
  expt$`min_alpha_beta` = 0.1
  expt$`v` = 20
  expt$`birth-factor` = 1
  
  
  expt$`lock_beta?` = "false"
  
  expt
})

goFun = function(x, vars, play.rounds) {
  
  NLCommand("setup")
  
  reports = unique(c("ticks", vars))
  
  df.col.names = tolower(reports)
  
  NLDoReport(play.rounds, "go-once", reports,
             as.data.frame = TRUE, df.col.names = df.col.names)
  
}


#model.path = file.path(nl.path,"models/Sample Models/Earth Science/Fire.nlogo")
model.path = "/home/lee/NetLogo_models/edm.2.6.2.nlogo"

expt.name = "expt_demo_07"

vars = getGlobalsFromFile(model.path);
vars

play.rounds = 1000

# check params ====
checkParams(nl.path, model.path, exptsList)

# run! ====
rest = doExperiments(
  model.path,
  exptsList,
  goFun = goFun,
  cores = cores,
  expt.name = expt.name,
  vars = vars,
  nl.path = nl.path,
  output.dir = output.dir,
  play.rounds = play.rounds,
  r.source.dir = r.source.dir)



print(rest)


# 
# 
# # subset(rest,density>83)
# # 
# (cut(rest,1:30))
# 
# cut(a,c(1,3,5))
# 
# # 
# # print.Variable
# # 
# # filterByTicks()
# # library(crayon)
# # 
# 
# print.MultiverseData
# 
# cut.MultiverseData
# 
# print.TickData()



# expt.result = loadExpt(expt.name, output.dir)
# print(expt.result)
#cut(expt.result, c(1, 3, 5))


# 10, 10, 17:23
# 12, 09, 16:17
# 14, 09, 15:15
# 16, 08, 14:29
# 18, 08, 14:07
# 20, 08, 13:41
# 22, 08, 13:51



