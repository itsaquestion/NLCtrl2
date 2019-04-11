MyUtils::rmAll()


sysname = Sys.info()["sysname"]

working.path = ifelse(sysname == "Linux",
                      "/home/rstudio/Rprojects/NLCtrl2",
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
  nl.path = "/home/rstudio/docker_share/NetLogo-6.0.1/app"
  output.dir = "/home/rstudio/docker_share/NLCtrl2_output"
}

r.source.dir = file.path(working.path, "R")

# set cores 
cores = detectCores(logical = F)
logical_cores = detectCores()
if (logical_cores > cores) {
  cores = ceiling(cores * 1.2)
};
cores



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

num.expt = 4

exptsList = makeExptsList(num.expt, function(expt) {
  # 参数表
  ## 初始人数
  expt$density = pickOne(70:80)
  expt
})


model.path = file.path(nl.path,"models/Sample Models/Earth Science/Fire.nlogo")

expt.name = "expt_demo"

vars = getGlobalsFromFile(model.path);
vars

play.rounds = 100

# check params ====
checkParams(nl.path, model.path, exptsList)

# run! ====
rest = doExperiments(
  model.path,
  exptsList,
  goFun = goFun_default,
  cores = cores,
  expt.name = expt.name,
  vars = vars,
  nl.path = nl.path,
  output.dir = output.dir,
  play.rounds = play.rounds,
  r.source.dir = r.source.dir)



print(rest)

a = getherVariable(rest, "burned-trees")

getherParam(rest, "density")
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



expt.result = loadExpt(expt.name, output.dir)
print(expt.result)
#cut(expt.result, c(1, 3, 5))



