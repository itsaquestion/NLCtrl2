# 1000人，500代，生育4 = 2小时
library(MyUtils)

library(parallel)
library(ggplot2)

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
  nl.path = "/home/lee/NetLogo-6.0.4/app"
  output.dir = "/home/lee/Rprojects/NLCtrl2_output"
}

r.source.dir = file.path(working.path, "R")

# set cores 
cores = detectCores();cores
# logical_cores = detectCores()
# if (logical_cores > cores) {
#   cores = ceiling(cores * 1.2)
# };
cores = 11


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

num.expt = 300

if(num.expt < cores){cores = num.expt}

exptsList = makeExptsList(num.expt, function(expt) {
  
  expt$`fixPop?` = "true"
  
  expt$`num_pop` = 1000
  
  expt$`reproduction_rate` = 5
  
  # 初始性别比
  expt$`init_sr` = randomIntRange(100, 300) / 100
  
  
  # 分配方案
  expt$`distribute_factor%` = 1 # randomIntRange(50, 100)/100
  
  expt$`male_h` = 2
  
  expt$`female_h` = 2 #randomIntRange(100, 200) / 100
  
  expt$`beta` = 0.8
  
  expt$`mutation_rate` = 0.001
  
  expt$`learning_rate` = 0.1
  
  expt$`adj_rate` = 0.8
  
  expt$`child_cost%` = 0.05 #randomIntRange(1, 10)/100
  
  expt$`oldage_sup%` = 0.1 #randomIntRange(1, 10)/100
  
  
  # !!! 保留这句 !!! =======
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
model.path = "/home/lee/NetLogo_models/ugt_0.4.3_total_saving_rate.nlogo"

expt.name = "expt_ugt_0.4.3_test_03"

vars = getGlobalsFromFile(model.path);
vars

play.rounds = 1500

# check params ====
checkParams(nl.path, model.path, exptsList)


java_parameters = c("-Xmx2500m")

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
  r.source.dir = r.source.dir,
  java_parameters = java_parameters)


print(rest)



rest = loadExpt(expt.name, output.dir = output.dir)

#
mid_sv = getherVariable(rest,"mid_saving_rate_2") %>%
  filterByTicks(600:1500) %>%
  colMeans()

mean_sv = getherVariable(rest,"mean_saving_rate_2") %>%
  filterByTicks(600:1500) %>%
  colMeans()

init_sr = getherParam(rest,"init_sr")


df = data_frame(init_sr,mid_sv,mean_sv)

lm0 = lm(mid_sv ~ init_sr, data=df)
lm1 = lm(mid_sv ~ init_sr + I(init_sr^2) , data=df)

viewModel(lm1)

lm2 = lm(mean_sv ~ init_sr, data=df)

plot(mid_sv ~ init_sr)
abline(lm0,col="red")

summary(lm1)

plot(mean_sv ~ init_sr)
abline(lm2,col="red")
summary(lm2)

summary(lm1)


#qplot(init_sr, mid_sv, data=df)






i = 25
plot(getherVariable(rest,"mid_saving_rate_2")[,i] ,type="l")
plot(getherVariable(rest,"mean_saving_rate_2")[,i] ,type="l")
plot(getherVariable(rest,"mean_unity_2")[,i] ,type="l")
plot(getherVariable(rest,"sd_unity_2")[,i] ,type="l")
plot(getherVariable(rest,"total_saving_rate")[,i] ,type="l")





# 
# 
# dist = getherParam(rest,"distribute_factor%")
# 
# 
# female_h = getherParam(rest,"female_h")
# 
# cc = getherParam(rest,"child_cost%")
# 
# os = getherParam(rest,"oldage_sup%")
# 
# 
# sds = getherVariable(rest,"sd_saving_rate_2")
# 
# 
# unity = getherVariable(rest,"mean_unity_2")
# 
# plot(msr ~ init_sr) 
# 
# plot(getherVariable(rest,"sd_saving_rate_2")[,2],type="l")
# plot(getherVariable(rest,"sd_saving_rate_2"))
# 
# 
# df = data.frame(msr,dist,init_sr,female_h,cc,os)
# # 
# lm0 = lm(msr ~ init_sr )
# lm1 = lm(msr ~ init_sr + dist + female_h + cc + os )
# lm2 = lm(msr ~ init_sr + dist + female_h + cc + os + init_sr * female_h)
# lm3 = lm(msr ~ init_sr + dist + female_h + cc + os + init_sr * dist)
# lm4 = glm(msr ~ init_sr + dist + female_h + cc + os + init_sr * female_h,family = "binomial")
# lm5 = lm(msr ~ log(init_sr) + log(dist) + log(female_h) + log(cc) + log(os) )
# 
# 
# summary(lm4)
# 
# 
# qplot(b,a)
# 
# # Recover
# rest = recoverTmpData("/home/lee/Rprojects/NLCtrl2_output/expt_ugt_0.4.2_unity_test@20190406_113534")
# 
# 
# viewModel = function(x,...){stargazer::stargazer(x,type="text",omit.stat=c("f","ser"),...)}
# 
# viewModel(lm0,lm1,lm2, lm3)
# 
# library(MyPlot)
# 
# gghist(cc)
# # 
# 
# df2 = filter(df,female_h>1.8,cc<=0.05,os<=0.15)
# nrow(df2)
# 
# plot(msr ~ init_sr,data=df2)



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



