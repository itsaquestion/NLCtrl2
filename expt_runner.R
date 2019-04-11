lsr::rmAll(F)
working.path = "/home/lee/Rprojects/NLCtrl2/"
try(
  setwd(working.path),silent = T
)

library(MyPlot)
source("./R/NLCtrl.R")
source("R/OO.R")
source("R/DataSubsetting.R")
source("R/plots.R")
source("R/IO.R")


goFun <- function(x) {
  
  NLCommand("setup")
  
  reporters = read.csv("/home/lee/Rprojects/NLCtrl2/nl_models/reporters_0.2.3.csv",header = F)$V1 %>% 
    as.character()
  
  reports = c("ticks", reporters)
  
  # reports  =  c("ticks",
  #               
  #               "Alpha_mean",
  #               "Beta_mean",
  #               "EDM",
  #               
  #               "A_cost_mean",
  #               "D_cost_mean",
  #               
  #               "Fights_theo",
  #               "Fights_real",
  #               "Matchs",
  #               "Fights_theo_p",
  #               "Fights_real_p",
  #               
  #               "Retreats_theo_p",
  #               "Retreats_real_p",
  #              
  #               
  #               "Profit_mean"
  #               
  # )
  
  df.col.names = tolower(reports)
  
  #data.frame(df.col.names,reports)
  
  
  NLDoReport(1000, "go-once", reports, 
             as.data.frame=TRUE, df.col.names=df.col.names)
}

#Sys.setenv(NETLOGO_PATH = "C:/Program Files/NetLogo 6.0/app")

# input ====
model.path = ("/home/lee/NetLogo_models/edm.2.5.1.nlogo")

num.expt = 600

# setup params list (for every experiments) ====
exptsList = list()
for(i in 1:num.expt){
  expt = makeBaseParamsSet()
  
  # 变量 ====
  # 原则上不能等???0，因此从1???始???
  expt$`p_res` = (randomIntRange(1,100) / 100)
  expt$`v0` = (randomIntRange(1,500) / 100)
  
  # 0.01 ~ 0.1
  expt$`misjudge-rate` = randomIntRange(1,10) / 100
  
  # 0.001 ~ 0.01
  expt$`mutation-rate` = randomIntRange(1,10) / 1000
  
  
  # 不变???
  expt$`init_pop` = 2000
  expt$`max_alpha_beta` = 10
  expt$`min_alpha_beta` = 1
  expt$`v` = 5
  expt$`lock_beta?` = "false"
  
  exptsList[[i]] = expt
}



# load reporters, and make a goFun

cores = detectCores()
rest = doExperiments(model.path,exptsList,
                     goFun, cores=cores,
                     expt.name = "expt_04")


##???文中???
##aabbaabb
print(rest)

# lapply(rest,function(x){
#   getParamsSet(x)
# })
# 
# ex1 = getExperiment(rest,1)
# tail(ex1)

alpha = getherVariable(rest,"alpha_mean") 

beta = getherVariable(rest,"beta_mean")

edm = getherVariable(rest,"edm_mean")

fights_real = getherVariable(rest,"fights_real_p")
plotStat(fights_real)

retreats_a_p = getherVariable(rest,"retreats_a_p")
plotStat(retreats_a_p)


retreats_d_p = getherVariable(rest,"retreats_d_p")
plotStat(retreats_d_p)


multiplot2(
  plotStat(alpha)+ mytheme_none + ggtitle("Alpha"),
  plotStat(beta) + mytheme_none + ggtitle("Beta"),
  plotStat(edm) + mytheme_none + ggtitle("Endowment")
)


plotStat(edm) + mytheme_none + ggtitle("Endowment")


edm_t1000 = getCrossSection(edm,1000)

gghist(edm_t1000,bins = 30) + mytheme + xlab("") +
  ggtitle("Distrbution of Endowment at 1000 rounds")
# 
# p0 = gghist(fights_real$fights_real_p_1000,bins = 30) + mytheme + xlab("") +
#   ggtitle("Distrbution of Fight at 1000 rounds")
# 
# p1 = gghist(retreats_a_p$retreats_a_p_1000,bins = 30) + mytheme + xlab("") +
#   ggtitle("Distrbution of A retreats at 1000 rounds")
# 
# p2 = gghist(retreats_d_p$retreats_d_p_1000,bins = 30) + mytheme + xlab("") +
#   ggtitle("Distrbution of D retreats at 1000 rounds")
# 
# multiplot(p0,p1,p2)
# 
# 
# 
# mean(edm$edm_mean_1000) - 2 * sd(edm$edm_mean_1000)
# 
# rest
# 
# params = getherParam


library(stargazer)
# =================
var.name = "edm_mean"
var.df = getherVariable(rest,var.name)
ticks = c(50,100,200,500,1000)

lapply(ticks,function(t){
  df = data.frame(l_edm = log(getCrossSection(var.df,t))[,1],
                  l_pres = log(getherParam(rest,"p_res")),
                  l_v0 = log(getherParam(rest,"v0")),
                  l_misjudge = log(getherParam(rest,"misjudge-rate")),
                  #l_population = log(getherParam(rest,"num-turtles")),
                  l_mutation = log(getherParam(rest,"mutation-rate"))
  )

  #log(df[,-1])
  lm(l_edm ~ .,data= df)
}) %>% stargazer(type="text",column.labels=paste0("t",ticks),omit.stat=c("f")) #
  #%>% stargazer(column.labels=paste0("t",ticks),omit.stat=c("f"))

# =================
var.name = "fight_real_p"
var.df = getherVariable(rest,var.name)
ticks = c(50,100,200,500,1000)

lapply(ticks,function(t){
  df = data.frame(l_ra = log(getCrossSection(var.df,t))[,1],
                  l_pres = log(getherParam(rest,"p_res")),
                  l_v0 = (log(getherParam(rest,"v0"))),
                  l_misjudge = log(getherParam(rest,"misjudge-rate")),
                  #l_population = log(getherParam(rest,"num-turtles")),
                  l_mutation = log(getherParam(rest,"mutation-rate"))
  )
  
  #log(df[,-1])
  lm(l_ra ~ .,data= df)
}) %>% stargazer(column.labels=paste0("t",ticks),omit.stat=c("f"))
