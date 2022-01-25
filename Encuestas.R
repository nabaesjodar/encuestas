setwd("C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/2021 tablas")
library(readODS)
library(dplyr)
##

encuestas <- read_ods("multidim_quests_bra_arg_ecu.ods")   #incluye cargas de datos en proceso
encuestas <- encuestas[1:111,] #solo estudios cargados completamente

### Biodiversity Friendly Practices ###
dataBFP <- encuestas %>% select(n_crops_tot,wild_veg_edge_incrop,wild_area_property,
                                fertil_yes1_no0, fertil_org1_ino0,
                                pestic_chem_appl_year, pestic_che0_org1)
#n_crops (starts in 1) to binary:
crops_01z <- (dataBFP$n_crops_tot-1)#resto 1: paso "1 cultivo" a categor?a 0. Y "+ de 1 cultivo" a categor?a >0.
dataBFP$crops_01[crops_01z>0] <-1
dataBFP$crops_01[crops_01z==0] <-0  #hay q aclararle, si no pone NA. 


#pestic_chem_appl_year to binary: yes or no pesticide
dataBFP$pestic_01[dataBFP$pestic_chem_appl_year>0] <-1
dataBFP$pestic_01[dataBFP$pestic_chem_appl_year==0] <-0 #hay q aclararle, si no pone NA. 

### Build BFP variable. With all binary 0-1 ###
colnames(dataBFP)

#all BFP (all in apple Arg):
dataBFP <- dataBFP %>% mutate(BFP0= crops_01 + pestic_01+pestic_che0_org1+
                                fertil_yes1_no0+fertil_org1_ino0+
                                wild_veg_edge_incrop+wild_area_property)
#all in coffe:
dataBFP <- dataBFP %>% mutate(BFP1= crops_01 + pestic_01+
                                fertil_yes1_no0+fertil_org1_ino0+
                              wild_veg_edge_incrop+wild_area_property)
#all in beans Ecu:
dataBFP <- dataBFP %>% mutate(BFP2= crops_01+pestic_che0_org1+fertil_org1_ino0)

dataBFP$study <- encuestas$study  #incluyo indicador de estudio.

### Select predictors ###  *(por ahora conservation actions qued? afuera de ambas)
colnames(encuestas)
predictors <- encuestas %>% select(study, owner, education, interact_w_prof, gender_f1_m0,	
                               machin_s0_h1,	assoc_inst, other_income,hive_use, commerce,
                               access_credit,	irrigation,	family_tot_n,	family_female_n,	
                               family_male_n, bee_importance, age, internet_access,	certific,
                               employee_tot_n,	employee_male_n, employee_female_n, 
                               crops_area, property_ha, activs_field0_admin1_both2)

### Regression trees ### 
#subset per study

#Alto Valle _AV
dataBFP_AV <- filter(dataBFP, study == "alto_valle")
predictors_AV <- filter(predictors, study == "alto_valle")
#arrange data for RT for Alto Valle
BFP0 <- dataBFP_AV$BFP0   #response var
RT_AV <- cbind(BFP0, predictors_AV)

#Coffe _CBH
dataBFP_CBH <- filter(dataBFP, study == "coffe_bahia")
predictors_CBH <- filter(predictors, study == "coffe_bahia")
#arrange data for RT for coffe_bahia
BFP1 <- dataBFP_CBH$BFP1   #response var
RT_CBH <- cbind(BFP1, predictors_CBH)

#habas_cotopaxi
dataBFP_HCO <- filter(dataBFP, study == "habas_cotopaxi")
predictors_HCO <- filter(predictors, study == "habas_cotopaxi")
#arrange data for RT for coffe_bahia
BFP2 <- dataBFP_HCO$BFP2   #response var
RT_HCO <- cbind(BFP2, predictors_HCO)


library(rpart)
# grow tree

#Alto Valle _AV
fit <- rpart(BFP0~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+  bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+	employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_AV)
#Coffe _CBH
fit <- rpart(BFP1~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+ bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+	employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_CBH)
#habas_cotopaxi
fit <- rpart(BFP2~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+ bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+	employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_HCO)

#Resulatados
plot(fit)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results  


### PARTY...
install.packages("multcomp")
install.packages("party")
library(party)
fit <- ctree(BFP0~., data=RT_AV)




