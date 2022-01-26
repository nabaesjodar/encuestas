setwd("C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas")
library(dplyr)

encuestas <- read.csv(file = 'encuestas.csv') #script armado partiendo de la tabla con todos los estudios.
#al final del script: manejo de tablas para GitHub.

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

## Build BFP variable. With all binary 0-1 
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

dataBFP$study_code <- encuestas$study_code  #incluyo indicador de estudio.


### Select predictors ###  *(por ahora conservation actions quedo afuera de ambas)
colnames(encuestas)
predictors <- encuestas %>% select(study_code, owner, education, interact_w_prof, gender_f1_m0,	
                               machin_s0_h1,	assoc_inst, other_income,hive_use, commerce,
                               access_credit,	irrigation,	family_tot_n,	family_female_n,	
                               family_male_n, bee_importance, age, internet_access,	certific,
                               employee_tot_n,	employee_male_n, employee_female_n, 
                               crops_area, property_ha, activs_field0_admin1_both2)


### Regression trees ### 
#subset per study

#Alto Valle _AV
dataBFP_AV <- filter(dataBFP, study_code == "AV") %>% select(-study_code)
predictors_AV <- filter(predictors, study_code == "AV") %>% select(-study_code)
#arrange data for RT for Alto Valle
BFP0 <- dataBFP_AV$BFP0   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_AV <- cbind(BFP0, predictors_AV) %>% filter(!is.na(BFP0)) 

#Coffe _CBH
dataBFP_CBH <- filter(dataBFP, study_code == "CBH")%>% select(-study_code)
predictors_CBH <- filter(predictors, study_code == "CBH")%>% select(-study_code)
#arrange data for RT for coffe_bahia
BFP1 <- dataBFP_CBH$BFP1   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_CBH <- cbind(BFP1, predictors_CBH)%>% filter(!is.na(BFP1)) 

#habas_cotopaxi
dataBFP_HCO <- filter(dataBFP, study_code == "HCO")%>% select(-study_code)
predictors_HCO <- filter(predictors, study_code == "HCO")%>% select(-study_code)
#arrange data for RT for coffe_bahia
BFP2 <- dataBFP_HCO$BFP2   #response var
#junto predics con rta, y elimino NAs en respuesta (para party-ctree):
RT_HCO <- cbind(BFP2, predictors_HCO)%>% filter(!is.na(BFP2)) 

### PRUEBAS DE Regression Trees PRELIMINARES ###
## rPART
library(rpart)

# grow tree
#Alto Valle _AV
fit0 <- rpart(BFP0~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+ bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_AV)
#Coffe _CBH
fit1 <- rpart(BFP1~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+ bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_CBH)
#habas_cotopaxi
fit2 <- rpart(BFP2~owner+ education+ interact_w_prof+ gender_f1_m0+	
               machin_s0_h1+	assoc_inst+ other_income+hive_use+ commerce+
               access_credit+	irrigation+	family_tot_n+	family_female_n+	
               family_male_n+ bee_importance+ age+ internet_access+	certific+   #
               employee_tot_n+	employee_male_n+ employee_female_n+ 
               crops_area+ property_ha+ activs_field0_admin1_both2, 
             method="anova", data=RT_HCO)

#Resultados
plot(fit0)
printcp(fit1) # display the results
plotcp(fit1) # visualize cross-validation results
summary(fit1) # detailed summary of splits
# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit0) # visualize cross-validation results  


### PARTY
install.packages("party")
library(party)
fit1y <- ctree(BFP1~., data=RT_CBH)   #VER lo del character...
plot(fit1y)
summary(fit1y)

### plots de datos ###
library(Hmisc)
hist.data.frame(dataBFP)






#bases para GitHub:
Apple_arg <- filter(encuestas, study_code == "AV")
Coffe_bra <- filter(encuestas, study_code == "CBH")
Beans_ecu <- filter(encuestas, study_code == "HCO")
write.csv(Apple_arg,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Apple_arg.csv", row.names = FALSE)
write.csv(Coffe_bra,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Coffe_bra.csv", row.names = FALSE)
write.csv(Beans_ecu,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\Beans_ecu.csv", row.names = FALSE)

# tabla completa. falta SEGUIR AGREGANDO estudios. 
encuestas1 <- read_ods("encuestas_full.ods")   #incluye cargas de datos en proceso
encuestas1 <- encuestas[1:111,]              #solo estudios cargados completamente.
write.csv(encuestas1,"C:/Users/diego/Dropbox/IRNAD/OBServ/EBD/encuestas\\encuestas.csv", row.names = FALSE)

