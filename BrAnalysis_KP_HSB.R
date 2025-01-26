library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)

setwd('E:\\ResearchProject\\Sorowar Sir\\Breast Cancer\\Breast Cancer KP HSB')
BrData <- read.csv("BreastCancerDiagnost_DATA_Raw data.csv")

#1.	Form Number / Identification Number
BrData$Record_ID <- factor(BrData$Record.ID)

# BrData$Survey_Identifier <- BrData$Survey.Identifier #all missing
# BrData$X  #not important
# BrData$Form.no...Patient.ID #not important

#2.	Date of Interview (today’s date)
BrData$Date.of.interview <- BrData$Date_of_interview

#3.	Patient’s Name
BrData$Patients_Name <- factor(BrData$X1..Patient.s.Name)

#4.	Contact: XX, XX
BrData$Contact_Telephone <- factor(BrData$X2..Contact.Telephone)

#9.	Age in years (whole year)
BrData$Age <- BrData$X3..Age..in.whole.year.

BrData$AgeCat[BrData$Age < 40]  = 1
BrData$AgeCat[BrData$Age >= 40 & BrData$Age <=  49]  = 2
BrData$AgeCat[BrData$Age >= 50 & BrData$Age <= 59] = 2
BrData$AgeCat[BrData$Age >= 60] = 3

BrData$AgeCat <- factor(BrData$AgeCat,levels=c(1,2,3),labels = c('<40','40-59','>=60'))
BrData$AgeCat

summary(BrData$AgeCat)
x <- table(BrData$AgeCat)
x
round(prop.table(x),4)*100

#Division
BrData$Division <- factor(BrData$Division)

BrData$Division_cat <- factor(BrData$Division,levels=c("Barisal","Chittagong","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet"),
                        labels = c("Barisal","Chittagong","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet"))
BrData$Division_cat

summary(BrData$Division_cat)
x <- table(BrData$Division_cat)
x
round(prop.table(x),4)*100

#6.	Current place of residence (Home District)
# BrData$Home_district <- factor(BrData$X4..Home.District..permanent.residence.)
# 
# summary(BrData$Home_district)
# x <- table(BrData$Home_district)
# x
# round(prop.table(x),4)*100

#5.	Current place of residence (Rural/Urban)
BrData$residence <- factor(BrData$X5..Location.of.of.residence)
summary(BrData$residence)
BrData$residence <- factor(BrData$X5..Location.of.of.residence,levels=c("Rural","Urban"),labels = c('Rural','Urban'))
summary(BrData$residence)

x <- table(BrData$residence)
x
round(prop.table(x),4)*100

#5.	Current place of residence (Current District)
BrData$Current_eistrict <- factor(BrData$X6..Current.place.of.residence..district.)

#11.	Marital status (single/widowed/never married/ married)
BrData$Marital_status <- factor(BrData$X2..Marital.status)
summary(BrData$Marital_status)
BrData$Marital_status <- factor(BrData$Marital_status,levels=c("Married","Never married","Single","Widowed"),labels = c("Married","Single","Single","Single"))
summary(BrData$Marital_status)

x <- table(BrData$Marital_status)
x
round(prop.table(x),4)*100

#7.	Education (highest level completed) (primary, secondary, higher secondary, university)
BrData$Patients_education <- factor(BrData$X1..Education.completed)
summary(BrData$Patients_education)
BrData$Patients_education <- factor(BrData$Patients_education,levels=c("Graduate","Higher secondary (College)","Illiterate","Primary","Secondary"),labels = c("Secondary/higher","Secondary/higher","Illiterate","Primary","Secondary/higher"))
summary(BrData$Patients_education)

x <- table(BrData$Patients_education)
x
round(prop.table(x),4)*100

#8.	Husband’s education (Primary, secondary, higher secondary, university)
BrData$Husbands_education <- factor(BrData$X3..Husband.s.education..completed.)
summary(BrData$Husbands_education)
BrData$Husbands_education <- factor(BrData$Husbands_education,levels=c("Graduate","Higher secondary (College)","Illiterate","Primary","Secondary"),labels = c("Secondary/higher","Secondary/higher","Illiterate","Primary","Secondary/higher"))
summary(BrData$Husbands_education)

x <- table(BrData$Husbands_education)
x
round(prop.table(x),4)*100

#Monthly.family.income.in.Taka
BrData$Family_income <- factor(BrData$X4..Monthly.family.income.in.Taka)
summary(BrData$Family_income)
BrData$Family_income <- factor(BrData$Family_income,levels=c("<5,000","10,000","20,000","Others"),labels = c("<5,000","10,000","20,000",">20,000"))
summary(BrData$Family_income)

x <- table(BrData$Family_income)
x
round(prop.table(x),4)*100

#Monthly.income.Others
BrData$Others_income <- factor(BrData$Monthly.income.Others)

#5. Access to communication and media (choice=Mobile)
summary(BrData$X5..Access.to.communication.and.media..choice.Mobile.)
BrData$PortableElectronicDevices_mobile <- factor(BrData$X5..Access.to.communication.and.media..choice.Mobile.,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$PortableElectronicDevices_mobile)

x <- table(BrData$PortableElectronicDevices_mobile)
x
round(prop.table(x),4)*100

#5. Access to communication and media (choice=Smartphone)
summary(BrData$X5..Access.to.communication.and.media..choice.Smartphone.)
BrData$PortableElectronicDevices_smartphone <- factor(BrData$X5..Access.to.communication.and.media..choice.Smartphone.,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$PortableElectronicDevices_smartphone)

x <- table(BrData$PortableElectronicDevices_smartphone)
x
round(prop.table(x),4)*100

#5. Access to communication and media (choice=Personal Computer (PC))
summary(BrData$X5..Access.to.communication.and.media..choice.Personal.Computer..PC..)
BrData$PortableElectronicDevices_pc <- factor(BrData$X5..Access.to.communication.and.media..choice.Personal.Computer..PC..,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$PortableElectronicDevices_pc)

x <- table(BrData$PortableElectronicDevices_pc)
x
round(prop.table(x),4)*100

#Portable Electronic Devices
summary(BrData$PortableElectronicDevices)
BrData$PortableElectronicDevices <- factor(BrData$PortableElectronicDevices,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$PortableElectronicDevices)

x <- table(BrData$PortableElectronicDevices)
x
round(prop.table(x),4)*100

#Social Media
BrData$social_media_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Social.media.)
BrData$socialmedia_social <- factor(BrData$X5..Access.to.communication.and.media..choice.Social.media.,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$socialmedia_social)

x <- table(BrData$socialmedia_social)
x
round(prop.table(x),4)*100

#TV
BrData$TV_access <- factor(BrData$X5..Access.to.communication.and.media..choice.TV.)
BrData$socialmedia_TV <- factor(BrData$X5..Access.to.communication.and.media..choice.TV.,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$socialmedia_TV)

x <- table(BrData$socialmedia_TV)
x
round(prop.table(x),4)*100

#Newspaper
BrData$Newspaper_access <- factor(BrData$X5..Access.to.communication.and.media..choice.Newspaper.)
BrData$socialmedia_news <- factor(BrData$X5..Access.to.communication.and.media..choice.Newspaper.,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$socialmedia_news)

x <- table(BrData$socialmedia_news)
x
round(prop.table(x),4)*100

#Mass Media Access
summary(BrData$MassMediaAccess)
BrData$MassMediaAccess <- factor(BrData$MassMediaAccess,levels=c("Unchecked","Checked"),labels = c("Unchecked","Checked"))
summary(BrData$MassMediaAccess)

x <- table(BrData$MassMediaAccess)
x
round(prop.table(x),4)*100

#Lump
BrData$symptom_Lump_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Lump.)
BrData$symptom_Lump_br

summary(BrData$symptom_Lump_br)
x <- table(BrData$symptom_Lump_br)
x
round(prop.table(x),4)*100

#Breast Pain
BrData$symptom_breastPain_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Breast.pain.)

summary(BrData$symptom_breastPain_br)
x <- table(BrData$symptom_breastPain_br)
x
round(prop.table(x),4)*100

#Nipple discharge
BrData$symptom_nippleDischarge_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Nipple.discharge.)

summary(BrData$symptom_nippleDischarge_br)
x <- table(BrData$symptom_nippleDischarge_br)
x
round(prop.table(x),4)*100

#Skin changes
BrData$symptom_SkinChanges_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Skin.changes.)

summary(BrData$symptom_SkinChanges_br)
x <- table(BrData$symptom_SkinChanges_br)
x
round(prop.table(x),4)*100

#Bone Pain
BrData$symptom_BonePain_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Bone.pain.)

summary(BrData$symptom_BonePain_br)
x <- table(BrData$symptom_BonePain_br)
x
round(prop.table(x),4)*100

#Others
BrData$symptom_Others_br <- factor(BrData$X1.3.2.What.was.the.first.symptom.noticed...choice.Other.)

summary(BrData$symptom_Others_br)
x <- table(BrData$symptom_Others_br)
x
round(prop.table(x),4)*100

#anysymptoms
summary(BrData$AnySymptoms)
x <- table(BrData$AnySymptoms)
x
round(prop.table(x),4)*100

#Others_name
BrData$symptom_Othersname_br <- factor(BrData$If.other..please.specify.other.symptoms.noticed)





#2.4 Have you experienced following discomfort? 


#c.	Pain in arm on the same side as the affected? (Yes/No)
BrData$discomfort_armPain_br <- factor(BrData$Pain.in.arm.on.the.same.side.as.the.affected.)
BrData$discomfort_armPain_br <- factor(BrData$discomfort_armPain_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_armPain_br)

summary(BrData$discomfort_armPain_br)
x <- table(BrData$discomfort_armPain_br)
x
round(prop.table(x),4)*100

#b.	Pain in breast  (Yes/No)
BrData$discomfort_breastPain_br <- factor(BrData$Pain.in.breast)
BrData$discomfort_breastPain_br <- factor(BrData$discomfort_breastPain_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_breastPain_br)

summary(BrData$discomfort_breastPain_br)
x <- table(BrData$discomfort_breastPain_br)
x
round(prop.table(x),4)*100

#f.	Itching in the breast?(Yes/No)
BrData$discomfort_Itching_br <- factor(BrData$Itching.in.the.breast.)
BrData$discomfort_Itching_br <- factor(BrData$discomfort_Itching_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_Itching_br)

summary(BrData$discomfort_Itching_br)
x <- table(BrData$discomfort_Itching_br)
x
round(prop.table(x),4)*100

#a.	Lump in the armpit, neck or trunk? Yes/No 
BrData$discomfort_Lump_br <- factor(BrData$Lump.in.the.armpit..neck.or.trunk.)
BrData$discomfort_Lump_br <- factor(BrData$discomfort_Lump_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_Lump_br)

summary(BrData$discomfort_Lump_br)
x <- table(BrData$discomfort_Lump_br)
x
round(prop.table(x),4)*100

#d.	Color changes in the breast skin (like red, brown or purple)? (Yes/No)
BrData$discomfort_skinColor_br <- factor(BrData$Color.changes.in.the.breast.skin..like.red..brown.or.purple..)
BrData$discomfort_skinColor_br <- factor(BrData$discomfort_skinColor_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_skinColor_br)

summary(BrData$discomfort_skinColor_br)
x <- table(BrData$discomfort_skinColor_br)
x
round(prop.table(x),4)*100

#e.	Ulcer or sore on the skin of the breast? (Yes/No)
BrData$discomfort_skinUlcer_br <- factor(BrData$Ulcer.or.sore.on.the.skin.of.the.breast.)
BrData$discomfort_skinUlcer_br <- factor(BrData$discomfort_skinUlcer_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_skinUlcer_br)

summary(BrData$discomfort_skinUlcer_br)
x <- table(BrData$discomfort_skinUlcer_br)
x
round(prop.table(x),4)*100

#g.	Changes in breast shape?(Yes/No)
BrData$discomfort_shape_br <- factor(BrData$Changes.in.breast.shape.)
BrData$discomfort_shape_br <- factor(BrData$discomfort_shape_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_shape_br)

summary(BrData$discomfort_shape_br)
x <- table(BrData$discomfort_shape_br)
x
round(prop.table(x),4)*100

#h.	Liquid or blood came out from the nipple?(Yes/No)
BrData$discomfort_liquidBlood_br <- factor(BrData$Liquid.or.blood.came.out.from.the.nipple.)
BrData$discomfort_liquidBlood_br <- factor(BrData$discomfort_liquidBlood_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$discomfort_liquidBlood_br)

summary(BrData$discomfort_liquidBlood_br)
x <- table(BrData$discomfort_liquidBlood_br)
x
round(prop.table(x),4)*100

#Total_Delay
BrData$TD <- factor(BrData$TD,levels=c("Yes","No"),labels = c("Yes","No"))
BrData$TD
summary(BrData$TD)
x <- table(BrData$TD)
x
round(prop.table(x),4)*100

#3.1 What medical centerdid you visit before coming to the cancer treatment centre?  (Select ONE)
BrData$firstHF <- factor(BrData$First.medical.center)
BrData$firstHF <- factor(BrData$First.medical.center,
                         levels=c("General hospital","Others","Pharmacy",
                                  "Private clinic/hospital","Upazila health complex"),
                         labels = c("General hospital","Others","Others",
                                    "Private clinic/hospital","General hospital"))
summary(BrData$firstHF)
x <- table(BrData$firstHF)
x
round(prop.table(x),4)*100

BrData$remedy <- factor(BrData$X3.3.Have.you.tried.to.treat.at.home.or.taken.alternative.remedy.for.this.problem.)
BrData$remedy <- factor(BrData$remedy,
                         levels=c("Yes","No"),
                         labels = c("Yes","No"))
summary(BrData$remedy)
x <- table(BrData$remedy)
x
round(prop.table(x),4)*100


#Family a support  
#4.1 Who is the person you talked first about your health problem? (Select ONE)
BrData$FamilySupport_firstTalked <- factor(BrData$X4.1.Who.is.the.person.you.talked.first.about.your.health.problem.)
BrData$FamilySupport_firstTalked <- factor(BrData$FamilySupport_firstTalked,
                        levels=c("Friend",   "Husband",    "Mother", "Neighbour",
                                 "Other",   "Sibling", "son or daughter"),
                        labels = c("Other",   "Husband",    "Mother", "Neighbour",
                                   "Other",   "Other", "son or daughter"))
summary(BrData$FamilySupport_firstTalked)
x <- table(BrData$FamilySupport_firstTalked)
x
round(prop.table(x),4)*100

#4.2 Who recommended you to consult with a doctor?  (Select ONE)
BrData$FamilySupport_WhoRecommendDoc <- factor(BrData$X4.2.Who.recommended.you.to.consult.with.a.doctor.)
BrData$FamilySupport_WhoRecommendDoc <- factor(BrData$FamilySupport_WhoRecommendDoc,
                                           levels=c("Friend", "herself",  "Husband",    "Mother", "Neighbour",
                                                    "Other",   "Sibling", "son or daughter"),
                                           labels = c("Other",  "herself", "Husband",    "Other", "Neighbour",
                                                      "Other",   "Other", "son or daughter"))
summary(BrData$FamilySupport_WhoRecommendDoc)
x <- table(BrData$FamilySupport_WhoRecommendDoc)
x
round(prop.table(x),4)*100

#4.3 Did you fear or uncomfortable to tell about the problem to your spouse? Yes/No
BrData$FamilySupport_uncomfortabletoShareHusband <- factor(BrData$X4.3.Did.you.fear.or.uncomfortable.to.tell.about.the.problem.to.your.spouse.)
BrData$FamilySupport_uncomfortabletoShareHusband <- factor(BrData$FamilySupport_uncomfortabletoShareHusband,
                        levels=c("Yes","No"),
                        labels = c("Yes","No"))
summary(BrData$FamilySupport_uncomfortabletoShareHusband)
x <- table(BrData$FamilySupport_uncomfortabletoShareHusband)
x
round(prop.table(x),4)*100

#4.4 Did you receive support from spouse after diagnosis?  yes/no
BrData$FamilySupport_supportHusband <- factor(BrData$X4.4.Did.you.receive.support.from.spouse.after.diagnosis.)
BrData$FamilySupport_supportHusband <- factor(BrData$FamilySupport_supportHusband,
                                                           levels=c("Yes","No"),
                                                           labels = c("Yes","No"))
summary(BrData$FamilySupport_supportHusband)
x <- table(BrData$FamilySupport_supportHusband)
x
round(prop.table(x),4)*100

#4.5 If no, did you receive negative behavior from spouse? (Yes/No)
BrData$FamilySupport_supportHusband_negative <- factor(BrData$X4.5.If.no..did.you.receive.negative.behavior.from.spouse.)

#4.6 Did you receive support from social circle?  yes/no
BrData$FamilySupport_supportSocialCircle <- factor(BrData$X4.6.Did.you.receive.support.from.social.circle.)
BrData$FamilySupport_supportSocialCircle <- factor(BrData$FamilySupport_supportSocialCircle,
                                              levels=c("Yes","No"),
                                              labels = c("Yes","No"))
summary(BrData$FamilySupport_supportSocialCircle)
x <- table(BrData$FamilySupport_supportSocialCircle)
x
round(prop.table(x),4)*100

#Cancer Stage
BrData$pathos_cancerStage <- factor(BrData$Stage.of.cancer)
summary(BrData$pathos_cancerStage)
BrData$pathos_cancerStage <- factor(BrData$pathos_cancerStage,levels=c("Stage I","Stage II","Stage III","Stage IV"),
                                    labels = c("Stage I","Stage II","Stage III","Stage IV"))
summary(BrData$pathos_cancerStage)
x <- table(BrData$pathos_cancerStage)
x
round(prop.table(x),4)*100

#5. Knowledge and practices of early detection of cancer

#2.1 When you noticed symptom for the first time did you think that it could be cancer? 
BrData$symptom_cancer_br <- factor(BrData$X2.1.When.you.noticed.symptom.for.the.first.time.did.you.think.that.it.could.be.cancer.)
BrData$symptom_cancer_br <- factor(BrData$symptom_cancer_br,levels=c("No","Yes"),labels = c("No","Yes"))

summary(BrData$symptom_cancer_br)
x <- table(BrData$symptom_cancer_br)
x
round(prop.table(x),4)*100

#5.1 Did you usually check your own breasts? (Yes/No)
BrData$KP_checkBreast <- factor(BrData$X5.1.Did.you.usually.check.your.own.breasts.)
summary(BrData$KP_checkBreast)
BrData$KP_checkBreast <- factor(BrData$KP_checkBreast,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_checkBreast)

x <- table(BrData$KP_checkBreast)
x
round(prop.table(x),4)*100

#After noticing any symptoms, do you go to the doctor immediately?
BrData$DoctorVisitImmediately <- factor(BrData$DoctorVisitImmediately,levels=c("No","Yes"),labels = c("No","Yes"))

summary(BrData$DoctorVisitImmediately)
x <- table(BrData$DoctorVisitImmediately)
x
round(prop.table(x),4)*100

#5.2 Before this health problem, did a doctor or nurse check your breasts Yes/No
BrData$KP_checkBreast_Doc_Nurse <- factor(BrData$X5.2.Before.this.health.problem..did.a.doctor.or.nurse.check.your.breasts.)
summary(BrData$KP_checkBreast_Doc_Nurse)
BrData$KP_checkBreast_Doc_Nurse <- factor(BrData$KP_checkBreast_Doc_Nurse,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_checkBreast_Doc_Nurse)

x <- table(BrData$KP_checkBreast_Doc_Nurse)
x
round(prop.table(x),4)*100

#5.3 Before your breast problem have you heard of mammography or mammogram? Yes/No
BrData$KP_heardMammogram <- factor(BrData$X5.3.Before.your.breast.problem.have.you.heard.of.mammography.or.mammogram.)
summary(BrData$KP_heardMammogram)
BrData$KP_heardMammogram <- factor(BrData$KP_heardMammogram,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_heardMammogram)

x <- table(BrData$KP_heardMammogram)
x
round(prop.table(x),4)*100

#5.4 Do you know someone close to you who had or has cancer? Yes/No (THIS IS ABOUT CANCER, NOT BREAST CANCER)
BrData$KP_knowCancerPT <- factor(BrData$X5.4.Do.you.know.someone.close.to.you.who.had.or.has.cancer...THIS.IS.ABOUT.CANCER..NOT.BREAST.CANCER.)
summary(BrData$KP_knowCancerPT)
BrData$KP_knowCancerPT <- factor(BrData$KP_knowCancerPT,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_knowCancerPT)

x <- table(BrData$KP_knowCancerPT)
x
round(prop.table(x),4)*100

#5.5 Did you know about breast cancer before?
BrData$KP_knowCancerBefore <- factor(BrData$X5.5.Did.you.know.about.breast.cancer.before.)
summary(BrData$KP_knowCancerBefore)
BrData$KP_knowCancerBefore <- factor(BrData$KP_knowCancerBefore,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$KP_knowCancerBefore)

x <- table(BrData$KP_knowCancerBefore)
x
round(prop.table(x),4)*100

#5.6 Any particular information you wish you knew before and want others to know?
BrData$KP_wishknowCancerBefore <- factor(BrData$X5.6.Any.particular.information.you.wish.you.knew.before.and.want.others.to.know.)

#13.	Family history of breast cancer: yes/no
BrData$Hist_br <- factor(BrData$X6..Family.history.of.breast.cancer)
summary(BrData$Hist_br)
BrData$Hist_br <- factor(BrData$Hist_br,levels=c("No","Yes"),labels = c("No","Yes"))
summary(BrData$Hist_br)

x <- table(BrData$Hist_br)
x
round(prop.table(x),4)*100

#KPTotal
BrData$KPTotal
summary(BrData$KPTotal)

BrData$KPTotalCat[BrData$KPTotal > 1]  = 0
BrData$KPTotalCat[BrData$KPTotal <= 1]  = 1

BrData$KPTotalCat <- factor(BrData$KPTotalCat,levels=c(0,1),labels = c('Good','Low'))
BrData$KPTotalCat

summary(BrData$KPTotalCat)
x <- table(BrData$KPTotalCat)
x
round(prop.table(x),4)*100


c <- table(BrData$AgeCat ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$AgeCat), ref = ">=60"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$Division_cat ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Division_cat), ref = "Barisal"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$residence ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$residence), ref = "Urban"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Marital_status ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Marital_status), ref = "Married"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$Patients_education ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Patients_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
round(exp(cbind(coef(model), confint(model))),2)

c <- table(BrData$Husbands_education ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Husbands_education), ref = "Secondary/higher"),
             family=binomial(link='logit'),data=BrData)
summary(model)
round(exp(cbind(coef(model), confint(model))),2)

c <- table(BrData$Family_income ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Family_income), ref = ">20,000"),
             family=binomial(link='logit'),data=BrData)
summary(model)
round(exp(cbind(coef(model), confint(model))),2)

c <- table(BrData$firstHF,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$firstHF), ref = "Others"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$remedy,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$remedy), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$TD,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$TD), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$PortableElectronicDevices_mobile ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$PortableElectronicDevices_mobile), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$PortableElectronicDevices_smartphone ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$PortableElectronicDevices_smartphone), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$PortableElectronicDevices_pc ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$PortableElectronicDevices_pc), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$PortableElectronicDevices ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$socialmedia_social ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$socialmedia_social), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$socialmedia_TV ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$socialmedia_TV), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$socialmedia_news ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$socialmedia_news), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$MassMediaAccess ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$MassMediaAccess), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_Lump_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_Lump_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$symptom_breastPain_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_breastPain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_nippleDischarge_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_nippleDischarge_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_SkinChanges_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_SkinChanges_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_BonePain_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$symptom_Others_br,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$symptom_Others_br), ref = "Unchecked"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$AnySymptoms,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$AnySymptoms), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$pathos_cancerStage,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$pathos_cancerStage), ref = "Stage IV"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$discomfort_armPain_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_armPain_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_breastPain_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_breastPain_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_Itching_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_Itching_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$discomfort_Lump_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_Lump_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_liquidBlood_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_liquidBlood_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_shape_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_shape_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_skinColor_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_skinColor_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$discomfort_skinUlcer_br ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$discomfort_skinUlcer_br), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))


c <- table(BrData$FamilySupport_firstTalked ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$FamilySupport_firstTalked), ref = "Other"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$FamilySupport_WhoRecommendDoc ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$FamilySupport_WhoRecommendDoc), ref = "Other"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$FamilySupport_uncomfortabletoShareHusband ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$FamilySupport_uncomfortabletoShareHusband), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$FamilySupport_supportHusband ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$FamilySupport_supportHusband), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

c <- table(BrData$FamilySupport_supportSocialCircle ,BrData$KPTotalCat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$FamilySupport_supportSocialCircle), ref = "No"),
             family=binomial(link='logit'),data=BrData)
summary(model)
exp(cbind(coef(model), confint(model)))

#adjusted model
model <- glm(relevel(factor(BrData$KPTotalCat), ref = "Low")~ relevel(factor(BrData$Division_cat), ref = "Barisal")
             + relevel(factor(BrData$Patients_education), ref = "Secondary/higher")
             + relevel(factor(BrData$Husbands_education), ref = "Secondary/higher")
             + relevel(factor(BrData$Family_income), ref = ">20,000")
             + relevel(factor(BrData$remedy), ref = "No")
             + relevel(factor(BrData$TD), ref = "No")
             + relevel(factor(BrData$PortableElectronicDevices), ref = "Unchecked")
             + relevel(factor(BrData$socialmedia_news), ref = "Unchecked")
             + relevel(factor(BrData$symptom_Lump_br), ref = "Unchecked")
             + relevel(factor(BrData$symptom_BonePain_br), ref = "Unchecked")
             + relevel(factor(BrData$AnySymptoms), ref = "No")
             + relevel(factor(BrData$pathos_cancerStage), ref = "Stage IV")
             + relevel(factor(BrData$discomfort_breastPain_br), ref = "No")
             + relevel(factor(BrData$FamilySupport_firstTalked), ref = "Other")
             + relevel(factor(BrData$FamilySupport_WhoRecommendDoc), ref = "Other"),
             family=binomial(link='logit'),data=BrData)
summary(model)
round(exp(cbind(coef(model), confint(model))),2)


require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

#hoslem
hoslem.test(model$y, fitted(model)) #hosmer and lemeshow goodness of fit  test

#auc value
# 
# prob <- predict(model,type="response")
# pred <- prediction(as.numeric(prob),as.numeric(model$y))
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
# auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
# auc

library("verification")

# For model without ties
roc.area(model$y, fitted(model))
ci.auc(model$y, fitted(model))

#roc curve

# plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")
# grid()
# abline(0,1, col="blue", lty=2)

D.ex <- model$y
M.ex <- fitted(model)
mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)


binorm.roc <- data.frame(c = c.ex, 
                         FPF = pnorm((mu0 - c.ex)/s0), 
                         TPF = pnorm((mu1 - c.ex)/s1)
)
library(survivalROC)
library(plotROC)
binorm.plot <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") +  
  ggtitle("ROC Curves (Adjusted Model)")+
  #  scale_x_continuous("False positive fraction (1 - Specificity)", breaks = seq(0, 1, by = .1))+
  #  scale_y_continuous("True positive fraction (Sensitivity)", breaks = seq(0, 1, by = .1)) 
  style_roc(theme = theme_grey, xlab = "False positive fraction (1 - Specificity)", ylab = "True positive fraction (Sensitivity)")
binorm.plot1 <- binorm.plot + 
  theme(plot.title = element_text(size = 12,hjust=0.5),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
binorm.plot1


c <- table(BrData$FamilySupport_uncomfortabletoShareHusband, BrData$Division_cat)
c
round(prop.table(c,2)*100,2)
summary(c)

#graph2
q <- readShapeSpatial('bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


USH <- read.csv("USH.csv")

q_1$prev <- ifelse(q_1$id==0,USH$Percentage[1],
            ifelse(q_1$id==1,USH$Percentage[2],
            ifelse(q_1$id==2,USH$Percentage[3],
            ifelse(q_1$id==3,USH$Percentage[4],
            ifelse(q_1$id==4,USH$Percentage[5],
            ifelse(q_1$id==5,USH$Percentage[6],
            ifelse(q_1$id==6,USH$Percentage[7],
                   USH$Percentage[8])))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')


x <- ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=4)+
  ggtitle("Comfortable to talk with their spouse")+
  scale_fill_distiller(name='Percentage (%)',palette ="Blues", direction=1)+
  theme(plot.title = element_text(size = 10,hjust=0.5),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
x


c <- table(BrData$FamilySupport_supportHusband, BrData$Division_cat)
c
round(prop.table(c,2)*100,2)
summary(c)

#graph3
q <- readShapeSpatial('bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


HS <- read.csv("HS.csv")

q_1$prev <- ifelse(q_1$id==0,HS$Percentage[1],
                   ifelse(q_1$id==1,HS$Percentage[2],
                          ifelse(q_1$id==2,HS$Percentage[3],
                                 ifelse(q_1$id==3,HS$Percentage[4],
                                        ifelse(q_1$id==4,HS$Percentage[5],
                                               ifelse(q_1$id==5,HS$Percentage[6],
                                                      ifelse(q_1$id==6,HS$Percentage[7],
                                                             HS$Percentage[8])))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')


y <- ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=4)+
  ggtitle("Receive support from spouse after diagnosis")+
  scale_fill_distiller(name='Percentage (%)',palette ="Blues", direction=1)+
  theme(plot.title = element_text(size = 10,hjust=0.5),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
y



c <- table(BrData$FamilySupport_supportSocialCircle, BrData$Division_cat)
c
round(prop.table(c,2)*100,2)
summary(c)

#graph3
q <- readShapeSpatial('bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


SC <- read.csv("SC.csv")

q_1$prev <- ifelse(q_1$id==0,SC$Percentage[1],
                   ifelse(q_1$id==1,SC$Percentage[2],
                          ifelse(q_1$id==2,SC$Percentage[3],
                                 ifelse(q_1$id==3,SC$Percentage[4],
                                        ifelse(q_1$id==4,SC$Percentage[5],
                                               ifelse(q_1$id==5,SC$Percentage[6],
                                                      ifelse(q_1$id==6,SC$Percentage[7],
                                                             SC$Percentage[8])))))))
centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')

z <- ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=4)+
  ggtitle("Receive support from social circle")+
  scale_fill_distiller(name='Percentage (%)',palette ="Blues", direction=1)+
  theme(plot.title = element_text(size = 10,hjust=0.5),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
z

tiff("Map.tiff", units="in", width=12, height=6, res=300)
gridExtra::grid.arrange(x,y,z,ncol=3)
dev.off()



 