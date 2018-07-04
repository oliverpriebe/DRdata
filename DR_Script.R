
  # read in all sheets into a list
  read_excel_allsheets("~/Documents/UPENN/Aravind2018/LASER-ADVICE-DIAG-TREAT.xlsx", tibble = FALSE)
  
  #DIAGNOSIS EXPLAINATION
  #mr_no are mostly glued to UID numbers. This sheet is populated for each diagnosis that the 
  #doctor makes about the patient, every time that they go into the clinic. This results in many repeats of UID-mr_no-diagnosis
  #pairings with different dates. This set up will allow us to see how many times patients are coming back into the clinic. each
  #seperate vist does have a unique peid number, which allows mapping between the different data sets, and for counting
  #number of visits. Every patient that is getting a DR diagnosis is getting advised
  
  #ADVISED EXPLAINATION
  #The advised sheet is populated much in the same way as the diagnosis sheet, although things are slightly complicated by
  #the fact that diagnosis can designate a problem in both eyes (3) but each treatment is only entered for a specific eye.
  #These advisories are for any type of condition that requires laser treatment, so need to filter by UID number exiistance
  # in the DR_patients data set
  
  #TREATMENT EXPLAINATIOM
  #The treatment section data is collected by making an entry each time a eye recives a laser treatment. This treatment
  #need not be for DR/PDR, so this will also need to be filtered by DR_Patient UID numbers. 

  
  #ISSUES
  #many patients who have diagnosed PDR recieve treatments but do not get advice (UIDs: 12680542, 12337905, 9666714)
    #resolved-- this is prob due to refferalls, but using just merged advice and treatment will eliminate issue
  
  # Patient data has non-uniform advice entry methods (ex.10063988 has 4 sittings advised for each eye two times, and a single sitting
  ## adviced once, but had 9 treatments done instead of 17 (which would be implied)). This means that it is impossible
  ##to tell how many treatments a patient is getting advised to recieve. 
  ##hella sittings for advised for 10008395 -- this could be explained by lack of compliance & subsequent readmission
  
  #There is no corresnponding diagnoses peid for some advice, meaning it it impossible to tell if the advice is for one 
  ##eye or both.
  
  ------------------------------------------------------------------------------------------------------------------
  
  #Initial Set-Up
  
  # split list and transform to data.frame (theres probably a better way)
  UID_Advice <- data.frame(mysheets["advice"])
  UID_Diagnosis <- data.frame(mysheets["diagnosis"])
  UID_Treatment <- data.frame(mysheets["treatment"])
  
  #convert chr columns to numeric
   laser_advice_with_eyeid$UID <- as.numeric(as.character(laser_advice_with_eyeid$UID))
  
  #remove erroneus values
  UID_Advice <- subset(LASER_ADVICE, LASER_ADVICE$mr_no != "fn001")
  UID_Treatment <- subset(UID_Treatment, UID_Treatment$mr_no != "fn001")
  UID_Diagnosis <- subset(UID_Diagnosis, UID_Diagnosis$mr_no != "fn001")
  
  #frequency table approach -- 
  #make frequency table of treatments for each patient and change freqency to number of treatments
  numLaserTreatmentsPerPatient <- data.frame(table(factor(UID_Treatment$UID)))
  colnames(numLaserTreatmentsPerPatient) <- c("UID", "NumTreatments")
  
  #make frequency table for number of advisories per DR patient (by UID)
  AdvisoryFrequency <- data.frame(table(factor(UID_Advice$UID)))
  colnames(AdvisoryFrequency) <- c("UID", "NumAdvisories")
  
  #merge numLaserTreatments and Advisory frequency 
  patientCompliance <- merge(numLaserTreatmentsPerPatient, AdvisoryFrequency, by = "UID", all = FALSE, nomatch = NULL)
  
 
  ------------------------------------------------------------------------------------------------------------------
  # merging approach
    
    #diabtetic diagnosis detection code. Removes all diagnosis that are not DR related using string pattern matching (case insensitve)
    DR_diagnosis_patients <-
    filter(UID_Diagnosis, str_detect(UID_Diagnosis$DiagnosisDesc, regex(('dr|pdr|Diabetes|diabetic'), ignore_case = TRUE
    )))
  
    
  #remove duplicates in detected diabetic patients by UID number. This will remove all repeated enteries, leaving just the first time
  #the patient comes in (and is diagnosed with a diabetes-related disease). This should give the total number of unique diabetiuc
  #patients treated in the hopital from the period of 2015-09-30 17:10:30 to 2018-06-20 09:36:08
  uDR_Patients_diagnosis <- distinct(DR_diagnosis_patients, UID, .keep_all = TRUE)
  
  #to get unique diabetic retinopathy patients advice for 2017-18 
  uDR_Advised <- distinct(DR_Advised, UID, .keep_all = TRUE)
  uDR_Advised2017_18 <- filter(uDR_Advised, entereddatetime >= ymd("2017-01-01"))
  
  #finding number of treatments recieved by a given patient by merging the uDR_paitnets with the treatments table
  #non matches are therefore Not included, but there will be multiple matches from the multiple enteries in UID_Treatment
  #THIS DOES NOT TAKE INTO ACCOUNT THAT NOT ALL DIABETIC PATINETS GET ADVISED
  TreatmentsRecivedByUDR_Patients <-
    merge(
      uDR_Patients_diagnosis,
      UID_Treatment,
      by = "UID",
      all = FALSE,
      nonmatch = NULL
    )
  
  #merge unique diagnosis with unique advice to find all diabetic laser treatment advisees, unmatched rows are removed.
  #this should give us the number of diabetic patients advised laser treatment. 
  #patients recive multiple instances of advice, some of which is repeated, this is because advice needs to be given
  #for both eyes (if both are affected)
  DR_advised_Patients <- merge(uDR_Patients_diagnosis, UID_Advice, by = "UID", all = FALSE, nomatch = NULL)
  
  #gets count of number of unique diabetic patients advised laser treatment
  num_uniqueDR_Advised <- nrow(uMerged_DR_advised)
  
  #merge unique diabetic laser advisees with unique treatment to find the number of unique advisees who actually underwent treatment
  uMerged_DR_OneTreatComplied <-
    merge(
      uDR_Advised,
      distinct(TreatmentsRecivedByUDR_Patients, UID, .keep_all = TRUE),
      by = "UID",
      all = FALSE,
      nomatch = NULL
    )
  
  -------------------------------------------------------------------------------------------------------------------------
  #Using laser_advice_with_eyeid
  
  #merges & connects the diagnosed DR patients to their advisories. This is important because not all advisories are for 
  #DR, and since a patient has multiple diagnoses 
  DR_ADV_wEyeID <- merge(uDR_Patients_diagnosis, laser_advice_with_eyeid, by = c("UID"), all = FALSE, suffixes = c("Diag","Ad"), nonmatch = NULL)
  
  #rm useless columns, including eyepartidDiag b/c merged entereis do not match with treatment eyeid
  #and advice eyeid more reliable??

  DR_ADV_wEyeID$eyepartidDiag <- NULL
  DR_ADV_wEyeID$DoctorDiag <- NULL
  DR_ADV_wEyeID$adviceid<- NULL
  DR_ADV_wEyeID$Scheduledate <- NULL
  
  DR_ADV_wEyeID$mr_noAd <- NULL
  DR_ADV_wEyeID$mr_noDiag <- NULL
  DR_ADV_wEyeID$peidAd <- NULL
  DR_ADV_wEyeID$peidDiag <- NULL
  DR_ADV_wEyeID$entereddate <- NULL
  
  #replacing enteries with eyeid == 3 with two enteries, one for eyeid = 1 and other for eyeid = 2
  allthrees <- filter(DR_ADV_wEyeID, eyepartidAd == 3)
  dup <- allthrees
  dup$eyepartidAd <- 2
  allthrees$eyepartidAd <- 1
  rep <- rbind(dup, allthrees)
  rm(dup, allthrees)
  without <- filter(DR_ADV_wEyeID, eyepartidAd != 3)
  DR_ADV_wEyeID <- rbind(without, rep)
  rm(without, rep)
  
  #remove erroneus num sittings values
  DR_ADV_wEyeID <- filter(DR_ADV_wEyeID, DR_ADV_wEyeID$recTreats < 5)
  
  
  #results
  
  #>table(factor(DR_ADV_wEyeID$ScaledComply))
  
  # FALSE  TRUE 
  # 6490  7090 
  
  # > 7090/ (6490+7090)
  # [1] 0.5220913
  
  -------------------------------------------------------------------------------------------------------------------
    ##SUMMARY##
    
    ## 5879 distinct UID in UID_Advice -- nrow(distinct(UID_Advice, UID, .keep_all = TRUE))
    ## 5828 distinct UID in UID_Diagnosis -- nrow(distinct(UID_Diagnosis, UID, .keep_all = TRUE))
    ## 4188 distinct UID in UID_Treatment -- nrow(distinct(UID_Treatment, UID, .keep_all = TRUE))
    
    # 26845 of the UID_Diagnosis diagnoses contain 'dr|pdr|Diabetes|diabetic'
    ##see DR_Diagnosis_patients
    
    # 4859 unique patietns (filtered by UID) that have DR diagnosis
    ##uDR_Patients_diagnosis <- distinct(DR_diagnosis_patients, UID, .keep_all = TRUE)
  
  # ** Also ** #
  
  # 4859 unique patients (filtered by UID) that have a DR diagnoses and were advised treatment, also
  # not a single UID in Diagnosis was not in laser advice
  ##filter(UID_Diagnosis, !(UID %in% UID_Advice$UID))
  
  # so there were no DR patients that were not advised laser treatment (selection by santhana)
  ##see u_DR_Patients_Diagnosis
  
  # 4756 unique patients whtat have DR diagnosis and were advised treatment starting on Jan 1st 2017
  ## see uDR_Advised2018_18
  
  # 12611 patients that are diagnosed with some sort of 'dr|pdr|Diabetes|diabetic' and are advised laser (out of 14109 total advised)
  ## DR_Advised <- merge(uDR_Patients_diagnosis, UID_Advice, by = "UID", all = FALSE, nomatch = NULL)
  
  #--------------------------------------------------------------------------------------------------------------------
