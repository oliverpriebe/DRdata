#need to deal with eyeID numbers somehow
  # read in all sheets into a list
  read_excel_allsheets("~/Documents/UPENN/Aravind2018/LASER-ADVICE-DIAG-TREAT.xlsx", tibble = FALSE)
  
  #DIAGNOSIS EXPLAINATION
  #The Diagnosis sheet seems to opperate with mr_no glued to UID numbers. This sheet is populated for each diagnosis that the 
  #doctor makes about the patient, every time that they go into the clinic. This results in many repeats of UID-mr_n0-diagnosis
  #pairings with different dates. This set up will allow us to see how many times patients are coming back into the clinic.
  
  #ADVISED EXPLAINATION
  #The advised sheet is populated much in the same way as the diagnosis sheet, although things are slightly complicated by
  #the fact that diagnosis can designate a problem in both eyes (3) but each treatment is only entered for a specific eye.
  
  #TREATMENT EXPLAINATIOM
  #It appears that the treatment section data is collected by entering a laser advisory each time a patient comes in,
  #The first advosory will have an immeadiate scheduling designation, while later advisory enteries will be labeled
  #NULL in the scheduled column
  
  #ISSUES
  #many patients who have diagnosed PDR recieve treatments but do not get advice (UIDs: 12680542, 12337905, 9666714)
    #resolved-- this is prob due to refferalls, but using just merged advice and treatment will eliminate issue
  #patients have non-uniform advice entry methods (ex.10063988 has 4 sittings advised for each eye two times, and a single sitting
  # adviced once, but had 9 treatments done instead of 17 (which would be implied)
  
  
  # split list and transform to data.frame (theres probably a better way)
  UID_Advice <- data.frame(mysheets["advice"])
  UID_Diagnosis <- data.frame(mysheets["diagnosis"])
  UID_Treatment <- data.frame(mysheets["treatment"])
  
  #remove erroneus values
  UID_Advice <- subset(LASER_ADVICE, LASER_ADVICE$mr_no != "fn001")
  UID_Treatment <- subset(UID_Treatment, UID_Treatment$mr_no != "fn001")
  UID_Diagnosis <- subset(UID_Diagnosis, UID_Diagnosis$mr_no != "fn001")
  
  #diabtetic diagnosis detection code. Removes all diagnosis that are not DR related using string pattern matching (case insensitve)
  DR_diagnosis_patients <-
    filter(UID_Diagnosis, str_detect(UID_Diagnosis$DiagnosisDesc, regex(('dr|pdr|Diabetes|diabetic'), ignore_case = TRUE
    )))
  
  #remove duplicates in detected diabetic patients by UID number. This will remove all repeated enteries, leaving just the first time
  #the patient comes in (and is diagnosed with a diabetes-related disease). This should give the total number of unique diabetiuc
  #patients treated in the hopital from the period of 2015-09-30 17:10:30 to 2018-06-20 09:36:08
  uDR_Patients_diagnosis <- distinct(DR_diagnosis_patients, UID, .keep_all = TRUE)
  
  #finding number of treatments recieved by a given patient by merging the uDR_paitnets with the treatments table
  #non matches are therefore Not included, but there will be multiple matches from the multiple enteries in UID_Treatment
  #THIS DOES NOT TAKE INTO ACCOUNT THAT NOT ALL DIABETIC PATINETS GETS ADVISED
  TreatmentsRecivedByUDR_Patients <-
    merge(
      uDR_Patients_diagnosis,
      UID_Treatment,
      by = "UID",
      all = FALSE,
      nonmatch = NULL
    )
  
  #u_advice <- distinct(UID_Advice, UID, .keep_all = TRUE)
  #u_treatment <- distinct(UID_Treatment, UID, .keep_all = TRUE)
  
  #merge unique diagnosis with unique advice to find all diabetic laser treatment advisees, unmatched rows are removed.
  #this should give us the number of diabetic patients advised laser treatment. 
  #patients recive multiple instances of advice, some of which is repeated, this is because advice needs to be given
  #for both eyes (if both are affected)
  DR_advised_Patients <- merge(uDR_Patients_diagnosis, UID_Advice, by = "UID", all = FALSE, nomatch = NULL)
  
  #make frequency table of treatments for each patient and change freqency to number of treatments
  numLaserTreatmentsPerPatient <- data.frame(table(factor(UID_Treatment$UID)))
  colnames(numLaserTreatmentsPerPatient) <- c("UID", "NumTreatments")
  
  #make frequency table for number of advisories per patient (by UID)
  AdvisoryFrequency <- data.frame(table(factor(UID_Advice$UID)))
  colnames(AdvisoryFrequency) <- c("UID", "NumAdvisories")
  
  #merge numLaserTreatments and Advisory frequency 
  patientCompliance <- merge(numLaserTreatmentsPerPatient, AdvisoryFrequency, by = "UID", all = FALSE, nomatch = NULL)
  
  #gets count of number of unique diabetic patients advised laser treatment
  num_uniqueDR_Advised <- nrow(uMerged_DR_advised)
  
  #merge unique diabetic laser advisees with unique treatment to find the number of unique advisees who actually underwent treatment
  uMerged_DR_OneTreatComplied <-
    merge(
      uMerged_DR_advised,
      u_treatment,
      by = "UID",
      all = FALSE,
      nomatch = NULL
    )
  
  #finding number of advisees who complied with at least one treatment of PRP
  num_oneTreatComply <- nrow(uMerged_DR_OneTreatComplied)
  
  #non-UID data: 
