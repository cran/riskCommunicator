#' The \code{framingham} data set
#'
#' The Framingham Heart Study is a long term prospective study of the etiology of 
#' cardiovascular disease among a population of free living subjects in the community of Framingham, Massachusetts. The Framingham 
#' Heart Study was a landmark study in epidemiology in that it was the first prospective study of cardiovascular disease and 
#' identified the concept of risk factors and their joint effects. The study began in 1948 and 5,209 subjects were initially 
#' enrolled in the study. Participants have been examined biennially since the inception of the study and all subjects are 
#' continuously followed through regular surveillance for cardiovascular outcomes. Clinic examination data has included 
#' cardiovascular disease risk factors and markers of disease such as blood pressure, blood chemistry, lung function, smoking 
#' history, health behaviors, ECG tracings, Echocardiography, and medication use. Through regular surveillance of area hospitals,
#' participant contact, and death certificates, the Framingham Heart Study reviews and adjudicates events for the occurrence of 
#' Angina Pectoris, Myocardial Infarction, Heart Failure, and Cerebrovascular disease.
#' The enclosed dataset is a subset of the data collected as part of the Framingham study and includes laboratory, clinic, 
#' questionnaire, and adjudicated event data on 4,434 participants. Participant clinic data was collected during three examination 
#' periods, approximately 6 years apart, from roughly 1956 to 1968. Each participant was followed for a total of 24 years for the 
#' outcome of the following events: Angina Pectoris, Myocardial Infarction, Atherothrombotic Infarction or Cerebral Hemorrhage 
#' (Stroke) or death.
#'
#' @name framingham
#' @format A data frame with 11627 rows and 39 variables:
#' \describe{
#' \item{RANDID}{Unique identification number for each participant. Values range from 2448-999312.}
#' \item{SEX}{Participant sex. 1 = Male (n = 5022), 2 = Female (n = 6605).}
#' \item{TOTCHOL}{Serum Total Cholesterol (mg/dL). Values range from 107-696.}
#' \item{AGE}{Age at exam (years). Values range from 32-81.}
#' \item{SYSBP}{Systolic Blood Pressure (mean of last two of three measurements) (mmHg). Values range from 83.5-295. }
#' \item{DIABP}{Diastolic Blood Pressure (mean of last two of three measurements) (mmHg). Values range from 30-150.}
#' \item{CURSMOKE}{Current cigarette smoking at exam. 0 = Not current smoker (n = 6598), 1 = Current smoker (n = 5029).}
#' \item{CIGPDAY}{Number of cigarettes smoked each day. 0 = Not current smoker. Values range from 0-90 cigarettes per day.}
#' \item{BMI}{Body Mass Index, weight in kilograms/height meters squared. Values range from 14.43-56.8.}
#' \item{DIABETES}{Diabetic according to criteria of first exam treated or first exam with casual glucose of 200 mg/dL or more. 0 = Not a diabetic (n = 11097), 1 = Diabetic (n = 530)}
#' \item{BPMEDS}{Use of Anti-hypertensive medication at exam. 0 = Not currently used (n = 10090), 1 = Current use (n = 944).}
#' \item{HEARTRTE}{Heart rate (Ventricular rate) in beats/min. Values range from 37-220.}
#' \item{GLUCOSE}{Casual serum glucose (mg/dL). Values range from 39-478.}
#' \item{educ}{} 
#' \item{PREVCHD}{Prevalent Coronary Heart Disease defined as pre-existing Angina Pectoris, Myocardial Infarction (hospitalized, silent or unrecognized), or Coronary Insufficiency (unstable angina). 0 = Free of disease (n = 10785), 1 = Prevalent disease (n = 842).}
#' \item{PREVAP}{Prevalent Angina Pectoris at exam. 0 = Free of disease (n = 11000), 1 = Prevalent disease (n = 627).}
#' \item{PREVMI}{Prevalent Myocardial Infarction. 0 = Free of disease (n = 11253), 1 = Prevalent disease (n = 374).}
#' \item{PREVSTRK}{Prevalent Stroke. 0 = Free of disease (n = 11475), 1 = Prevalent disease (n = 152).}
#' \item{PREVHYP}{Prevalent Hypertensive. Subject was defined as hypertensive if treated or if second exam at which mean systolic was >=140 mmHg or mean Diastolic >=90 mmHg. 0 = Free of disease (n = 6283), 1 = Prevalent disease (n = 5344).}
#' \item{TIME}{Number of days since baseline exam. Values range from 0-4854}
#' \item{PERIOD}{Examination Cycle. 1 = Period 1 (n = 4434), 2 = Period 2 (n = 3930), 3 = Period 3 (n = 3263)}
#' \item{HDLC}{High Density Lipoprotein Cholesterol (mg/dL). Available for Period 3 only. Values range from 10-189.}
#' \item{LDLC}{Low Density Lipoprotein Cholesterol (mg/dL). Available for Period 3 only. Values range from 20-565.}
#' \item{DEATH}{Death from any cause. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{ANGINA}{Angina Pectoris. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{HOSPMI}{Hospitalized Myocardial Infarction. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{MI_FCHD}{Hospitalized Myocardial Infarction or Fatal Coronary Heart Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{ANYCHD}{Angina Pectoris, Myocardial infarction (Hospitalized and silent or unrecognized), Coronary Insufficiency (Unstable Angina), or Fatal Coronary Heart Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{STROKE}{Atherothrombotic infarction, Cerebral Embolism, Intracerebral Hemorrhage, or Subarachnoid Hemorrhage or Fatal Cerebrovascular Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{CVD}{Myocardial infarction (Hospitalized and silent or unrecognized), Fatal Coronary Heart Disease, Atherothrombotic infarction, Cerebral Embolism, Intracerebral Hemorrhage, or Subarachnoid Hemorrhage or Fatal Cerebrovascular Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{HYPERTEN}{Hypertensive. Defined as the first exam treated for high blood pressure or second exam in which either Systolic is 6 140 mmHg or Diastolic 6 90mmHg. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{TIMEAP}{Number of days from Baseline exam to first Angina during the followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMEMI}{Number of days from Baseline exam to first HOSPMI event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMEMIFC}{Number of days from Baseline exam to first MI_FCHD event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMECHD}{Number of days from Baseline exam to first ANYCHD event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMESTRK}{Number of days from Baseline exam to first STROKE event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMECVD}{Number of days from Baseline exam to first CVD event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' \item{TIMEDTH}{Number of days from Baseline exam to death if occurring during followup or Number of days from Baseline to censor date. Censor date may be end of followup, or last known contact date if subject is lost to followup.}
#' \item{TIMEHYP}{Number of days from Baseline exam to first HYPERTEN event during followup or Number of days from Baseline to censor date. Censor date may be end of followup, death or last known contact date if subject is lost to followup.}
#' }
#' @details This dataset is the teaching dataset from the Framingham Heart Study (No. N01-HC-25195), provided with permission from #' the National Heart, Lung, and Blood Institute (NHLBI). The Framingham Heart Study is conducted and supported by the NHLBI in 
#' collaboration with Boston University. This package was not prepared in collaboration with investigators of the Framingham Heart 
#' Study and does not necessarily reflect the opinions or views of the Framingham Heart Study, Boston University, or NHLBI.
#' @docType data
#' @usage data(framingham)

#'
NULL
