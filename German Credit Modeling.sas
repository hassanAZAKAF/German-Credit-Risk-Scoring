ods pdf file="/home/u63486878/GermanCreditData/German Credit Scoring.pdf";
options orientation=portrait;
ods noproctitle;
ods graphics / imagemap=on;
/*import the german credit data from a CSV file*/
proc import out=credit
    datafile="/home/u63486878/GermanCreditData/german_credit.csv"
    dbms=csv
    replace;
    getnames=YES;
run;
/* View the credit data */
proc print data=credit(obs=5);
run;

/* Number of Attributes : 20 (7 numerical, 13 categorical) */

/*calculate descriptive statistics for the numerical variables */
proc summary data=credit;
   var class age duration credit_amount installment_rate residence_since existing_credits num_dependents;
   output out=numericalsummary;
run;

/*print output dataset*/
proc print data=numericalsummary;

/*Univariate for Normality Tests for continous variables*/
proc univariate data=credit normal;
    var age duration credit_amount   ;
run;

proc univariate data=credit;
    histogram age duration credit_amount   / normal;
run;

/*create boxplot to visualize distribution of numerical variables*/
ods output sgplot=boxplot_data;
proc sgplot data=credit;
    vbox credit_amount;
run;

/*view summary of boxplot descriptive statistics*/
proc print data=boxplot_data;

proc univariate data=credit noprint;
   qqplot age duration credit_amount;
run;

/*create scatter plot continuous using continous variables in credit*/

proc sgscatter data=credit; 
	matrix   age duration credit_amount  /group=class diagonal=(histogram kernel);
run;

/*create boxplots by group*/
proc sgplot data=credit;
   vbox credit_amount / group=class;
   keylegend / title="Credit Amount by Class";
run; 
/* Barplot of class */
PROC SGPLOT DATA = credit;
	VBAR class;
RUN;

/* Histogram of credit amount when there is a default*/
proc univariate data=credit;
	where class = 1;
    histogram credit_amount;
run;

/* Target encoding of categorical variables */
%let categorical_vars = checking_status credit_history purpose savings employment personal_status  other_parties property_magnitude other_payment_plans housing job telephone foreign_worker;


/* Iterate over each categorical variable */
%macro target_encode;
  %do i = 1 %to %sysfunc(countw(&categorical_vars.));
    %let current_var = %scan(&categorical_vars., &i.);

    /* Compute target means for the current variable */
    proc sql;
      create table target_means_&current_var. as
      select &current_var., mean(class) as target_mean_&current_var.
      from credit
      group by &current_var.;
    quit;

  %end;
%mend;

%target_encode(credit);

/* Replace the categorical variables with the class mean */

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM credit AS A
	inner join target_means_checking_status AS B
	on A.checking_status=B.checking_status;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_credit_history AS B
	on A.credit_history=B.credit_history;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_savings AS B
	on A.savings=B.savings;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_personal_status AS B
	on A.personal_status=B.personal_status;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_purpose AS B
	on A.purpose=B.purpose;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_employment AS B
	on A.employment=B.employment;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_other_parties AS B
	on A.other_parties=B.other_parties;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_property_magnitude AS B
	on A.property_magnitude=B.property_magnitude;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_other_payment_plans AS B
	on A.other_payment_plans=B.other_payment_plans;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_foreign_worker AS B
	on A.foreign_worker=B.foreign_worker;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_housing AS B
	on A.housing=B.housing;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_telephone AS B
	on A.telephone=B.telephone;
QUIT;

PROC SQL;
	CREATE TABLE encoded_credit as
	SELECT * FROM encoded_credit AS A
	inner join target_means_job AS B
	on A.job=B.job;
QUIT;

data cleaned_data;
	set encoded_credit;
	drop VAR1;
	drop &categorical_vars.;
run;


/* View the encoded credit data */
proc print data=cleaned_data(obs=5);
run;

proc export data=cleaned_data
    outfile="/home/u63486878/GermanCreditData/CleandData.csv"
    dbms=csv
    replace;
run;

proc import out=cleaned_data
    datafile="/home/u63486878/GermanCreditData/CleandData.csv"
    dbms=csv
    replace;
    getnames=YES;
run;

/* Stratified Sampling */

proc sort data= cleaned_data out=startified_data;
by class;
run;

proc surveyselect data=startified_data rate=0.7 outall out=startified_credit seed=1234;
strata class;
run;

data train test; 
set startified_credit; 
if selected =1 then output train; 
else output test; 
drop selected;
run;

/* Logistic Regression */
proc logistic data=train descending  plots =EFFECT plots=ROC(id=prob);
  model class = age duration credit_amount installment_rate residence_since existing_credits num_dependents target_mean_checking_status target_mean_credit_history target_mean_purpose target_mean_savings target_mean_employment target_mean_personal_status  target_mean_other_parties target_mean_property_magnitude target_mean_other_payment_plans target_mean_housing target_mean_job target_mean_telephone target_mean_foreign_worker;
  store out=LogitModel;
run;

proc plm restore=LogitModel;
   score data=test out=Logit_scores ;
run;

proc plm restore=LogitModel;
   score data=test out=Logit_probabilities / ilink;
run;

proc print data=Logit_probabilities(obs=5);
	var predicted;
run;

title 'Distribution of Credit Scores';
proc sgplot data=Logit_scores;
  histogram predicted / group=class transparency=0.5;       
  density predicted / type=kernel group=class; /* overlay density estimates */
run;

data Logit_ROC;
	set Logit_probabilities;
	keep predicted class;
run;
/* ROC Curve */

proc logistic data=Logit_ROC;
   model class(event='1') = predicted / nofit;
   roc 'Logistic Regression' pred=predicted;
   ods select ROCcurve;
run;

/* Kolmogorov-Smirnov Test */
Proc npar1way data=Logit_scores edf;
class class;
var predicted;
run;
/* We have area under ROC : 0.8221, Gini coefficient : 0.6442, KS-statistics : 0.547*/

/* Linear Discriminant Analysis */
PROC DISCRIM data=train method=NORMAL testdata=test testout=LDAScores;
	class class;
	var age duration credit_amount installment_rate residence_since existing_credits num_dependents target_mean_checking_status target_mean_credit_history target_mean_purpose target_mean_savings target_mean_employment target_mean_personal_status  target_mean_other_parties target_mean_property_magnitude target_mean_other_payment_plans target_mean_housing target_mean_job target_mean_telephone target_mean_foreign_worker;
run;

proc print data=LDAScores(obs=5);
run;

data LDA_ROC;
	set LDAScores(rename=('0'n=class_0));
	keep class_0 class;
run;

/* ROC Curve */
proc logistic data=LDA_ROC;
   model class(event='0') = class_0 / nofit;
   roc 'LDA' pred=class_0;
   ods select ROCcurve;
run;

/* Kolmogorov-Smirnov Test */
Proc npar1way data=LDA_ROC edf;
class class;
var class_0;
run;

/* We have area under ROC : 0.8214, Gini coefficient : 0.6428, KS-statistics : 0.539 */


/* Fit Naive Bayes Network classifier */
/* Naive Bayes, Tree Augmented Naive Bayes, Parent Child Bayesian Network, Markov Blanket */
proc hpbnet data=cleaned_data nbin=5 structure= Naive TAN PC MB bestmodel;
target class;
input age duration credit_amount installment_rate residence_since existing_credits num_dependents target_mean_checking_status target_mean_credit_history target_mean_purpose target_mean_savings target_mean_employment target_mean_personal_status  target_mean_other_parties target_mean_property_magnitude target_mean_other_payment_plans target_mean_housing target_mean_job target_mean_telephone target_mean_foreign_worker;
output network=net validinfo=vi varselect=vs
 varlevel=varl parameter=parm fit=fitstats pred=prediction;
partition fraction(validate=0.3 seed=111);
run;

/* Print the fit statistics */
proc print data=prediction;
run;


data NB_ROC;
	set prediction;
	keep P_class0 class;
run;

/* ROC Curve */
proc logistic data=NB_ROC;
   model class(event='0') = P_class0 / nofit;
   roc 'Naive Bayes' pred=P_class0;
   ods select ROCcurve;
run;

/* Kolmogorov-Smirnov Test */
Proc npar1way data=NB_ROC edf;
class class;
var P_class0;
run;

/* We have area under ROC : 0.8846, Gini coefficient : 0.7692, KS-statistics : 0.6128 */


/* Decision Tree Classifier */
PROC HPSPLIT DATA=cleaned_data;
    CLASS class;
    MODEL class = age duration credit_amount installment_rate residence_since existing_credits num_dependents target_mean_checking_status target_mean_credit_history target_mean_purpose target_mean_savings target_mean_employment target_mean_personal_status  target_mean_other_parties target_mean_property_magnitude target_mean_other_payment_plans target_mean_housing target_mean_job target_mean_telephone target_mean_foreign_worker;
    PRUNE costcomplexity;
    PARTITION FRACTION(VALIDATE=0.3 SEED=111);
    OUTPUT OUT = SCORED;
run;

data DT_ROC;
	set SCORED;
	keep P_class0 class;
run;


/* Kolmogorov-Smirnov Test */
Proc npar1way data=DT_ROC edf;
class class;
var P_class0;
run;
/* We have area under ROC : 0.7, Gini coefficient : 0.4, KS-statistics : 0.4147 */

/* Random Forest Classifier */
proc hpforest DATA=train;
	target class / level= binary;
	input age duration credit_amount installment_rate residence_since existing_credits num_dependents / level = interval;
	input target_mean_checking_status target_mean_credit_history target_mean_purpose target_mean_savings target_mean_employment target_mean_personal_status  target_mean_other_parties target_mean_property_magnitude target_mean_other_payment_plans target_mean_housing target_mean_job target_mean_telephone target_mean_foreign_worker / level = nominal ;
	ods output fitstatistics = fit;
	save file = "/home/u63486878/GermanCreditData/RandomForest.bin";
run;

proc hp4score data=test;
 score file= "/home/u63486878/GermanCreditData/RandomForest.bin"
 out=RF_scoring;
run;

data RF_ROC;
	set RF_scoring;
	keep P_class0 class;
run;

/* ROC Curve */
proc logistic data=RF_ROC;
   model class(event='0') = P_class0 / nofit;
   roc 'Random Forest' pred=P_class0;
   ods select ROCcurve;
run;


/* Kolmogorov-Smirnov Test */
Proc npar1way data=RF_ROC edf;
class class;
var P_class0;
run;

/* We have area under ROC : 0.7752, Gini coefficient : 0.504, KS-statistics : 0.4111 */
ods pdf close;