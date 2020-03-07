libname pmlr 'C:\Users\jeong\Documents\SASBA\PredictiveModeling\pmlr93\data';

options nodate nonumber ls=95 ps=80;
ods pdf file='C:\Users\jeong\Documents\SASBA\PredictiveModeling\CHAPTER3_EXERCISE.pdf'
					startpage=no;
ODS graphics on / reset width=6 in scale=on border=off imagefmt=png;

/************************************************************/
/*NO.1*/

data pva(drop=i);
   set pmlr.pva;
   /* name the missing indicator variables */
   array mi{*} mi_DONOR_AGE mi_INCOME_GROUP 
               mi_WEALTH_RATING;
   /* select variables with missing values */
   array x{*} DONOR_AGE INCOME_GROUP WEALTH_RATING;
   do i=1 to dim(mi);
      mi{i}=(x{i}=.);
   end;
run;

proc rank data=pva out=pva groups=3;
   var recent_response_prop recent_avg_gift_amt;
   ranks grp_resp grp_amt;
run;

proc sort data=pva out=pva;
   by grp_resp grp_amt;
run;

proc stdize data=pva method=median
            reponly out=pva1;
   by grp_resp grp_amt;
   var DONOR_AGE INCOME_GROUP WEALTH_RATING;
run;

options nolabel;
proc means data=pva1 median;
   class grp_resp grp_amt;
   var DONOR_AGE INCOME_GROUP WEALTH_RATING;
run;
options label;

/***************************************************************************************/
/*NO.2*/
ods pdf startpage=now;

proc means data=pva1 noprint nway;
   class CLUSTER_CODE;
   var target_b;
   output out=level mean=prop;
run;

ods output clusterhistory=cluster;

proc cluster data=level method=ward outtree=fortree
      plots=(dendrogram(horizontal height=rsq));
   freq _freq_;
   var prop;
   id CLUSTER_CODE;
run;

proc freq data=pva1 noprint;
   tables CLUSTER_CODE*TARGET_B / chisq;
   output out=chi(keep=_pchi_) chisq;
run;

data cutoff;
   if _n_ = 1 then set chi;
   set cluster;
   chisquare=_pchi_*rsquared;
   degfree=numberofclusters-1;
   logpvalue=logsdf('CHISQ',chisquare,degfree);
run;

proc sgplot data=cutoff;
   scatter y=logpvalue x=numberofclusters 
           / markerattrs=(color=blue symbol=circlefilled);
   xaxis label="Number of Clusters";
   yaxis label="Log of P-Value" min=-50 max=-10;
   title "Plot of the Log of the P-Value by Number of Clusters";
run;
title; 

proc sql;
   select NumberOfClusters into :ncl
   from cutoff
   having logpvalue=min(logpvalue);
quit;

ods html close;
proc tree data=fortree nclusters=&ncl out=clus;
   id cluster_code;
run;
ods html;

proc sort data=clus;
   by clusname;
run;

proc print data=clus;
   by clusname;
   id clusname;
   title "Cluster Assignments";
run;

data pva1; 
   set pva1;
   ClusCdGrp1 = CLUSTER_CODE in("13", "20", "53", ".", "28");
   ClusCdGrp2 = CLUSTER_CODE in("16", "38", "03", "40", "18",
                                "24", "01", "14", "46", "35");
   ClusCdGrp3 = CLUSTER_CODE in("06", "10", "32", "41", "44", "47");
   ClusCdGrp4 = CLUSTER_CODE in("09", "43", "49", "51",
                                "21", "30", "45", "52",
                                "08", "37", "50");
run;

/***************************************************************************************/
/*NO.3*/
ods pdf startpage=now;

proc varclus data=pva1 short hi maxeigen=0.70 plots=dendrogram;
   var &ex_inputs mi_DONOR_AGE mi_INCOME_GROUP 
       mi_WEALTH_RATING ClusCdGrp1 ClusCdGrp2 
       ClusCdGrp3 ClusCdGrp4;
   title "Variable Clustering of PVA data set";
run;

/***************************************************************************************/
/*NO.4*/
ods pdf startpage=now;

ods html close;
ods output spearmancorr=spearman
           hoeffdingcorr=hoeffding;

proc corr data=pva1 spearman hoeffding rank;
   var &ex_reduced;
   with target_b;
run;

ods html;

data spearman1(keep=variable scorr spvalue ranksp);
   length variable $ 32;
   set spearman;
   array best(*) best1--best&ex_nvar;
   array r(*) r1--r&ex_nvar;
   array p(*) p1--p&ex_nvar;
   do i=1 to dim(best);
      variable=best(i);
      scorr=r(i);
      spvalue=p(i);
      ranksp=i;
      output;
   end;
run;

data hoeffding1(keep=variable hcorr hpvalue rankho);
   length variable $ 32;
   set hoeffding;
   array best(*) best1--best&ex_nvar;
   array r(*) r1--r&ex_nvar;
   array p(*) p1--p&ex_nvar;
   do i=1 to dim(best);
      variable=best(i);
      hcorr=r(i);
      hpvalue=p(i);
      rankho=i;
      output;
   end;
run;

proc sort data=spearman1;
   by variable;
run;

proc sort data=hoeffding1;
   by variable;
run;

data correlations;
   merge spearman1 hoeffding1;
   by variable;
run;

proc sort data=correlations;
   by ranksp;
run;

proc print data=correlations label split='*';
   var variable ranksp rankho;
   label ranksp = 'Spearman rank*of variables'
         rankho = 'Hoeffding rank*of variables';
   title "Ranks of Variables Based on Spearman and Hoeffding";
run;

/* Find values for reference lines */
proc sql noprint;
   select min(ranksp) into :vref 
   from (select ranksp 
         from correlations 
         having spvalue > .5);
   select min(rankho) into :href 
   from (select rankho
         from correlations
         having hpvalue > .5);
quit;

/* Plot variable names, Hoeffding */
/* ranks, and Spearman ranks      */
proc sgplot data=correlations;
   refline &vref / axis=y;
   refline &href / axis=x;
   scatter y=ranksp x=rankho / datalabel=variable;
   yaxis label="Rank of Spearman";
   xaxis label="Rank of Hoeffding";
   title "Scatter Plot of the Ranks of Spearman vs. Hoeffding";
run;

%let var = LIFETIME_GIFT_RANGE;
proc rank data=pva1 groups=10 out=out;
   var &var;
   ranks bin;
run;

proc means data=out noprint nway;
   class bin;
   var target_b &var;
   output out=bins sum(target_b)=target_b 
          mean(&var)=&var;
run;

data bins;
   set bins;
   elogit=log((target_b+(sqrt(_FREQ_ )/2))/
          ( _FREQ_ -target_b+(sqrt(_FREQ_ )/2)));
run;

proc sgplot data = bins;
   reg y=elogit x=&var /
       curvelabel="Linear Relationship?"
	   curvelabelloc=outside
	   lineattrs=(color=ligr);
   series y=elogit x=&var;
   title "Empirical Logit against &var";
run;

proc sgplot data = bins;
   reg y=elogit x=bin /
       curvelabel="Linear Relationship?"
	   curvelabelloc=outside
	   lineattrs=(color=ligr);
   series y=elogit x=bin;
   title "Empirical Logit against Binned &var";
run;

ods pdf close;
