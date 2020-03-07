%let ex_pi1=0.05;

proc logistic data=pva plots(only)=(effect(clband x=(pep_star recent_avg_gift_amt
                       frequency_status_97nk)) oddsratio (type=horizontalstat));
   model target_b(event='1') = pep_star recent_avg_gift_amt
                  frequency_status_97nk / clodds=pl;
   score data=pva out=scopva priorevent=&ex_pi1;
run;

