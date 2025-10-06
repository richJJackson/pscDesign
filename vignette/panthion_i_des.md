### Panthion Design


### Synthetically controlled trial

Panthion I is designed as a single arm synthetically controlled trial to 
investigate the impact of protons + Atezo Bev against Atezo Bev alone.

This is a synthetically controlled trial which does not recruit control patients
but makes use of a parametric statistical model to estimate how patients will 
respond to treatments.  This generates personalised synthetic controls whereby a 
patients observed response can be compared against their model estimated control.
Details can be found here ("https://richjjackson.github.io/mecPortal/").

The model which acts as a control in this instance is a a model generated on a 
pan-European cohort of patients with intermediate HCC.  Full details on the 
model setting, construction, validation and interpretation are found 
[here](https://richjjackson.github.io/mecPortal//models/hcc_atezo_bev.html)

## Trial Design

The overall design uses a Bayesian approach and uses a simulation 
study to estimate the study parameters.  Evaluations of efficacy are based on 
the posterior distribution comparing the experimental regimen against the 
synthetically generated controls.

Based on a study with 4 sites recruiting at an average rate of 0.84 
patients/site/month and sites opening to recruitment at a rate of 1 per month 
then a total of 77 patients should be recruited over a period of 24 months.

Including a further follow up of 12 months then we expect 58 patients to observe 
an event.  Using personalised synthetic controls an a clinically relevant 
difference of log(0.7)=0.257 we expect a standard error of 0.17 to be observed.  
The study is then powered at 90% when using a one-sided significance level of 0.1.


## 'Traditional' design comparisons

As an illustration, we examine traditional single stage and randomised phase II trial design


#### Traditional Single arm study
A single arm study looking to demonstrate a target 12 month PFS rate of 70% and 
considering a rate of 60% to be unacceptable would require 151 patients.  

NB.  It is noted that single arm studies traditionally look for larger 
differences between unnacceptable and target rates.  We use 0.6 and 0.7 here 
repsectively as this equated to a similar hazard ratio (hr=0.7) used in the 
study design

```
ph2single(0.6,0.7,0.1,0.1)
    n   r Type I error Type II error
1 156 101   0.09760318    0.09061008
2 157 102   0.08732618    0.09991705
3 159 103   0.09412761    0.08987827
4 160 104   0.08422045    0.09903723
5 162 105   0.09078748    0.08914555
```

### Randomised pahse II study design

```
gsdesign.survival(c(1),haz.ratio=0.7,r=1,sig.level=0.1,power=0.9,alternative="one.sided")

 Group sequential design for comparing survival data with hazard ratio = 0.7 
   Treatment allocated at 1:1 (C:E) ratio 
   power family of boundary; 0 (Pocock) to 0.5 (O'Brien-Fleming) 

 total number of events = 206.56 
   information fraction = 1 
      efficacy boundary = 1.282 (power = 0.5) 
              sig.level = 0.1 
                  power = 0.9 
            alternative = one.sided 
```


