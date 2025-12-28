V2 
- auroc = .946
- excellent calibration

V3 changed strat from yes no to high low. 

It also only includes demographic features related to fairness analyses and age (ordinal for ordered categorical vars and dummy coded for gender and race). It also is on updated sample size (N = 307) after removing participants with less than 2 known locations other than home. It also standardizes features and brings to fence outliers with z scores > |5|
- auroc = .936
- calibration not good 

V4 removes the fence, performance stayed the same 
- auroc = .936
- calibration still bad (raw probabilities don't span as far)

V5 = V4 change back to yes/strat
- auroc = .943
- calibration better (spans full range) but not as good as V2?

V6 = V5 without normalization
- auroc = .942
- calibration same
- will keep V5 as current version


V7 = Add back in fence to V5, add opioid baseline feats, add day of week back in. Updated filtering of subs based on at least 20 gps points per day on avergae (final N = 302)

V8 = final sample (N = 300)
