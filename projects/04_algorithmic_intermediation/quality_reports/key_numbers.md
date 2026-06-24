# Key numbers (06_tables.R, R&R round)

## Sample
- deeds 376256 (buys 193054, disp 185635)
- states 22; state-ZIPs 18657; CBSAs 525; cells 768414; sales 42382121
- median price 173000; mean SD 0.4087; mean IQR 0.3104; hedonic R2 0.597
- iBuyer resid buy -0.0290; sell 0.0082; 2021 buys 67626
- sorting slope -1.279; n 8509

## Exit DiD (zshare:post / odshare:post)
- lvol: z=-0.10044 (0.03502, p 0.0081); od=-0.00472 (0.00677)
- lvol_gross: z=-0.08042 (0.03545, p 0.0318); od=-0.00658 (0.00692)
- disp: z=0.00837 (0.01063, p 0.4385); od=-0.00583 (0.00388)
- iqr: z=-0.00198 (0.00627, p 0.7541); od=-0.00293 (0.00232)
- ltail: z=0.00517 (0.00541, p 0.3482); od=-0.00213 (0.00277)
- ltail_raw: z=0.00386 (0.00672, p 0.5709); od=-0.00452 (0.00392)
- lmedp: z=0.01546 (0.01758, p 0.3871); od=0.00371 (0.00465)
- N household-vol 53343; disp 49130

## Exit sample
- 2425 ZIPs, 27 CBSAs, 940 zero-exposure; zshare mean 0.168; pre 0.000; ramp 0.168; odshare 0.701

## Vintage split (hh volume)
- pre: 2.06232 (1.56583)
- ramp: -0.10705 (0.03555)

## Dose-response (hh volume, terciles vs zero)
- T1: -0.07339 (0.01879, p 0.001)
- T2: -0.06496 (0.01854, p 0.002)
- T3: -0.06364 (0.01953, p 0.003)

## Extensive margin (P(cell>=5) on zshare:post)
- 0.02449 (0.00968, p 0.018)

## Inference
- perm p (999): lvol 0.010; disp 0.442; ltail 0.326
- wild p (999): lvol 0.008; disp 0.510
- placebo lvol: obs t=-2.545, perm p=0.012
- placebo disp: obs t=0.598, perm p=0.555
- placebo ltail: obs t=1.306, perm p=0.216

## Entry (descriptive)
- lvol: -0.0220 (0.0130, p 0.090); pretrend p 0.050
- disp: 0.0015 (0.0027, p 0.566); pretrend p 0.011
- iqr: -0.0039 (0.0022, p 0.080); pretrend p 0.768
- lmedp: 0.0117 (0.0038, p 0.002); pretrend p 0.000
- ltail: 0.0004 (0.0012, p 0.735); pretrend p 0.005
