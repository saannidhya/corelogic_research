# Email draft to Tanner Regan

Subject: Re: UEA meeting follow up

Hi Tanner,

Thanks — I agree that opening up the assessment process would make this much more informative. I looked into the data availability, and Cook County looks like a particularly promising starting point for what you have in mind.

The Assessor’s Office publishes its [residential valuation code and documentation](https://github.com/ccao-data/model-res-avm), including the property characteristics used, and separate downloadable files for the sales used in model development and the properties to be assessed. It previously used township-level linear regressions and now uses a countywide boosted-tree model. This seems to get us much of the way toward all three ingredients you mentioned, although we would need to verify the exact production run and estimation/holdout split for a chosen year.

There is also [assessment-stage data](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Assessed-Values/uzyt-m557): mailed values, values after Assessor appeals, and Board of Review values. Linking these to the model predictions could let us distinguish what comes from the valuation model from what happens during review and appeals. As another possibility, a [King County report for Seattle condos](https://kingcounty.gov/en/dept/assessor/buildings-and-property/property-value-and-information/reports/area-reports/2024/-/media/king-county/depts/assessor/buildings-property/reports/area-reports/2024/commercial/700_02.ashx) publishes the regression specification, coefficients, and lists of sales used and excluded.

What interests me is using your regression-to-the-mean mechanism to ask how much regressivity can be reduced with the information assessors already have, versus how much requires better property information. We could first reproduce an actual assessment model, then vary the information set and modeling choices separately, evaluating on sales excluded from estimation. We would also need to distinguish persistent valuation differences from transaction-price noise when measuring regressivity. My CoreLogic transaction histories could help with that, subject to checking overlap with the county records.

I found closely related recent work on [Cook County’s model reform](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6543863) and on [richer-data assessment models nationally](https://arxiv.org/abs/2605.15020), so we would want to sharpen the contribution beyond showing that a different model improves assessments.

Would you be interested in starting with a small Cook County pilot around this question? I could take the first pass at mapping one completed reassessment year’s inputs, model, and assessment stages, and we could use that to decide which counterfactuals would be most informative.

Best,
Saani

---

Draft prepared 2026-10-18. Not sent. The county datasets would supply contemporary assessments; this email does not promise a contemporary national CoreLogic assessment panel.
