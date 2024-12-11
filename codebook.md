# Codebook

## Variables

| **Variable Name**               | **Description**                                                                                           |
|----------------------------------|-----------------------------------------------------------------------------------------------------------|
| `empty`                         | Indicates rows to exclude (1 = Exclude, 0 = Include).                                                     |
| `Remove`                        | Flags rows for exclusion due to misinterpretation of intervention (1 = Exclude, 0 = Include).             |
| `bogus1`                        | Flags rows for exclusion due to misinterpretation of scales (1 = Exclude, 0 = Include).                   |
| `Bogus`                         | Another flag for excluding rows based on specific criteria (1 = Exclude, 0 = Include).                    |
| `country`                       | Represents the country of the participants (factor).                                                     |
| `experimental`                  | Indicates experimental group assignment (factor).                                                        |
| `FSMAS`                         | Variable for FSMAS scores.                                                                                |
| `STAI_S_Task_1`                 | Pre-intervention state anxiety score (STAI).                                                             |
| `STAI_S_Task_2`                 | Post-intervention state anxiety score (STAI).                                                            |
| `PFS_Task_1`                    | Pre-intervention flow state score (PFS).                                                                 |
| `PFS_Task_2`                    | Post-intervention flow state score (PFS).                                                                |
| `SIMA__1`                       | Variable for SIMA scores.                                                                                |
| `math_fluency_Task_1_234`       | Pre-intervention math fluency score.                                                                     |
| `math_fluency_Task_2_234`       | Post-intervention math fluency score.                                                                    |
| `IOS`                           | Inclusion of Others in the Self scale (IOS) score.                                                       |
| `Social_Rel_recoded`            | Recoded social relationship variable (factor).                                                           |
| `VAR00001`                      | Identifier variable used in repeated measures analysis.                                                  |
| `Time`                          | Time variable for repeated measures (factor; e.g., Pre and Post conditions).                             |

## Derived Variables for Analysis

| **Variable Name**               | **Description**                                                                                           |
|----------------------------------|-----------------------------------------------------------------------------------------------------------|
| `correlation_data`              | Data frame containing selected variables for correlation analysis.                                       |
| `data_cleaned`                  | Cleaned data set after excluding flagged rows.                                                           |
| `data_long`                     | Long-format data for repeated measures ANOVA.                                                           |

## Interaction and Mediation Analysis Variables

| **Variable Name**               | **Description**                                                                                           |
|----------------------------------|-----------------------------------------------------------------------------------------------------------|
| `block1_model`                  | Linear model for mediator analysis controlling for pre-intervention state anxiety.                       |
| `block2_model`                  | Linear model for outcome analysis controlling for pre-intervention state anxiety and math fluency.        |
| `mediator_model_step`           | Final mediator model after stepwise selection.                                                           |
| `outcome_model_step`            | Final outcome model after stepwise selection.                                                            |

## Statistical Analysis Variables

| **Variable Name**               | **Description**                                                                                           |
|----------------------------------|-----------------------------------------------------------------------------------------------------------|
| `cor_matrix`                    | Pearson correlation matrix of selected variables.                                                        |
| `p_matrix`                      | P-value matrix corresponding to the correlation matrix.                                                  |
| `formatted_matrix`              | Formatted matrix combining correlation coefficients and p-values.                                        |
