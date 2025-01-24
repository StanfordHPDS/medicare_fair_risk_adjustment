# Data Transfer Documentation

## Redivis

The goal of data extraction from Redivis is to create data cuts that can be transferred to the secure Nero GCP computing environment. Data cuts should be be as small as possible with the minimum required indicators. 

#### Eligiblity
First, we identify the cohort of eligible Medicare beneficiaries. Eligiblity is defined over a two-year analytic period. We use the MBSF Base A/B/C/D file to identify beneficiaries that meet our eligiblity criteria: A) enrolled in Medicare Part A and B but not part C for every month of years 1 and 2 OR enrolled in Medicare Part A and B but not part C for every month of year 1 and up until death in year 2, B) only eligible for Medicare based on age, and C) not dual eligible for Medicaid.

Transform files 1-5 apply these eligiblity criteria to identify a cohort of beneficiaries.

#### Cost
We extract data on Medicare payments for eligible beneficiaries from the MBSF Cost & Utilization file, as well as payments from carrier, outpatient, and inpatient claims. These represent two different sources of data from which we can calculate the spending outcome variable in risk adjustment.

Transform files 6-8 select relevant cost indicators from the MBSF file and filter to eligible beneficiaries.

Transform file 11 selects total payments from the carrier line file, transform file 24 selects total payments from the outpatient base file, and transform 29 selects total payments from the MedPAR file.

#### Carrier
We extract data on diagnosis codes from eligible claims from the carrier line file. Importantly, eligiblity is determined at the claim level. Claims are eligible if any HCPCS associated with the claim is on the eligible list.

Transforms 9-10 and transform 12 select relevant indicators for eligible beneficiaries. Transform 13 identifies CLM\_IDs that are eligible based on HCPCS. Transform 14 filters the full list of diagnosis codes to those from an eligible CLM\_ID, and transforms 15-17 are related to data export.

#### Outpatient
We extract data on diagnosis codes from eligible claims from the outpatient base claim file. Eligiblity of claims based on associated HCPCS is identified through the outpatient revenue center file.

Transforms 18-21 determine eligible outpatient CLM\_IDs from eligible beneficiares, transforms 22-23 select relevant indicators, and transform 25 identifies claims from the first year that are eligible.

#### MedPAR
We extract data on diagnosis codes MedPAR claims that come from short-stay or long-stay inpatient claims (excludes skilled nursing facility, home health agency, and Medicare Advantage claims). We use NCH\_CLM\_TYPE\_CD and SS\_LS\_SNF\_IND\_CD to identify eligible MedPAR claims.

Transforms 26 and 27 filter to eligible claims, and transform 28 filters to eligible beneficiaries.

## Transfer via Windows Server
Download all files marked as an output file in sql\_documentation.csv to the PHS Windows Server.

Open Google Cloud SDK Shell. Use the following commands in the shell:

Set the project: `gcloud config set project PROJECT_ID`

Copy files from the windows server to the google cloud storage bucket: `gsutil cp -r dir gs://my-bucket`

## Transfer from Bucket to Instance

From a VM instance terminal: `gsutil cp -r gs://medicare_rif/Raw/<year>/ ~/Medicare/medicare_rif/Raw/`

*Adding `-m`, for example `gsutil -m cp -r` permits parallel copying and can increase transfer speed.* 
