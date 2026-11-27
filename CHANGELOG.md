## 2026-11-27 - Online release compatibility check

- Replaced Bootstrap-5-only bslib card/grid wrappers with plain Shiny containers
  in Home, Resources and About, preserving the existing Bootstrap 3 appearance
  and content. Removed their browser compatibility errors without changing the
  analysis interfaces, models, source data or exports.

## 2026-11-27 - Manual refresh and release-bundle verification

- Updated the manual for full-author searches, optional Full authors, code-to-DOI
  links, fixed table headers/scrollbar, integer Ns and explained invalid values.
  Documented complete report references and regenerated/visually checked HTML/PDF.
- Built the actual rsconnect archive locally from the twelve deployment files,
  resolved its 142 package dependencies and verified its R 4.5.1 manifest.
- Tested the isolated app with published data, both statistical models and
  CSV/HTML/ZIP/RDS exports, unchanged numerical references, complete bibliography
  and stale-result guards. App-focused tests passed; no upload or deployment.
- Clarified the current OpenMx/R requirement and the release order: publish the
  reviewed manual/citations on main/Pages before deploying the app.

## 2026-11-27 - Review search across all authors

- Include the full author list in text searches, even when hidden. Added optional
  Full authors column with readable separators, retaining the compact default
  Authors/Year/Title view and unchanged source data/CSV exports. Local preview only.

## 2026-11-27 - Direct article links in Review

- Made the always-present article code a compact DOI link with an external-link
  marker, tooltip and accessible name. Links open in a new tab; articles without
  a valid DOI retain a plain code. Other columns remain escaped text, and CSV
  exports retain original codes and data. Local preview only.

## 2026-11-27 - Review table scrolling preview

- Added native vertical scrolling within the Review table (up to 45% of the
  viewport height), keeping column headers and the horizontal scrollbar outside
  the scrolling rows. Short filtered results shrink to their content.
- Retained compact initial columns, readable added columns, sorting, pagination,
  article selection and full CSV downloads. Local preview only.

## 2026-11-27 - Local app/report corrections

- Show initial, stale-selection and failed-fit instructions once in the relevant
  status area. Unready tables and plots stay empty, preventing repeated validation
  messages from overlapping adjacent content; download guards remain in place.
- Display reference Ns and counts as integers and make source-table/report DOI
  links clickable. Explain that invalid values are observed r/N combinations
  that cannot be estimated, excluding ordinary missing correlations from the count.
- Build complete report bibliographies using DOI/title matches, supplemented by
  full coded review metadata for acquisitions absent from the historical bib.
  Missing reference metadata blocks export instead of silently dropping articles.
  Original bibliographic exports are unchanged.
- Share HTML table formatting between both reports; render conditional tables as
  HTML, including the optional pooled matrix. Verified four references for fluid
  intelligence, preserved estimates/SE, fresh/stale states and rendered tables.
  Local preview only; no deployment.

## 2026-11-27 - App guide and release information

- Updated Home, Resources and About & Citation for interface 0.2.0, including
  article search/cards, compact results, model settings, package versions and
  loaded-data counts. Retrieval time is explicitly distinct from data revision
  dates. Local checkouts open the newly rendered manual; standalone apps use Pages.
- Rewrote the manual to match Review, Meta-analysis and MetaSEM, and regenerated
  its HTML and six-page PDF. Corrected the shared APA punctuation and BibTeX
  journal field without duplicating the project citation.
- Updated tutorial comments and made the local data source the explicit default;
  the remote alternative remains commented. Both tutorials reproduce their prior
  results exactly with the same local data. Statistical models are unchanged.
- Verified app checks, standalone initialization/model fitting, rendered document
  layout and public resource links. Publisher DOI pages reject automated access
  with HTTP 403 after resolving correctly. No publication or deployment.

## 2026-11-26 - Controlled local metaSEM 2.0 update

- Updated the tested runtime to metaSEM 2.0 and lavaan 0.7-3, with previous
  packages retained in a local ignored library. Kept the one-stage statistical
  model, sampling-covariance settings, missing values and reference Ns unchanged.
- Replaced the old safety restriction on fixed paths and variables without
  directed paths with an acyclicity check that includes both fixed and free
  paths. Added a minimum-version check and recorded runtime versions in reports
  and exports. No automatic package installation occurs when launching the app.
- Added numeric reference and variable-order tests, alongside actual fitted
  fixed-path, isolated-variable and covariance-only examples. Local preview only.
- Re-ran the twelve OSMASEM model blocks and their formatted tables/narrative
  summaries from both paper documents, plus the tutorial, in isolated old/new
  libraries. Estimates and standard errors match exactly; parameter names,
  variable mappings and fields used by the summaries remain compatible.
- Passed the complete repository test suite after updating the Review test
  fixture for the model-editor variable notes. Verified actual HTML/ZIP/RDS
  downloads and the local browser flow with the updated regular R library.
- Also completed the larger eleven-variable pooling from both documents in
  old/new libraries; the resulting tables match at their reported two decimals.

## 2026-11-26 - Local MetaSEM workflow review

- Listed selected variable codes and readable labels as automatically refreshed
  comments in the model editor, retaining the user's syntax. Removed the optional
  eigenvalue audit and its notices at the user's request; source values are unchanged.
- Kept one-stage OSMASEM settings unchanged (weighted sampling covariances,
  zero between-study heterogeneity and Wald-z intervals). Reference estimates
  and standard errors match the previous workflow exactly.
- Bound data, exploratory TSSEM and one-stage results to their saved selections;
  model edits invalidate only SEM results, while variable/filter edits require
  reloading data. Reports and exports reject stale selections.
- Preserved model syntax using the lavaan parser, including labels and explicit
  variances, with clear rejection of unsupported constructs. Added preflight
  checks for fixed nonzero paths, unused variables and feedback structures that
  can hang the installed metaSEM 1.5 backend; no constraints are silently dropped.
- Added readable K/reference-N matrices, distinct-pair coverage warnings, matrix
  audits and sortable source information. Missing values and source Ns remain
  unchanged. R-squared uses model-implied total variance for applicable endogenous
  variables, retaining and flagging out-of-range values.
- Rewrote the report to distinguish exploratory pooling from one-stage estimation
  and to state dependency/heterogeneity limitations. ZIP exports preserve one CSV
  per matrix with variable names, source information, coverage and model settings.
- Verified mouse-independent controller behavior, numerical equivalence, syntax
  protection, CFA, missing cells, ZIP/RDS round trips and real HTML report download.
  Local preview only; no deployment or merge to main.

## 2026-11-26 - Local meta-analysis preview

- Kept r, confidence intervals, p, articles, matrices and reference N in the main
  results table; moved SE, heterogeneity variance, Q and status to expandable
  model details in both the app and its HTML report. Fitted models are unchanged.
- Made the app's disclosure visibly interactive with a bordered header, rotating
  chevron, hover and keyboard-focus feedback; verified mouse and keyboard toggling.
- Preserved the REML paper/matrix model and coded reference Ns. Added per-domain
  coverage, explicit unusable-value flags and isolated handling of sparse or
  failed domains without changing source data.
- Saved data, filters, labels and models together at calculation. Results and
  report are blocked when the current selection differs from that saved run.
- Added sortable matrix information and explicit article/matrix/reference-N
  counts. Shared small R helpers format the app and report consistently on the
  correct r or Fisher-z scale, including forest plots.
- Corrected report effect counts, association summaries and dependency wording;
  removed codebook downloads and global-variable dependencies from the report.
- Verified unchanged academic-achievement estimates, empty/sparse/invalid/error
  cases, filter invalidation and real Shiny HTML report generation (11 articles,
  12 matrices, 60 effects). Local preview only; no deployment or merge.

## 2026-11-26 - Local Review browser preview

- Added a compact, paginated article table, initially ordered by article code,
  with sortable headings and a human-selectable set of displayed columns.
- Kept minimum readable column widths, with compact code/author/year/title columns
  and horizontal scrolling when the selected columns exceed the available space.
- Added article details with objectives, readable construct/topic/design labels,
  instruments, abstract, DOI, data access and coding notes. Filtering and text
  search include review-only papers; CSV downloads retain all original fields.
- Kept the implementation in the existing Shiny UI/server style, with a small
  presentation helper and no new package dependencies. Verified search, selection,
  column changes, empty states, downloads and existing review/shared helpers.
- Local preview only; no Shiny deployment or merge to main.

## 2026-11-26 - Publication review and local manual backup

- Added local manual-backup synchronization to accepted monthly result updates,
  with simple local per-article scoring prepared during extraction, protection
  for autonomous workbook edits, and comparison against accepted outputs.
- Kept all backup data and article scripts Git-ignored; versioned only the generic
  updater and technical runtime templates. Historical original files are preserved.
- Re-executed the publication report against the reviewed current datasets.
- Replaced the report's inaccurate "independent samples" wording with sample-level
  correlation matrices, preserving previously removed text.

## 2026-11-20 - Human review corrections

- Clarified Walton0014:11 external criterion indicators mapped to broad constructs,not11 validated outcome scales. Retained source-based index with explicit proxy/item limitations.
- Expanded Living SEB0129 topics with nomological network,incremental validity and achievement; added intervention/digital-assistance topics for Kramer0138.
- Applied the user-approved broader Development/change contribution to0061,0126 and0144,without changing their designs or claiming observed longitudinal change.
- Recovered18 full publication abstracts: five missing and13 truncated. Canonical coding and derived outputs are updated; raw bibliographic exports and manual historical inputs remain unchanged.
- Reconciled missing/legacy open_data values using publication declarations,distinguishing request-only,prohibited/unavailable sharing,unreported and inapplicable cases; recovered declared data-resource links. Remote access was not reverified.
- Updated dictionary/defaults and readable evidence. Workbook/filter/targeted tests passed; results HTML rebuilt with six converged models. All correlations,Ns,eligibility,dates and source matrices remain unchanged.

## 2026-11-19 - Compact Excel presentation

- Capped current generated workbook column widths at40 Excel units and disabled text wrapping,without changing cell values,formulas,filters or source data.
- Applied the same presentation rule to workflow,screening,deduplication,codebook and matrix exports. Manual historical inputs and downloaded originals remain unchanged.
- Verified all52 current workbooks against their original cell data/formulas; full test suite and October regression14/14 passed.

## 2026-11-19 - Completed expanded coding and corrected0123 coverage

- Applied the user's0123 clarification: five domains plus all29 non-compound facets of the adapted45-item form. Kept the format caveat and quantitative exclusion; superseded the earlier incomplete-mapping evidence.
- Completed0132 Pellegrino,0141 Sharabi,0144 Feraco,0145 Lubczyk,0148 Njoten/Feyer and0150 Napolitano. All51 current articles now have expanded coding and harmonized focus topics,with1136 article/construct links.
- Registered interventioninterest,overall complexproblemsolving and the skillforeducation topic. Distinguished current skill scores from change beliefs,hypothetical workshops,bibliometric keywords and scoring intermediates.
- Preserved every correlation,N,eligibility decision,download date,matrix,pair-N file,extraction script and user QMD text. Workbook/local-app checks passed; results HTML rebuilt with six converged models.
- Updated the review filter test to use explicit unassessed-context fixtures once the production corpus became fully coded; preserved distinct tests for Not applicable and Not reported.

## 2026-11-19 - Expanded review coding, tenth group

- Completed0119 Jayawickreme/Fleeson,0120 Fernandes,0123 Rakhimbekova,0124 Hopwood and0126 Allemand:45/51 articles,1024 construct links.
- Registered skillprofiles and digitalassistance focus topics; reused existing variable codes and kept theoretical discussions separate from analyzed-score coverage.
- Documented the incomplete0123 facet key: five domains and16 explicitly named profile-facet contents indexed,with13 claimed facets unidentified. Preserved quantitative exclusion.
- Classified0120 as concurrent validation with an ancillary14-day reliability retest; retained its scoring,matrices,Ns and known publication discrepancies. Indexed0126 support preference without adding matrix correlations.
- Workbook/app checks and targeted tests passed; results HTML rebuilt with six converged models. All correlations,Ns,eligibility,dates,matrices,extraction scripts and user QMD prose remain unchanged.

## 2026-11-19 - Expanded review coding, ninth group

- Completed source-based coding for0104 Sun,0107 Fallucchi,0108 Soto,0114 Jalynskij and0118 Cravet/Usai:40/51 articles,938 construct links.
- Registered nine distinct analyzed constructs and the boardgames focus topic; retained grouped,unwrapped Excel interfaces.
- Clarified substantive designs,nonrandomized intervention status,independent age-comparison cohorts and source-reported BESSI anchors. Supplementary analyses are searchable without expanding every outcome into a focus topic.
- Preserved all correlations,Ns,eligibility,download dates,matrices,pair Ns,extraction scripts and user QMD prose. Workbook/app checks and targeted tests passed; results HTML rebuilt with six converged models.

## 2026-11-19 - Integrate Breil self-appraisals and review coding group 8

- Integrated the explicitly approved0049 matched role-play self-appraisals. Registered self as self-rated task performance; existing unsuffixed observer task codes and global BESSI self-concepts remain distinct. Clarified the three base definitions without renaming codes or changing their grouping. Source values retain1-6. Script0049 now exports28 variables and378 correlations,including78 additions; all300 old coefficients and N137 are preserved. Each added self-appraisal has137 observations; new pair Ns130-137,overall128-137. All nine selected published self-appraisal references reproduce within rounding.
- Updated common matrix CSV/XLSX,private individual CSV,pair Ns,summary,current/before-after audit,exact historical revision,source provenance and general coding notes through the existing workflow. Marked the earlier proposal approved/integrated and retained its historical text. No further Breil decision is pending. Six auxiliary cross-exercise ratings remain outside the matched focal analyses.
- Completed expanded coding for0066 De Fruyt/Scheirlinckx (assessment review),0073 Ichsan (VR synthesis),0087 Collie/Ryan (autonomy/PSEC),0095 Sewell (volunteering experiences) and0100 Pellegrino (university SLD). Coverage is35/51 articles and810 construct links. The two descriptive/narrative reviews have no original measured-score index; source SEB taxonomy categories in0073 are not misrepresented as BESSI measurements or pooled effect estimates. Historical eligibility is unchanged.
- 0087 is long for initial autonomy support predicting later competence/skills; retained N373,221 follow-up participants,error-adjusted/FIML model correlations and15 blank cells. 0095 is long;exp for the explicitly reported nonrandomized quasi-experimental program comparison; no random assignment or proven positive effect is inferred. Its later reflection/hour information enters review coverage only,with original baseline matrix,N455 and available-item rule retained. Added experientiallearning; positive valence and social interaction reuse existing broad constructs with operational evidence. 0100 indexes13 analyzed scores and now includes learningfactors as a focus topic,without manufacturing separately analyzed scoring intermediates.
- Validation: full R suite and October14/14 passed. Independently checked unchanged old effects/Ns,source-to-individual mapping,CSV/XLSX agreement within machine serialization precision,exact revision contracts,scope preservation,registered variant menus,meta/SEM pair readers and standalone Shiny selections for the self-appraisals. Workbooks contain no errors or wrapped cells; five-article preview inspected. Results HTML regenerated with all six OSMASEM fits at status0 and finite estimates. Historical manual scripts,other matrices/pair-N files,raw sources,QMD prose and user HTML preserved. No commit,push or deployment.

## 2026-11-19 - Review coding group 7 and separate Breil proposal

- Completed expanded review coding for0049 Breil (self-concepts/role-play expression),0050 Lechner (German BESSI),0057 Napolitano (conceptual framework),0061 Feraco (change goals/beliefs) and0065 Napolitano (longitudinal correlated change). Coverage is30/51 articles and775 distinct article-construct links; this group indexes24,59,not applicable,4 and29 constructs. Preserved all quantitative eligibility decisions,samples,reference Ns,coefficients,matrices,pair-N files and original extraction scripts.
- Added three source-defined personal-change constructs: conditional feasibility,motivation and anticipated life benefit. In0061 these plus desired change are the analyzed constructs; the five skill/trait labels are referents,not measured ability/personality scores. SEB measurement is Not applicable and the within-person framing experiment remains exp. Theoretical0057 uses theory and Not applicable for sampled context,administered measures and analyzed constructs; its conceptual contributions remain in focus topics.
- Added temporalstability for substantive rank-order/true-score persistence,distinguished from mean-level normative change and ancillary reliability retests. Assigned it to0050/0065. German pilot/report and Study2 designs now include cross;long,reflecting the explicit central stability analyses; Study3 stays cross with the previously agreed staggered timing. The two exported matrices,selected occasions,revised scoring and overlap remain unchanged. 0065 is long,remains review-only and indexes behavioral/emotional engagement separately without adding unanalysed outcomes from the wider baseline cohort.
- At the users request,prepared a separate unpromoted0049 recovery proposal in existing coding evidence. Preserve current observer task codes; proposed attached self suffix would distinguish task_socialengagementself,task_cooperationself and task_emotionalresilienceself. The suffix and values are not registered/exported. Each matched self-appraisal has137 responses on1-6; read-only reconstruction would add78 associations (25 to28 variables,300 to378 coefficients),with new pair Ns130-137. Nine selected published observed-score references reproduce within rounding and the entire old block agrees within1e-12. No new individual/matrix/pair-N files or numeric revision ledger entries were written for this proposal.
- Validation: codebook,historical-revision,human-workflow and review-coding suites passed. Verified saved-workbook import,source-based coverage,exact numeric contracts,protected-file bytes and standalone app filters for theory,experimental and multi-valued longitudinal designs. General/codebook/result workbooks contain no wrapped cells or Excel errors; coding preview inspected. Results HTML regenerated; all six OSMASEM fits returned status0 with finite estimates. Raw data,historical manual scripts,QMD prose and user HTML preserved. No commit,push or app deployment.

## 2026-11-19 - Review coding group 6: five articles in one integration

- Completed expanded review coding for0035 Yoon (ACT prediction),0042 Feraco/Meneghetti (adolescent age/gender differences),0043 Soto (matched skill/trait ratings),0045 Sewell (prospective volunteering) and0048 Soto (adolescent outcomes beyond traits). Coverage is25/51 articles with659 unique article-construct links; indexed11,37,19,9 and40 constructs respectively. Reused existing codes with no codebook changes or additional permanent coding files.
- 0035 indexes five representative BESSI facets rather than five unmeasured full domains,plus five Big Five traits and ACT composite as standardized achievement. 0042 indexes five domains and all32 facets directly analyzed in AppendixA; developmental age-pattern contribution retains cross-sectional design and historical review-only/noNewData routing.
- 0043 retains skill/trait differences,academic achievement and incremental validity and adds responseformats for the focal ability-versus-frequency comparison using identical BESSI20 items. Mosaic forced choice is self-report rather than performance assessment. Frequency-framed traits retain their existing broad trait mapping without claiming BFI2 administration; two grade measures index one construct,with no new composite computed.
- 0045 indexes the eight analyzed facets and volunteering once across its operationalizations. Retained long for initial skills predicting hours over ten weeks; no intervention or skill-change contribution is inferred. 0048 indexes five BESSI domains,five Big Five domains,five SECA dimensions and25 outcomes. Its focused topics are nomologicalnet,incrementalvalidity and skilltraitdifferences; academic outcomes remain discoverable through constructs. The approved0048-over0013 quantitative representation is unchanged.
- Imported edits through the general coding workbook and updated metadata/review outputs once for the five articles. Exact coefficient/N contracts,quantitative eligibility,sample IDs,download dates and all matrix/pair-N/extraction/script/codebook bytes are preserved. Existing raw data,manual scripts,QMD prose and user HTML remain untouched.
- Validation: codebook,manual human-workflow and review-coding test suites passed. Independently verified saved-workbook import,source-based construct counts,scope preservation and standalone app filters. Workbooks contain no wrapped cells or Excel errors; five-row coding preview inspected. Results HTML regenerated with all six OSMASEM fits at status0 and finite estimates. No commit,push or app deployment.

## 2026-11-19 - Review coding group 5: short forms and Italian/Spanish BESSI

- Completed the five expanded article fields, substantive designs and focus topics for 0027 Sewell (BESSI short forms), 0030 Feraco (Italian adaptation) and 0034 Postigo (Spanish adaptation). Coverage is 20 of 51 articles and 543 distinct article-construct links; this group indexes 78,46 and68 constructs respectively. No new workbook or per-article coding table was introduced.
- 0027: assessment,short forms and language adaptation. Its43 criterion rows become41 unique constructs after merging repeated GPA and life satisfaction, plus five BESSI domains and32 facets. Informants and questionnaire lengths do not duplicate construct entries. Added personal income and personal burnout with definitions/source provenance; finer SECA subscales recovered for0053 are not separately analyzed here. Existing review-only/noNewData routing is preserved, with no0027 matrix.
- 0030: assessment,language adaptation and nomological network. Indexed BESSI domains/facets,five Big Five domains,self-efficacy,procrastination,reappraisal and suppression; no unmeasured BFI-2 facets or emotion-regulation total invented. Preserved both samples, their overlap notes and all Ns. Recontacting adults to obtain remaining questionnaires does not create a longitudinal skill-change analysis.
- 0034: assessment,language adaptation,nomological network and newly assigned incremental validity, supported by the combined-predictor models in Table6. Indexed15 Big Five facets,four SEC-Q dimensions and the explicitly analyzed criteria. Existing broad SEC-Q mappings are retained without claiming administration of SECA. SNS meanings, official BESSI weights and the user-approved participant decisions remain unchanged.
- All three retain cross: ancillary reliability retests do not add long. Removed generic selfreport topics while retaining actual self/other ability and typical-behavior measurement modes. Validation outcomes remain discoverable in the construct view. The variable codebook now contains259 entries; topic codes remain50 active plus one retired.
- Validation: full R suite and October regression14/14 passed. Verified CSV/XLSX agreement, manual-import round trip, registered/no-duplicate constructs, exact coefficient/N contracts, unchanged matrix/pair-N/extraction/script bytes and standalone app filters for the review-only criteria. Excel workbooks have no wrapped cells or cell errors; coding preview inspected. Results HTML regenerated with all six OSMASEM fits at status0 and finite estimates. Raw sources, historical scripts,QMD prose and user HTML preserved. No commit,push or deployment.

## 2026-11-18 - Review coding group 4 and approved workplace performance recovery

- Completed expanded review coding for 0018 Feraco (SLD and skill profiles), 0023 Beatty (purpose during university) and 0026 Chen (workplace skills). Coverage is 17 of 51 articles and 351 unique article-construct links. Indexed 12, 38 and 15 constructs respectively, based on analyzed scores rather than every administered item or available matrix column.
- Focus topics are learningdisabilities/academicachievement (0018), purposeinlife/wellbeing (0023), and skillfortheworkplace/jobperformance/incrementalvalidity (0026). Independent cohorts and company performance records do not imply repeated SEB measurement. Registered six variable codes with definitions and one topic; the grouped, unwrapped workbooks now contain 257 variables and 50 active topics plus one retired code.
- With explicit user approval, recovered both objective performance indicators in 0026a from the original author-provided per-month, within-city-centered columns. The matrix increases from 15 to 17 variables and from 105 to 136 associations: 31 additions, every old coefficient and reference N=2992 preserved exactly. Available indicator Ns are 2723 and 2196; new pair Ns are 2195-2723. No participants were excluded and no second centering or division was applied.
- Compared all 20 performance reference coefficients: 18 match published rounding; two have opposing signs for the published Emotional Stability row versus raw neuroticism. Preserved raw-data coefficients and documented the discrepancy without claiming a confirmed publication error. Updated the current article script, common matrix CSV/XLSX, private individual data, pair Ns, evidence, summaries, exact transition ledger and final analytical outputs through workflow.R. Historical manual scripts remain unchanged.
- Walton 0014 remains review-only/noValidatedMeasure with no matrix or analytical metadata row; Beatty 0023 remains review-only/missingData. Registering review constructs does not change quantitative eligibility. Bibliographic and original download dates were not invented.
- Validation: full R suite and October regression (14/14) passed. Native XLSX round-trips, unwrapped cells/error values, exact numerical preservation, source hashes, standalone app filters and coding preview checked. Report HTML regenerated; all six OSMASEM fits have status 0 and finite estimates. Raw originals, QMD prose and user HTML preserved. No commit, push or deployment.

## 2026-11-18 - Review coding group 3: 0012, 0014 and 0015

- Completed the five article fields, design and focus topics for 0012 Vega-Dienstmaier (Peruvian resilience validation), 0014 Walton (raters and response formats) and 0015 Lechner/Urban (social-category inequalities). Coverage is 14 of 51 review articles and 286 article-construct links. All three remain review-only with historical numerical/sample fields unchanged.
- 0012 indexes only emotional resilience and focuses on assessment and Spanish-language adaptation. Mentioned alternative instruments, other BESSI domains and informant versions were not treated as analyzed constructs or administered measures.
- 0014 indexes 22 distinct constructs: five Big Five traits, five separately registered ability-framed BFI domains, and mapped external criteria/cognitive ability/explicit trait-skill judgments. BESSI-specific classification is preserved; informants and response formats do not duplicate construct IDs. Forced choice remains self-report. Removed incrementalvalidity because the analyses compare criterion correlations rather than added prediction after controlling the alternative score.
- 0015 indexes all 32 BESSI facets and five domains; its 2022/2023 cohorts are independent, so cross is retained. Age is descriptive, not a focal social-category comparison. Added source-defined topics for migration background, educational tracking and intersectional inequalities; registered assessment response formats for 0014. There are 49 active topics plus one retired historical code, and 251 variable codes with grouped, unwrapped human workbooks.
- Registered 11 new variable codes with operational definitions and provenance, reusing existing constructs for homework checking, nervousness, knowledge-seeking, classroom participation, grades and cognitive ability. The two tolerance criteria share a broad construct ID; no composite or numerical score was computed. Earlier unreviewed papers were not automatically recoded.
- Validation: codebook and review-coding tests passed; CSV/XLSX round-trips, registered variants, source-based construct coverage, review filters and unchanged exact coefficient/N contracts verified. Matrix/pair-N bytes, historical scripts, QMD prose, extraction summaries and user HTML remain unchanged. Native Excel no-wrap styles and error values checked; coding preview inspected. No commit, push or app deployment.

## 2026-11-18 - Review coding group 2: 0071, 0080 and 0113

- Coded five expanded article fields, substantive design and focus topics for 0071 Feraco (SEB/SRL integration), 0080 Sharabi (university achievement and satisfaction) and 0113 Feraco (SLD replication and synthesis). Coverage is 11 of 51 review articles and 226 distinct article-construct links; no new data table or per-article workbook was introduced.
- 0071: learningfactors and incrementalvalidity; outcomes remain discoverable through constructs. 0080: learningfactors, academicachievement and schoolsatisfaction, reflecting the primary aims. 0113: learningdisabilities, academicachievement and learningfactors; cross-sectional reanalysis plus meta-analysis. Its shared 0071 cohort remains review-only/noNewData; no matrix, sample ID, or sample descriptive value was added.
- Read the 0113 final Figure 1 and note 3: the published synthesis has two high-school studies; the university study was removed. Residual university discussion does not change the coded context. Indexed 14, 9 and 14 constructs respectively; SRL item groupings not analyzed as distinct scores are not fabricated as subscale outcomes.
- Completed all remaining positiveschooloutcomes assignments. The historical code is explicitly retired with zero current uses and remains registered for compatibility; 45 active topics remain. Recorded source pages, reasons and PDF hashes in the existing evidence table and topic ledger, and synchronized the general workbook and review outputs.
- Validation: native XLSX round-trips, unchanged matrix/pair-N bytes, unchanged exact coefficient/N contracts, preserved historical scripts/QMD/user HTML, review-only routing and topic/construct/design filters verified. Review-coding tests passed. No commit, push, app deployment or numerical re-extraction.

## 2026-11-18 - Integrate expanded review coding and recover published resources

- Promoted the approved five-article pilot and coded three further articles (0068 Pellegrino, 0070 Feraco, 0128 Springstein): eight of 51 review articles now have the five expanded article fields, substantive multi-valued designs and focus-based topics. records.csv remains the single coding table; review_data has 36 fields, while analytical metadata retains the legacy 31-field layout plus correlations.
- Integrated the 45-topic dictionary with one temporary legacy-only tag for three unreviewed articles. Existing old assignments remain searchable; no automatic outcome substitution. Added a derived construct view to coding.xlsx and optional app filters for constructs, contexts, SEB measurement, contributions and designs, including review-only articles. Workbooks group codebook rows and disable text wrapping.
- User-approved recovery of four CAAS resources in 0068a (six-item means; 9 to 13 variables; 42 new associations) and three SPACS resources in 0070a (four-item means; 9 to 12 variables; 30 new associations). Every old coefficient is preserved exactly, with N=356 and N=702 unchanged. Published references reproduce all 55 CAAS comparison coefficients and all 30 SPACS resource coefficients within rounding. Eight new base constructs registered with definitions and source evidence, including EBQ usefulness for review discovery; supplementary coefficients for 0128 were not independently checked or invented.
- Updated common matrices, private individual CSVs, pair Ns, extraction summaries, exact historical transitions and coding evidence. last_update.xlsx compares the complete turn against its initial checkpoint: 72 added associations, zero old coefficient or N changes. Historical manual scripts, QMD prose, raw originals and the user HTML are unchanged.
- Validation: complete R suite passed; October 14/14 passed; targeted codebook/manual-import/review tests and standalone Shiny UI/server filters passed. Results HTML regenerated: all six OSMASEM optimizers returned status 0 with finite estimates. Native XLSX CSV round-trips, leading-zero IDs and unwrapped-cell styles checked; construct preview visually inspected. No commit, push or app deployment.

## 2026-11-17: Recover SECA facets and refine the article-coding pilot

- Applied the user's focus rule to the separate pilot: outcomes remain in construct lists; Soto is tagged for assessment/nomological network and Ringwald for trait-skill distinction/incremental validity. Each base construct is indexed once per article. BESSI 0138 is ability; synthesis constructs and substantive-design conventions are accepted. Research infrastructure is added to the topic proposal, without promoting historical topic assignments.
- Confirmed five omitted SECA facets in the original d0053e.xlsx as author-provided means on 1-4. Registered and imported them without inventing item keys. All five have the same 249 observed respondents; all 322 college records remain included.
- Expanded 0053e from 82 to 87 variables and from 3321 to 3741 associations: 420 additions, no altered old coefficients or Ns. Overall pair Ns remain 247-322; new-facet pairs use 247-249. Other matrices, raw files, old manual script and QMD prose remain unchanged.
- Integrated scripts/keys, codebook, common matrix CSV/XLSX, private individual CSV with age, pair Ns, extraction summary, exact historical transition, coding evidence/notes and analytical outputs through workflow.R. Normal 0053 runs export only the college expansion; check-only remains read-only. The existing last_update workbook records the before/after.
- Validation: complete R suite passed with no failures, warnings or skips; October regression passed 14/14. Regenerated the HTML report; all six OSMASEM optimizers returned status 0 with finite estimates. Independent pilot checks confirm 35 field entries and 147 mapped construct entries with no within-article duplicates. No commit or push performed.


## 2026-11-17 — Integrate the approved variable codebook

- Integrated the reviewed 227-variable taxonomy with mandatory operational definitions, optional search aliases and the 32 complete BESSI facet labels verified against the official SEB Skills site. BESSI/BESSI_facet/BIGFIVE identifiers remain stable; compact CSV/XLSX agree and all variable rows are grouped by topic and subgroup with no wrapping.
- Registered the task suffix. Renamed conditionalcooperation to conditionalcooperationtask only in the inactive 0107 recovery candidate and its tests; no saved matrix contains the old name, and the earlier decision not to include this candidate is preserved. All active matrix bytes, coefficients and sample Ns remain unchanged.
- Both additive maintenance and manual workbook import now place new variables in their existing group, retain within-group order and regenerate unwrapped Excel rows. Recorded the approved taxonomy migration in the existing codebook ledger; no individual observations are added.
- Shared the variable resolver between standalone Shiny and local extraction. App menus support old/new taxonomy labels and add registered measurement variants present in effect columns. Search aliases retain canonical submitted IDs, and plots use readable method-specific labels. Existing historical task-prefix IDs are preserved.
- Validation: complete R suite passed, followed by targeted codebook/manual-import checks after the final importer change; October regression passed 14/14. Standalone UI/server smoke checks passed with local current data (the existing review-topic expansion reports its many-to-many join warning). Regenerated the results HTML without editing QMD prose. No commit, push or app deployment performed.

## 2026-11-15 — Examine final four historical quantitative articles

- Examined 0128 Springstein, 0134 Pellegrino, 0138 Kramer and 0141 Sharabi. Progress: all 30 active quantitative articles and 38 matrices examined, with source limitations and pending decisions retained explicitly; not every matrix is reconstructible from individual data.
- Reproduced all 105 historical 0128 coefficients from Table 2 after the existing cis-man sign reversal. Its 91 non-sex associations match directly. The binary contrast means non-cis-man versus cis-man, not female versus male; public code is available but its two input datasets are absent. User subsequently approved retaining all 14 sex associations and N=215 with an explicit interpretation note; decision propagated to coding, metadata/review and audit. No individual scores or pair Ns fabricated.
- Recalculated all 0134 BESSI and engagement means from items, including four reversals, reproducing all 45 correlations at T1 with every pair N=1330. Added compact CSV/XLSX, pair Ns and private individual CSV retaining age. The paper states T3 cohort reuse from 0068; 273 unique complete 45-item profiles match, 238 also contributing T1 here, with concordant sex. Documented dependence in both coding rows; no automatic participant exclusion, no linkage by anonymized ID, no assumed reconciliation of discordant ages.
- User approved one numerical correction: 0138 T1 self-efficacy x growth mindset .10 -> .16, verified visually in official Figure S3; other nine coefficients and N=446 unchanged. Preserved the exact historical transition and original-error audit. Public code confirms observed Pearson pairwise associations. No fictitious individual extraction or pair-N output; access to individual data requires a SWISSUbase contract.
- Reproduced all 45 0141 associations and the provided pair Ns=525 from the concordant total-sample author sheets. User checked original emails and confirmed the historical sex direction; correspondence itself was not acquired or read by the agent. Published N=651 descriptives cannot be certified for reduced N=525, and inconsistent split-group tabs are not substituted for the concordant total sheets.
- Integrated source checksums, actual 2026-11-15 public downloads, evidence, coding and metadata/review through the existing workflow. Unknown historical/bibliographic dates remain unknown. The existing review workbook contains the complete 11-field batch comparison, including exactly one coefficient change and no N changes. All other scientific matrix values, originals, historical scripts, user HTML/codebook and QMD prose are preserved. No commit or push performed.
- Validation: complete R test suite passed with no failures, warnings or skips, including 37 new final-batch checks; October regression passed 14/14. Regenerated livingSEBresults.html and visually checked the existing review workbook. All six OSMASEM models have optimizer status 0 and finite estimated parameters.

## 2026-11-14 — Audit Soto 0108, Cravet/Usai 0118 and Allemand 0126

- Reproduced all 388 previous coefficients. For 0108, preserved baseline skills/traits and first-observed wave-2 outcomes, applying the project's binary-sex convention (11 intermediate codes become missing, two already missing, all 470 people retained). Only 22 sex associations change. Main PDF supplied by the user confirms alternating outcome batteries and original effect coding. In official Table S2, 76 relevant coefficients match directly, 30 replicate at concurrent wave 2 and 30 other cross-wave cells are unreported; preserved historical first-observation timing rather than forcing the publication table.
- Applied the user's 0118 decisions: correct four CPM totals from 36 complete binary items; all other 146 totals already equal the sums. Use code 1 for female as supported by both published group counts (46 IG/27 CG), despite the contradictory source header; retain reported 48.7%. Twenty-nine correlations change; all 150 children retained. Reconstructed all five BESSI means to supplied three-decimal precision and reproduced 272 original published group-specific baseline references. Preserved BRIEF direction and source-index rounding; missing proprietary item/trial keys and reported BESSI frequency anchors remain explicit limitations.
- Applied the user's 0126 decision: retain all 1,917 Part-2 respondents and correct N from 1,898. The 1,113 Part-1-only rows never contributed bivariate data, so all 15 coefficients are unchanged. Preserved the three missing ages in the new private CSV. Did not invent a paper-restricted cohort when an omitted preference item makes four borderline cases unresolved.
- Aligned 0126 descriptives to the retained cohort: 50.60% female (970/1917) and mean age 52.72 from 1,914 observed birth years; preserved the published restricted-cohort age 52.67 and female 50.9% in notes.
- Integrated readable scripts, provenance/evidence, canonical coding, compact matrices/pair Ns/private individual CSVs, metadata/review and exact historical revisions. Kept all other matrix values, original bytes, historical scripts and user files unchanged. Recorded actual supplementary downloads and the user-supplied PDF receipt separately from unknown original/bibliographic download dates. Progress: 26/30 active quantitative articles, 34/38 matrices examined; four articles/four matrices remain.
- Validation: complete R suite passed (664 assertions; no failures, warnings or skips). After aligning descriptives, 150 targeted assertions passed, including all 43 batch checks; October regression passed 14/14. Rerendered the report with all six OSMASEM models at status 0 and finite estimates. Visually checked coding notes, the complete task before/after workbook and both analytical figures. The review workbook preserves all 63 field changes from the batch checkpoint, including 51 corrected coefficients and one reference N. QMD prose and protected user/historical files are unchanged.

## 2026-11-14 — Audit Sewell 0095, Pellegrino 0100 and Fallucchi 0107

- Reproduced all 289 previous coefficients across the three matrices. Retained 0095 available-item baseline means by explicit user decision; the complete-item alternative reproduces 21 published baseline references but remains inactive (20 candidate coefficient changes). All 455 records and the reference N remain unchanged.
- Applied the user's 0100 decisions: standardize grades within degree and recover the supplied Cattell fluid-intelligence total. Twelve existing grade correlations change and 13 associations are added. All 91 Table 4 references match within rounding. All 752 participants and 331 observed grades retained; private coded data preserve age, raw grade and degree.
- Applied the user's 0107 linkage/sex corrections: replace the 261-row many-to-many join with verified one-to-one linkage of all 259 originals; four Other gender responses remain included with binary sex missing. At the user's request, inspected the paper, full supplementary questionnaire, final public replication code and official BESSI key. Table 3 cites Soto scoring; Appendix D includes six ethical items, while supplied totals use five at half scale. Reconstructed six-item ethical means and normalized domain weights. Supplement Q35 confirms that source item_24 is the intended teamwork item23 label. All 190 stored correlations change after linkage; scoring changes 19 conditional on the corrected sample. Retained N=259 and declined conditional-cooperation recovery.
- Preserved downloaded originals and existing manual scripts. Newly retrieved 0107 supplement/README/final code and official BESSI form are private, with actual retrieval dates and checksums in the shared provenance. Public Dataset.csv checksum matches the existing source; no duplicate downloaded. The legacy bessi45 label is explicitly flagged as a 30-item cooperation subset pending later measurement-code harmonisation.
- Integrated coding, evidence, compact CSV/XLSX matrices, pair Ns, private coded CSVs, metadata/review and exact historical transitions through the existing workflow. All other matrix bytes and all reference Ns are unchanged. Progress: 23/30 active quantitative articles and 31/38 matrices examined; seven articles/seven matrices remain.
- Validation: complete R suite passed (622 assertions; no failures, warnings or skips), including 42 new batch checks; October regression passed 14/14. Rerendered livingSEBresults.html; all six OSMASEM models have optimizer status 0 and finite estimates. Visually checked the three coding notes and both analytical figures. Original files, historical scripts, user-owned workflow HTML, user codebook workbook and QMD prose are unchanged.

## 2026-11-14 — Verify 0071/0080 and retain 0087 with model-correlation caveats

- Reconstructed all 105 observed-score correlations for 0071 and all 45 for 0080. Independently verified BESSI means, 0080 SRL reversals and scale means; all 55 published 0080 correlations agree within rounding. Preserved 0071 questionnaire-specific missing/careless masks, distinguishing peer acceptance from SWLS and published factor scores from observed means.
- Independently checked all 63 published 0087 coefficients and 15 blank cells. The user explicitly retains quantitative inclusion and historical N=373. Notes explain bifactor ESEM/error-adjusted means, MLR/FIML, T1 N=373 versus T2 N=221, unknown pair Ns and ethics restrictions on sharing. No latent-to-observed equivalence, pair-N reconstruction or zero filling is inferred.
- Registered local original provenance with unknown acquisition dates and integrated notes through the general workbook and workflow. Kept the supplied 0080 grade conversion, including two zeros, with its unverified conversion rule documented. No original, matrix coefficient, N, participant selection or report prose changed. Progress: 20/30 active quantitative articles, 28/38 matrices examined; 10 articles/10 matrices remain. This includes published-table verification rather than raw-data reproduction for 0087.
- Validation: complete R suite passed (580 assertions; no failures, warnings or skips), including 27 new audit assertions; October regression passed 14/14. All numerical contracts and protected original/matrix/user-file bytes are unchanged. Visually checked all three coding notes. No analysis rerender was needed because scientific inputs are unchanged.

## 2026-11-14 — Recover the approved 0070 behavioral outcome

- Mapped SPACS to sustainabilitycompetence and PebSingle to the existing proenvironmentalbehavior construct after the user's confirmation. Retained the single item's original 1–7 behavioral-frequency metric. Added eight correlations to 0070a (nine variables, 36 coefficients), preserving all 28 previous coefficients and N=702 throughout.
- Upgraded the article script to the common compact exporter, saved the coded individual CSV privately and pair Ns in the common directory, and updated coding/evidence, metadata/review and the exact numerical revision ledger. Both new published CNS/PEB and SPACS/PEB associations agree within rounding, bringing the published reference check to 15. The prior deferral is retained as superseded history.
- Validation: all analytical and integration tests passed; codebook tests passed when rerun after the user released an exclusive Excel lock. CSV/XLSX codebook values remain identical. Regenerated the report successfully, with all six OSMASEM models converging and finite estimates; October regression passed 14/14. All other papers, original data and manual scripts are unchanged. Visually checked the coding note.

## 2026-11-14 — Align observer-report demographics and retain current 0070 measures

- Applied the user's 0053g decision: current descriptive age and female percentage refer to rated targets (47.52 years; 46.72%), while informant descriptives (49.26 years; 51.64%, historically 51.6%) remain in notes. Matrix sex already referred to targets. All correlations, sample Ns and participant selection are unchanged.
- Kept 0070 PebSingle excluded following the user's preference for the existing multi-item measures. Documented the source distinction: SPACS measures perceived action competence for sustainability; PebSingle measures reported behavioral frequency. Equivalence between these constructs is not assumed. The candidate remains deferred, rather than an unresolved approval request.
- Validation: targeted batch-five and numerical-contract tests passed. Verified that only 0053g age, female percentage and notes changed, while every matrix byte and numerical contract remained unchanged. Visually checked the updated coding note.

## 2026-11-14 — Audit 0053/0068/0070 and correct the approved 0068 extraction

- Added readable article scripts for seven matrices and constant-only keys for 0053/0068. Reproduced all 6,925 previous coefficients. Independently reconstructed all 160 BESSI facet scores and 25 official weighted domains across the five 0053 samples, plus all 20 college BFI-2 scores. Reused college Case labels are not treated as unique IDs; no participants are deleted.
- Applied the user's 0068 decisions: retain all 356 people, update N from 350 to 356, recalculate five-subject grade means (correcting 11 to 10 and 9.2 to 9), and standardize achievement separately within school year. Eight achievement correlations change; every other coefficient is preserved. Six sex codes remain missing only for the binary association. Updated female percentage to 207/356=58.15%; retained raw means, school year and age privately. The separate published-analysis replication matches all 55 Table 2 coefficients.
- Preserved the five 0053 matrices and all 28 existing 0070 correlations. The latter also matches 13 published references, with already reversed CNS items retained correctly. Two proposals remain pending: recovering 0070 PebSingle as the existing proenvironmentalbehavior construct (eight additional correlations), and aligning 0053g age/gender descriptives to the rated targets. Other-report name harmonization remains a separate operation.
- Registered pre-existing originals and author code in the shared sources/evidence tables without inventing retrieval dates. Updated canonical coding through the general workbook, the compact 0068 outputs, metadata/review and the exact revision ledger. Audit progress is 17/30 active quantitative papers and 25/38 matrices, with 13 papers/13 matrices remaining. Original files, historical manual scripts and other paper values are preserved.
- Validation: full R suite passed, including the new read-only seven-matrix audit test; October regression passed 14/14. All six rerendered OSMASEM models return status 0 and finite estimates. Visually checked the updated coding note and academic-achievement plot; QMD source prose and the user's standalone workflow HTML remain unchanged.

## 2026-11-14 — Restore the two manual 0050 sample components

- Reversed the unified-cohort experiment at the user's request. Restored 0050a (Study 2/T4, N=940) and 0050b (Study 3, N=767 with 768 released rows), keeping the approved revised T2 scoring and weighted-domain missingness corrections. Both numerical contracts exactly match the pre-unification checkpoint. The 238-person overlap and different pair Ns remain explicit limitations.
- Reactivated 0050b, restored the prior coding/demographics, regenerated both private individual datasets and common matrices, and updated metadata/review through workflow.R. Retained the experiment and reversal as exact ledger transitions and aggregate audit entries; marked its coding evidence as superseded. Active totals return to 38 matrices, 30 quantitative papers and 51 review papers. Historical manual scripts and raw sources are unchanged.
- Validation: both pre-experiment numerical contracts reproduced exactly, all other paper values preserved, full R suite passed and October regression passed 14/14. Regenerated the living-results report without changing QMD prose; all six OSMASEM fits converged with status 0 and finite estimates. Coding notes were visually checked. In 0050b, BESSI-domain versus Big Five-domain pair Ns are 579–580 and BESSI-domain versus IQ pair Ns are 469.

## 2026-11-13 — Unify 0050 with the first available BESSI observation (superseded 2026-11-14)

- Applied the user's choice of one observation per person: linked T0/T1/T2 and T4 using the original ID bridge, merging all 238 shared participants. The unified 0050a contains 1,857 unique private records, with analytic N=1,855 (two people have only sex), 61 variables and 1,830 correlations. First BESSI counts are T0=1,134, T2=8 and T4=703; no averaging or later-wave filling of first-wave facets. Retained source response-quality exclusions and the 607 eligible cognitive tests.
- Reconstructed BESSI means with each occasion's actual initial/revised key and the agreed weighted-domain missingness rule. Combined complementary BFI halves once, recovering 400 observed T0 responses lost by the authors' T1-restricted join. Age/sex use first observed values; age remains outside matrices.
- Kept 0050a as the only active matrix and preserved 0050b byte-for-byte as an explicitly inactive merged component. Removed the empty current 0050 placeholder while retaining all historical manual sources. Updated canonical coding through the general workbook, evidence, pair Ns, metadata/review and the exact numerical revision ledger. Totals: 37 active matrices, 30 quantitative papers and 51 review papers.
- Preserved every other paper's scientific values, all originals, historical manual scripts and the user-edited QMD prose. The independent publication replication remains available; its convergence problems are not imported as effects. Pair Ns 500–1,739 and the pairwise matrix's minimum eigenvalue (-0.10348) remain explicit warnings, without automatic matrix repair.
- Validation: independent item/weighted-domain/ID-bridge reconstruction passed, the full R suite passed, October regression passed 14/14, and the retained historical builder reproduces current outputs. Regenerated the living-results HTML without changing its QMD source; all six OSMASEM fits return status 0 with finite estimates. Visually checked the updated coding note, update summary and gender plot.

## 2026-11-13 — Reproduce German publication blocks before matrix reconciliation

- Added a dedicated publication-only R script, preserving original-source checksums and leaving every active matrix, canonical coding row, metadata/review value and report unchanged. Evaluated 704 PIM models with current lavaan plus the exact 12-variable author domain FIML model. Across Tables 5-9, 732/739 coefficients agree with saved author results within 0.001; seven substantive Table 6 discrepancies remain. Two additional tiny numerical differences cross printed rounding boundaries.
- Confirmed that Table 8 facet-IQ uses T0/T1, while Table 9 domain-IQ uses source T2 domain scores. The user requested discussion before creating a separate analytical component; no new sample/matrix ID is assigned.
- Tested seven discrepant fits and two controls in the authors' locked lavaan 0.6-10, downloaded from the official CRAN archive into an isolated temporary library. All seven discrepant fits are nonconvergent; six reproduce published rounded values, while both controls converge. Diagnostic values from nonconvergent fits are not approved analytical effects. Historical original fit convergence is unavailable, so the source explanation remains qualified.
- Validation: all 704 current-version PIM fits converge; the domain covariance result agrees with the saved 12x12 matrix within 0.0000002. Publication-script reference checks and cumulative-audit I/O were verified using those completed fits; the isolated legacy diagnostic mode was run directly. Analytical file hashes are unchanged.
- The targeted historical-audit test file also passes after adding the publication comparisons. No production extraction, metadata/review, coding, matrix or report file is changed in this checkpoint.

## 2026-11-13 — Integrate cohort selection and original German scoring

- Kept 0048 as the quantitative representative of the identical 0013 cohort, with reference N=897 and pair Ns 428-840. 0013 remains in the review; its matrix and all old manual source files are preserved. Registered the exact inactivity decision in the existing exclusions table and kept numerical protection for every active historical matrix. Active totals are now 30 quantitative papers/38 matrices and 51 review papers.
- Recovered 0049 bf_emotionalvolatility from the existing BFI item scoring: 24 added correlations, no changes to the previous 276. Kept other BFI domains and self-rated performances for the separate coding expansion.
- Preserved the user-supplied german.zip and 89 extracted original files privately. Its Rdata is identical to the historical input. Static source-code review explains all eight T2 facet discrepancies: a second scale_builder assignment discards revised _prm scores and inherits initial-key scores. Recomputed intended revised v0.2 available-item means from original T2 responses; no imputation.
- Retained the source primary-facet coverage for 0050 domains, adding available interstitial half-weight facets and normalizing observed weights. Changed 175/702 correlations with every previous pair N and missingness pattern unchanged. Retained all participants, reference Ns 940/767, all 768 Study 3 source rows, both samples and staggered cross-sectional associations. The released new_id bridge confirms 238 shared participants; dependence is documented.
- Updated coding, provenance/evidence, matrices, private coded individual CSVs, pair Ns, metadata/review and search history through the R workflow. The compact exporter now distinguishes explicit reference N from input_rows without filtering records.
- Validation: complete R suite and October regression (14/14) passed. Verified 24 added 0049 correlations and 175/702 changed 0050 correlations; every other active matrix value, all original files, the inactive 0013 matrix, old manual scripts and report prose are preserved. The five updated coding notes were visually checked.
- Verified the retained historical builder and its numerical guard against the current 38-matrix metadata. Rerendered the living-results HTML from data commit f241423: all six OSMASEM fits returned status 0 with finite estimates. Updated PRISMA to 30 quantitative/51 review articles and checked the diagram and analytical figures visually. The QMD source remains unchanged.

## 2026-11-13 — Extend historical audit to 0048, 0049 and 0050

- Reproduced all 3629 stored correlations across four matrices without changing scientific outputs. Three new article entry points update only the existing cumulative audit; check-only mode writes nothing.
- Detected complete duplication between 0013 and 0048: 897 linked records, identical values/missingness on 20 shared score columns and all 210 common correlations identical after direction harmonization. The user requested discussion before selecting a representative matrix; both remain included for now.
- Proposed recovering omitted 0049 bf_emotionalvolatility (24 additional correlations); nine selected published observed-score correlations agree within rounding. Kept additional BFI domains and self-rated performance outside this focused correction.
- Flagged 0050 official-domain weighting/missingness sensitivity (175/290 changed coefficients; minimum pair N for 0050a falls from 631 to 482), 8 unreconciled revised-facet scores in Study 3, the reported 238/203 recontacted adults, the 768-versus-767 file/cohort discrepancy, and the measurement timing. No automatic exclusions, score replacements or matrix changes.
- Progress: 15 of 31 quantitative articles examined, 19 of 39 matrices; 16 articles/20 matrices remain. Examined articles are not assumed to be independent samples or fully adjudicated.
- Validation: complete R suite passed, including 48 historical-audit assertions. New checks verify the overlap, published 0049 reference coefficients, the eight unreconciled 0050 facets, scoring sensitivities and preservation of scientific outputs. Original retrieval dates remain unknown; no new data were downloaded.

## 2026-11-13 — Integrate approved 0035, 0043 and 0045 revisions

- Recovered sex in 0035a, parentEducation in 0043a and capacityforconsistency in 0045a: 40 additional correlations. Replaced the 0045 ordinal volunteering group with observed total hours: eight existing correlations changed. Every participant, reference N and all other matrix files remain unchanged.
- Updated the general workbook, source/evidence records, common matrix CSV/XLSX pairs, pair Ns, private coded individual CSVs and final metadata/review through workflow.R. The original reviewed before/after remains in the single cumulative audit; exact transitions are registered for compatibility with the retained manual builder.
- Validation: full R suite and October regression (14/14) passed. The three new preparation routes reproduce all 321 saved correlations; check-only mode preserves scientific files. Coding notes were visually verified. Historical scripts, originals and report prose are preserved.
- Rerendered the living-results HTML from data commit 28a75de; all six OSMASEM models returned status 0 with finite estimates. Report source prose and unchanged PRISMA contents were preserved.

## 2026-11-13 — Verify historical articles 0035, 0043 and 0045

- Added three read-only article audits and seven rows to the existing aggregate report. Reproduced all 281 coefficients in three stored matrices, without changing scientific outputs, Ns, raw originals or historical code.
- Independently reconstructed all five measured BESSI facets and all five BFI-44 scales for 0035 to released rounding precision. Proposed recovering omitted sex; proposed recovering 0043 parentEducation lost to a capitalization mismatch; reconstructed eight 0045 facets and proposed adding the omitted capacityforconsistency. The three proposals add 11, 20 and 9 correlations, with existing coefficients unchanged.
- Kept the 0045 outcome-metric choice separate: current volunteering is the ordinal program-hours group; a continuous-hours comparison changes eight existing correlations. The paper's latent correlations are distinguished from observed-score Pearson correlations.
- Progress: 10 historical articles/13 matrices have been examined; including the two new validated articles, 12 of 31 quantitative articles and 15 of 39 matrices. Remaining: 19 articles/24 matrices. Open scientific choices remain recorded for human review.
- Validation: 30 targeted audit assertions passed, including exact numerical reproduction, original/scientific-file preservation, proposed-variable identities and the separate hours-metric comparison. No scientific outputs promoted.

## 2026-11-13 — Integrate 0034 corrections and retain 0030 samples

- Corrected the two SNS construct mappings and applied official normalized BESSI domain weights in 0034a. Changed 461 correlations while preserving the existing complete-constituent missingness, all 303 participants and all eight minors. Reference and pair-specific Ns are unchanged.
- Recorded the user's decision to retain both 0030 samples and all participants, with reference Ns 990 and 824 unchanged. The overlap evidence remains in coding notes and the audit; no 0030 correlations changed.
- Updated the general coding workbook, source/evidence tables and analytical outputs through the existing workflow. Saved the 0034 coded individual dataset privately and pair Ns in the common directory. Preserved the reviewed before/after in the existing aggregate audit and registered the exact transition in the existing legacy compatibility ledger. Historical scripts and raw originals remain intact.
- Validation: complete R suite and October regression (14/14) passed. Exactly 461 correlation values changed, only in 0034a; all sample Ns and the other 38 matrix files are unchanged. All 303 source ages, including eight minors, survive in the private coded CSV. The three updated workbook notes were visually checked; the extraction reproduces the approved saved matrix.
- Rerendered the living results HTML from data commit 851d37f. All six OSMASEM fits returned status 0 with finite estimates. Report prose, the unchanged PRISMA diagram and acquisition-history files were preserved.

## 2026-11-13 — Extend historical verification to 0026, 0030 and 0034 (before integration)

- Added three read-only article audits using the existing aggregate comparison table. Reproduced four stored matrices (4304 coefficients); official matrices, Ns, metadata, review data, raw originals and historical scripts are unchanged.
- Reconstructed the five 0026 BESSI-45 release-block means to supplied rounding precision, all 0030 Study 1 BESSI facets/domains, the 0030 Study 2 domain arithmetic and the 0034 selected-item facet sums. Distinguished score-level reproduction from unavailable item/reversal history.
- Flagged 0030 sample dependence: the paper reports 83 recontacted adults; 68 unique released 32-facet profiles match across studies and agree on sex. No identity-based exclusion was inferred from score matching.
- Quantified unapproved 0034 SNS mapping and official-weight corrections separately and together. The separate comparisons change 136 and 335 correlations. Preserved complete-constituent missingness. Recorded eight retained ages of 17, raw/prepared cohort-count discrepancies and the paper's reversed SNS item counts for review.
- Validation: 21 targeted audit assertions passed, including unchanged scientific files/originals, independent scoring checks and current approved 0120 reproduction. New findings remain in the existing cumulative report; no candidate matrices or individual datasets exported.

## 2026-11-13 — Integrate approved historical corrections

- Integrated official normalized BESSI domain weights in 0120a/b and treated the out-of-range Study 2 BESSI171 response as missing, averaging optimism over the other five items. Changed 175 and 339 correlations respectively; reference Ns remain 1103 and 123. Supplied BFI-2 scores remain unchanged.
- Saved coded individual data privately, pair Ns in the common directory and source/evidence provenance through the general coding workbook. Added the approved 0013 cohort note, keeping N=840 and all correlations unchanged. Author clarification on 0013 cohort selection and 0120 Tables 8-9 remains pending.
- Preserved the reviewed before/after in the existing aggregate audit and registered the two corrections in the existing legacy compatibility ledger. Historical manual scripts, raw originals and user-edited report prose remain unchanged. The 0120 article entry point now reproduces the approved extraction; --check-only never writes.
- Validation: complete R suite and October regression (14/14) passed. Exactly 514 correlation values changed, only in 0120a/b; all sample Ns and the other 37 matrices are unchanged. CSV/XLSX outputs agree and the three updated workbook notes were visually checked. The living results HTML was rerendered from data commit 93a6dca; all six OSMASEM fits returned status 0. The operating guide identifies 0120 as migrated, so its historical block is retained as a reference rather than the current scoring entry point.

## 2026-11-13 — First historical data verification (before integration)

- Independently reconstructed all 15 BFI-2 facets and five domains for 0120b using the official key. Nine facet-score differences across six facets and nine participants coincide with fractional item values; supplied scores equal the other-three-item means in these cases, compatible with different imputation/scoring order. Added the complete 480-coefficient Table 9 reference and comparison to the existing audit: 440 coefficients differ beyond rounding with either supplied or reconstructed BFI-2 scores. Official matrices and coding remain unchanged.
- Preserved the complete user-supplied OSF 7efmp archive and its eight extracted files under the Git-ignored 0120 raw-data directory. All three shared SAV files are byte-identical to the historical inputs. Static review found parcel/CFA syntax and descriptive Cohen's d code, but no Table 8 correlation analysis or observed-domain scoring rule; scientific discrepancies remain open. Archive provenance is pinned in the existing article audit, without promoting any scientific values.
- Added read-only article scripts for 0013, 0018 and 0120, with one aggregate comparison table and original-source checksums. Official matrices, coding and analytical outputs remain untouched; no individual observations are versioned.
- Reproduced five stored matrices. Rebuilt 0018 BESSI scores against the official key and checked both 0120 samples' 32 facets against raw items. Domain scores supplied for 0120 use equal facet weights; official half-weighted domains are retained only as sensitivity candidates.
- Flagged 0013 cohort reconciliation (897 released rows versus reference N=840) and one 0120b BESSI171 response of 32 on a 1-5 scale, propagated into the supplied optimism composite. Explicitly unapproved missing-value and weighting sensitivities quantify the differences; Table 8 is an independent publication comparison for Study 2.
- Validation: seven targeted test assertions passed; repeated audit output is idempotent. All 4,243 stored coefficients reproduced within numerical precision. Thirty of 35 source-reconstructed Study 2 domain-correlate coefficients differ from published Table 8 beyond two-decimal rounding; source/analysis version reconciliation remains open before any promotion.

## 2026-11-12 — Resolve Word icon and Typst font warnings

- Selected the installed Times New Roman font explicitly for the paper's Typst format.
- Bundled a PNG rasterization of the original ORCID SVG and added a Word-only choice in the local APAquarto frontmatter filter. HTML and Typst continue to use SVG, while Word no longer needs an external SVG converter. The local extension adaptation and its regeneration command are documented in INSTALLATION.md.
- Validation: Word, Typst and HTML compiled in the isolated layout fixture without the SVG-conversion or unknown-font warnings. The Word archive contains the original PNG and three ORCID drawings; the Typst PDF embeds Times New Roman. The paper body and scientific outputs are unchanged.

## 2026-11-12 — Format the app manual and retain one APAquarto extension

- Replaced APAquarto manual output with a self-contained HTML guide with left navigation and a standard six-page A4 PDF. Preserved the existing PDF URL, manual prose and citation includes. Improved heading/list hierarchy, mobile wrapping and long code lines; removed an empty References section.
- Added HTML manual links to the site source and README. Only the scientific paper now declares APAquarto; retained its 49 extension files unchanged and removed the unused root and manual copies.
- Validation: manual HTML checked at desktop/mobile widths with no horizontal overflow; PDF pages visually checked. The paper's four output formats compiled in an isolated layout-only fixture with analytical chunks omitted; warnings for omitted cross-references, Word SVG conversion and Typst's Times font remain environment/test limitations. Published paper files and scientific data were not regenerated.

## 2026-11-11 — Share analysis helpers between app and local workflows

- Consolidated the ten analysis, review and bibliography helpers in `app/R/utils.R`, including the former correlation-pair module. `R/utils.R` is now a compatibility loader; app startup uses the same Excel download helper.
- Retained existing function behavior and used explicit `dplyr::select()` in review filtering as well as matrix preparation. Removed duplicate implementations and obsolete commented model-building functions.
- Added standalone-app loading and masked-function checks, alongside the existing correlation-pair tests.
- Validation: full R suite and October regression (14/14) passed. A standalone app bundle loaded the public data and passed server checks for data selection, review and five meta-analysis models. The deployment file list includes the shared helpers. The living results report reran with all six OSMASEM status codes zero and all four analytical tables unchanged.

## 2026-11-11 — Organize materials and reconstruct update history

- Moved the search strategy to `paper/materials/search_code.docx` and replaced the manual search history with `data/search_results_history.csv` and its derived Excel interface. The original workbook remains in Git history.
- Reconstructed acquisition-batch counts from the existing import manifests, live registry and documented screening, using the current included datasets. Every distinct publication belongs to its first recorded acquisition batch. Counts describe its current status, not when screening occurred.
- Integrated history generation into `aggiorna_risultati()` and the same guarded save as analytical outputs. Historical publisher DOI resolver URLs are recognized without changing source coding.
- Archived the nine local analysis trial files under Git-ignored `archive_local/analysis_trials/`, verifying their hashes. Removed the unused empty matrix, three unreferenced default TIFF plots, generated LaTeX float files, and the standalone Quarto example and its sample image.
- Retained utility implementations, historical extraction scripts, Quarto extension copies and published document paths for the separate decisions discussed with the user.
- Validation: full R suite and October regression (14/14) passed, including a simulated future batch; exported Excel data and totals match the CSV reconstruction. Automatic workbook preview was unavailable because the bundled rendering runtime crashed during import.

## 2026-11-11 — Reconcile bibliographic counts before merging

- Included the historical May `.xlsm` screening export and counted distinct publications across manual exports and canonical batches. All 370 historical publications have documented screening; the previously reported 20-record gap was a counting error.
- Reconciled two user-approved registry duplicate pairs through guarded promotion on November 10, retaining source IDs and provenance. Historical snapshots and original exports remain unchanged; the live registry now contains 385 publications.
- Corrected report counts: 614 imported records, 229 duplicates, 385 screened publications and 149 full texts (one repeated historical full-text row removed from the count). The 51 review papers, 31 quantitative papers and 39 samples are unchanged.
- Preserved the user's current report prose; removed only the obsolete discrepancy node from its diagram.
- Validation: complete R test suite and October regression (14/14) passed; Quarto HTML regenerated with all six OSMASEM optimizer status codes equal to zero.

## 2026-11-09 — Refresh the living results report

- Rendered `paper/livingSEBresults.qmd` with the current local datasets (51 review papers; 31 quantitative papers, 39 samples), preserving the statistical model specifications and input data.
- Replaced remote-release metadata and flat-folder counts with local data provenance, import manifests and canonical screening CSVs alongside preserved manual exports. Counted October once and included external intake.
- Recorded 614 imported records, 387 registered unique records, 367 documented abstract screenings and 150 full texts. The historical 20-record screening discrepancy is explicitly unresolved, not an invented exclusion.
- Checked finite meta-analytic estimates and all six OSMASEM optimizer codes (all zero). Corrected threshold wording in the narrative and refreshed the figures. Report-input tests passed; browser file preview was unavailable, so figure images and HTML content were inspected directly.

## 2026-11-09 — Second cleanup audit

- Rechecked tracked additions, local automation folders, empty directories and references. Removed the obsolete combined reset script; the history-finalization message now names the explicit full-rebuild reset.
- No reset was executed. Retained all scientific data, bibliography, registries, test expectations, compatibility helpers and local RStudio session folders; no pre-automation manual file changed.
- The empty local `author_suggestions` directory was also removed; `external` remains the sole current intake directory for external discoveries.

## 2026-11-09 — Remove superseded AI workflow files

- Audited additions against pre-automation commit `0244692`; no pre-existing manual file or scientific output changed.
- Removed the completed migration script, obsolete monthly JSON date helper and legacy routing exporter. Download-date editing is covered by the cumulative workbook test.
- Removed the old full-text consolidation report, duplicate pilot summaries and separate dictionary XLSX; the current guide, original comparison CSV, test fixtures and cumulative dictionary sheet remain.
- Removed seven empty workflow directories. Preserved raw inputs, private data/PDFs, bibliographic registries, provenance, manual adjudications and the locally rendered guide HTML.
- Removed files remain recoverable from commit `d6dee47`.

## 2026-11-08 — Human workflow consolidation

- One screening workbook per batch and one cumulative coding workbook, with explicit imports, stable IDs and protection for unsaved edits.
- Added `workflow.R` for preparation, correction, extraction, before/after review and final updates. Scientific scoring remains visible in per-article scripts.
- Consolidated coding, evidence and source provenance; new extraction exports matrices to the common folder, pair Ns by matrix and one aggregate summary.
- Retired redundant monthly coding artifacts, staging matrices and separate screening workbooks; retained originals and historical scientific scripts.
- Compared all final values with checkpoint 0e1daaa and reproduced the 0150 individual and matrix CSVs exactly.
- Validation: full R suite passed, October screening 14/14, 39 quantitative samples and 51 review articles unchanged; workbook round trips and rendered previews checked. Original BibTeX and historical scientific scripts remain unchanged.
- Skill metadata checked with R's YAML parser. The optional Python `quick_validate.py` could not run because PyYAML is absent in the bundled runtime.

# Changelog

## External discovery intake — 2026-11-05

- Added one provenance/naming rule for author/expert suggestions, backward/forward
  citations, manual searches and other external sources. Keep original BibTeX/RIS
  bytes, receipt/download dates and discovery details in a checksum-pinned manifest.
- Isolate external acquisition batches with `YYYY-MM-external-NN` and an explicit
  process-local selector, retaining the ordinary monthly configuration. Recognise
  these batch IDs through full-text/coding/integration entry points.
- Parse external BibTeX with its abstract and publication type intact. Keep
  discovery source in CiteSource provenance, and test later database duplicates
  without double-counting papers or losing either source.
- First real case: author-referred Napolitano et al. (DOI
  `10.1177/27000710261487580`), one new record, no duplicate candidates. Promoted
  the 387-record bibliography only through the guarded script; title/abstract
  retained and persistent paper ID 0150 assigned. The publisher's automatic PDF
  download returned HTTP 403; the user subsequently provided the published PDF,
  original SAV and author R code. Data/code download dates remain unknown;
  paper/bibliographic date is confirmed as 2026-11-05.
- Read the complete full text and resolved sample overlap by linking 596
  H-prefixed IDs to 0053d, with age/gender agreement for all links (590 full item
  matches, six one-item differences). Exclude these from the new contribution,
  retaining historical data unchanged. All 32 facet and five weighted-domain
  scores reproduce the released scores; source Table 1 agrees within rounding.
- Integrated 0150a: N=30,719, 38 matrix variables, actual pair N=29,244-30,719;
  retain Other-gender participants and source categories in individual data, age
  outside matrices, and the unrepaired minimum eigenvalue (-2.914e-7) in notes.
  Outputs now contain 39 quantitative samples and 51 review papers, with external
  origin distinguished from monthly coding. Reused existing age/gender/development
  topics and variable names; no new construct codes or historical recoding.

## Stable checkout checksums — 2026-11-05

- Pin the recorded line endings of hashed coding, extraction and revision inputs
  in `.gitattributes`, so Git checkout does not invalidate scientific manifests
  through platform-dependent text conversion. Raw bibliographic exports are
  outside these rules; working-tree data and numerical values are unchanged.

## Historical 0132 extraction correction — 2026-11-05

- Corrected the existing 0132 block in `data/3.1opendata_to_cor.R`: apply the
  documented careless-response exclusions (598 students), average both social
  status items and all five test-anxiety items without an additional reversal.
- Average parent scale scores and coded education levels per student before
  linkage (321 reports, 315 students), avoiding duplicated self-report rows.
  Document the single-N metaSEM limitation; keep age out of the matrix and do
  not exclude individual records solely for missing age.
- Validated the corrected block in memory with exports disabled; the user then
  regenerated the historical matrix and individual XLSX with that block.
- Reconciled the analytical builder through an exact before/after revision
  ledger, preserving the original numeric fixture and guards on all other
  samples and on subsequent changes to 0132a. Added regression coverage for
  unauthorised matrix/N changes and invalid revision transitions.
- Verified the user's regenerated files against the corrected script, added
  canonical CSV companions and a 276-pair before/after audit with actual Ns and
  source/output checksums. Rebuilt metadata/review outputs with the N caveat
  sourced from the revision ledger; original downloaded and basic_info files stay
  unchanged. All other 36 historical numerical contracts remain unchanged.

## Bibliographic date and reuse checks

- Recorded the user-confirmed 2026-11-02 export date in all October coding sources,
  with per-batch provenance/history and a reusable date-setting command.
- Added valid empty-table contracts for review-only/no-retention batches and
  isolated audit/manifest directories for multiple samples in a new paper.
- Added regression fixtures for future-month coding shapes, date history and
  repeated date application; operational future bibliographic records are untouched.

## Script simplification and October integration

- Reduced the main 0145 script from 316 to 236 lines, separating named long keys
  and common technical checks while retaining explicit scientific operations.
  Correlation and pair-level CSVs are byte-identical to the preceding extraction.
- Fixed a temporary-file basename collision that had copied the matrix CSV into
  the individual-data destination; the actual individual export now has 973 rows
  and 62 columns, with full saved-value checks and a regression for this failure.
- Added guarded batch integration with fresh coding validation, extraction hashes,
  duplicate/conflict guards, common-folder matrix copies and separate readiness.
- Integrated October: 38 quantitative samples and 50 review papers, retaining every
  historical populated correlation and analytic N and the 31-field interface.
- Shared exact full-variable pair readers between paper/app, supporting names
  containing underscores and distinct attached measurement suffixes.

## 0145 quantitative extraction — 2026-11-04

- Added the first operational article script in the agreed eight-section style,
  producing 973 local individual records and a staged 61-variable correlation
  matrix with sex/gender; age remains in individual data only.
- Implemented the user-confirmed persuasion/perspective distinction, explicit item
  means, response-code handling, normalised BESSI domain weights and source checks.
- Added 15 defined variable-codebook entries with provenance; existing topics and
  historical codebook values are preserved. Added readable formatting for new rows.
- Recorded pair N=790-973, 23 missing HEXACO blocks, author-selected cohort limits
  and an unrepaired small negative eigenvalue, with input/output checksums.
- Updated source JSON notes and rebuilt descriptive coding, preserving its prior
  version. No matrix or coding was promoted into historical analytical outputs.

## Codebook maintenance — 2026-11-04

- Added CSV-led, staged additive maintenance for variable and review-topic
  codebooks, including definitions, source provenance, change ledger, conflict
  detection and idempotent reruns. Existing codebook values remain unchanged.
- Added the canonical variable CSV and exact base/suffix resolution for attached
  `other`/`iat` variants, including base names that contain underscores.
- Changed the compatibility maintenance command to verify CSV/XLSX consistency
  instead of overwriting canonical CSV from a manually edited workbook.

## Agreed next-stage extraction rules — 2026-11-04

- Added shared article-extraction conventions, a guarded eight-section R template
  and attached measurement-suffix registry (`other`, `iat`). Scientific scripts
  retain explicit scoring; age stays outside matrices and within individual data.
- Clarified evolving review topics: reuse common categories and add new ones for
  meaningful distinctions, preserving evidence and separating historical recoding.
  Suffix-aware readers and canonical codebook-maintenance commands remain pending.
- Prioritise original individual data over published matrices, preserve immutable
  local downloads and coded individual datasets in the Git-ignored `open_data/`
  directory, and version reproducible R scoring and source/checksum provenance.
- Record the agreed October-first integration sequence, separate review and
  quantitative readiness, and preserve historical eligibility until reassessment.
  The integration/export stage remains to be implemented.
- Acquired eight public Study 2 sources for 0145 with pinned OSF SHA-256 checks;
  added a repeatable acquisition script and non-individual source manifest. Raw
  data stay Git-ignored. Scoring and correlation extraction remain pending.

## Descriptive coding 1.0.0 — 2026-11-04

- Applied the user-confirmed `assessment -> skillassessment` correction for 0066,
  retaining `theorydevelopment` and recording the original coding in the audit.

- Added the versioned 31-field CSV dictionary, derived Excel dictionary and agreed
  coding rules for review-only scope, analytic N, broad ages and selected occasions.
- Added a validated descriptive-coding builder and October pilot: three papers,
  only Study 2 of 0145 as quantitative sample a (analytic N=973), with page evidence.
- Corrected the exact nomoligicalnet spelling alias in the active codebook and
  compatibility exports while preserving original historical coding files.
- Reconciled 0104a as an inactive matrix under the recorded review/dataQuality
  decision; removed its 18 entirely missing effect columns without changing any
  populated correlation, analytic N or quantitative sample.
- Added CSV companions, explicit matrix/topic audit queues and scientific numeric
  regression fixtures. No new correlation matrices or coding data were promoted.

## Full-text workflow 2.2.0 — 2026-11-04

- Added the repository-scoped `living-seb-full-text` skill to orchestrate PDF
  assessment, validated JSON output, monthly workbook generation and batched
  adjudication using the existing versioned workflow.

- Consolidated policy 2.0.0 on the current feature branch with typed JSON Schema
  validation, exact coverage checks, evidence and checksum provenance.
- Added canonical decision/audit CSVs and rebuild-safe operational XLSX outputs.
- Recovered October artifacts from its existing workbook, preserving six decisions
  and three adjudications with explicit migration provenance.
- Hardened IDs, alias reservations, batch selection and legacy discovery.
- Added guarded resolved-routing export without assigning samples/matrices or
  changing historical analytical workbooks.
- Added full-text tests and English local-workflow documentation. Removed stale
  package checksums and the un-applied ignore snippet; .gitignore is authoritative.

## 2026-10.3

- Added a versioned title/abstract screening policy, prompt, JSON schema, R helpers, scripts, tests, and Codex/ChatGPT skill.
- Retained reviews, meta-analyses, and journal editorials by policy.
- Changed uncertainty handling to automatic conservative retention (`KEEP_UNCERTAIN`), avoiding routine human screening.
- Added stable DOI/title-based `screening_record_id` values and exact coverage validation.
- Added canonical CSV and human-readable XLSX outputs for decisions, included records, and excluded records.
- Added the independently validated 2026-10 pilot as a regression fixture.
- Added module-specific README files and a monthly workflow guide.
- Replaced the destructive reset with separate current-batch and full-rebuild scripts.
- Made Rayyan comparison optional after the exact 14/14 pilot validation, while still blocking promotion when an existing comparison contains discrepancies.
- Added repository-level `AGENTS.md` instructions and a repo-scoped skill under `.agents/skills/`.

## 2026-10.2

- Added Excel workbooks for all human-review steps while retaining CSV as the machine-readable audit format.
- Historical manual review now uses `history_candidate_pairs_review.xlsx`.
- Current-update manual review now uses `update_candidate_pairs_review.xlsx`.
- Added dropdown validation (`match` / `no_match`), instructions, filters, frozen panes and review-oriented formatting.
- Finalization reads only the decisions from Excel and applies them to the canonical candidate pairs held in the R deduplication result.
- Review rows may be sorted in Excel; decisions are matched back using the pair identifiers.
- Added backward compatibility with legacy `*_candidate_pairs_reviewed.csv` files.
- Added an XLSX mirror of the final new-record screening file.
- Added a multi-sheet Excel workbook for the Rayyan comparison while retaining all CSV comparison outputs.
- Added `openxlsx` as a workflow dependency.

## 2026-10.1

- Corrected the active update to publication month `2026-10`.
- Corrected the historical cutoff to `2026-09`.
- Integrated automatic Scopus BibTeX preamble removal in both historical and current imports.
- Preserved all original Scopus files and wrote normalised copies under `data/0.bib_normalized/`.
- Added per-file import counts, MD5 hashes, and import manifests.
- Improved database detection by inspecting the full path.
- Improved month detection for filenames such as `2026_04_30_scopus.bib` and `25_01_31_scopus.bib`.
- Added deterministic bootstrap `record_id` values to avoid row-number warnings.
- Added a guarded clean-reset script.
- Added a guarded registry-promotion script.
