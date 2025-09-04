------------------------------------------------------------------------------
0.bib_download includes the bib files downloaded from scopus and wos
	- After downloading, they are uploaded in Ryyan for duplicates removal
	- All papers remaining are then exported in 1.abstracting 

update history file [./materials/search_results_history.xlsx]

------------------------------------------------------------------------------
1.abstracting includes the ryyan exported articles after deduplication
	- Select only the useful variables from the ryyan export, reorder them and save it as xlsx (YY_MM_DD_deduplicated)
	- Papers are screened (title + abstract) and initial selection is conducted
		- 'exlcude' when the paper is not of interest / out of topic
		- 'include' when the paper is in topic or looks like they adopt the SEB measurement
		- when finished, add "_processed" at the file name
	- 1.abstracting_exclude.R is used to remove the excluded papers and save a new xlsx in the next folder

update history file 

------------------------------------------------------------------------------
2.full_abstracting includes clear decision data through full-text analysis
	- copy and create a new dataset from the previous "[...]decision_progress.xlsx"
	- name it with current YY_MM_DD_decision_progress
	- manually add the available infos from the current fullabstracting xlsx
	- fill empty info (doi, author_etal, title, journal)
	- download all the pdfs and save them XXXXXXXXXXXXXXXXXXXXXXXXXXXX
	- start screening and take decisions
	- rename it 'YY_MM_DD_fulldecisions' once all info are coded
	- we should now have all pdfs and decisions made

update history file

	- 2.full_exclude.R is used to remove the fulltexts excluded and write a new .xlsx basic_info file

------------------------------------------------------------------------------
3.meta_data includes all the final data that will be used for the meta-analysis and review
	- 'basic_info' includes the dataset with the coded dataframe after fulltext exclusion
	- 'open_data' includes the openly available data provided by the authors. 
		- data are downloaded from the archives and saved as .xlsx named with 'd' + 'paperID'
		- open code are also downloaded and stored as 'opencode' + 'paperID'
	- 'matrices' includes all the correlation matrices obtained from the included papers. These are calculated by
		- Calculating the correlation matrices from the available data (PREFERRED)
			- To do this use 3.1opendata_to_cor.R
			- Ensure that all the variable names are included exactly as reported in the 'matrix_codebook'
			- If they are not present in the 'matrix_codebook' add it
		- Directly extracting the correlation matrices from the papers
			- Rename the columns to ensure that colnames are included exactly as reported in the 'matrix_codebook'
			- If they are not present in the 'matrix_codebook' add it

	- 'meta_data' includes the final data to be used for meta-analytic purposes
		- This is obtained by running '3.2cor_to_metadata.R'. This code works using all the above information which should be available to be included
	- 'review_data' includes the basic_info data of all included papers, comprehending also those excluded from the meta-analysis. This is generated within '3.2cor_to_metadata.R'.