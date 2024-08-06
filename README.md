This project contains scripts with the main objective of analyzing text complexity in MECO's texts. For this purpose, we use universal dependencies (UD) measures. Coded by Dalmo Buzato (Federal University of Minas Gerais) and Victor Kuperman (McMaster University).

- **MECO_SCRIPT_1** -> The first script, measures calculated by each text and language.
- **MECO_SCRIPT_Sentences** -> This script calculates measures for each sentence in each text and language. 
- **MECO_SCRIPT_L2** -> This script calculates measures for texts read by L2 participants in English. (sentence measures) 
- **MECO_SCRIPT_L2_Passages** -> This script calculates measures for texts read by L2 participants in English. (passage measures)
- **descriptive_sent.txt** -> the tab-separated text file reporting descriptive statistics (mean, sd, min, max) for eye-tracking and text complexity measures at the sentence level
  * firstpass - first-pass sentence reading time (the summed duration of all fixations landing in the sentence region of interest before the gaze left the sentence for the first time
  * total - total sentence reading time (the summed duration of all fixations landing in the sentence region of interest)
  * rate - reading rate (words per minute)
  * skip - skip rate (proportion of skipped words out of total)
  * reg - regression rate (the binary index of whether there were regressions into the sentence)
  * d_ratio	- the ratio of heads to nodes in a sentence
  * mean_depth - the longest path in a dependency tree for the sentence
  * num_tokens – number of word tokens in the sentence
  * ttr_d – same as ttr_w but based on dependency relations
  * ttr_p – same as ttr_w but based on part-of-speech types and tokens
  * ttr_w - the number of word types (unique words) in a text divided by the number of word tokens (all words, unique or repeated) in the text
  * n_v_ratio – the noun-verb ratio: token_nouns/(token_nouns + token_verbs)	
- **descriptive_passage.txt** -> the tab-separated text file reporting descriptive statistics (mean, sd, min, max) for eye-tracking and text complexity measures at the passage level
  * total - total passage reading time
  * rate - reading rate
  * skip - skip rate (proportion of skipped words out of total)
  * reg - regression rate (proportion of regressive saccades)
  * d_ratio	- mean of d-ratio values of sentences in the passage
  * mean_depth - the mean dependency length across all sentences in the passage
  * embeddedness - the number of simplex and complex sentences was calculated, and the sums were divided to compute a ratio (1 – n_simplex/n_complex)
  * mlu - average sentence length in word tokens
  * num_tokens - – number of word tokens in the passage
  * ttr_d – same as ttr_w but based on dependency relations
  * ttr_p – same as ttr_w but based on part-of-speech types and tokens
  * ttr_w	- the number of word types (unique words) in a text divided by the number of word tokens (all words, unique or repeated) in the text
  * n_v_ratio - the noun-verb ratio: token_nouns/(token_nouns + token_verbs)
