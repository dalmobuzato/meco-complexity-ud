# Load the udpipe library if not already loaded
# install.packages("udpipe")
library(udpipe)
library(tidyverse)
library(igraph)

# Load the RDA file
#load("C:/Users/buzat/Desktop/meco-complexity-ud/texts.meco.l2 (1).rda")
load("texts.meco.l2.rda")

# Keep only the "text" column in the dataframe
d <- d[, "text", drop = FALSE]

# Load the model once, as it doesn't change
model <- udpipe_download_model(language = "english-gum") 
model <- udpipe_load_model(file = model$file_model)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Name the variable based on the row number
  variable_name_udpipe <- paste0("line_", i, "_udpipe")
  
  # Apply udpipe to the current row
  assign(variable_name_udpipe, udpipe_annotate(model, x = current_text))
  assign(variable_name_udpipe, as.data.frame(get(variable_name_udpipe)))
}

# ------------------------ Lexico-syntactic variables ----------------------------
# ------------------------ Noun-to-Verb Ratio ----------------------------
# Create an empty data frame to store the results
noun_to_verb_passage <- tibble(
  doc_id = character(),
  num_noun = integer(),
  num_verbs = integer(),
  n_v_ratio = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter rows based on the condition
  count_lang <- current_df %>%
    filter(upos %in% c("VERB", "NOUN")) %>%
    group_by(doc_id, upos) %>%
    summarise(occurrences = n())
  
  # Pivot the data frame to wide format for easy ratio calculation
  count_lang_wide <- count_lang %>%
    spread(upos, occurrences, fill = 0)
  
  # Calculate the ratio
  count_lang_wide <- count_lang_wide %>%
    mutate(ratio = NOUN / VERB)  # Use column names directly
  
  # Summarize by doc_id
  count_lang_summarized <- count_lang_wide %>%
    group_by(doc_id) %>%
    summarise(
      num_noun = sum(NOUN),
      num_verbs = sum(VERB),
      n_v_ratio = sum(NOUN) / sum(VERB),
      language = "english-l2",  
      column_name = paste0("line_", i)
    ) 
  
  # Convert doc_id to character before combining data frames
  count_lang_summarized$doc_id <- as.character(count_lang_summarized$doc_id)
  
  # Add the information to noun_to_verb_passage
  noun_to_verb_passage <- bind_rows(noun_to_verb_passage, count_lang_summarized)
}

# Remove rows with empty or null sentence_id
noun_to_verb_passage <- noun_to_verb_passage %>%
  filter(!is.na(doc_id) & doc_id != "")
# ------------------------ Type-Token Ratio of Words (TTR-w) ----------------------------
# Create an empty data frame to store the results
type_token_words <- tibble(
  doc_id = character(),
  num_types = integer(),
  num_tokens = integer(),
  ratio = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_types and num_tokens
  count_lang <- current_df %>%
    group_by(doc_id) %>%
    summarise(
      num_types = length(unique(token)),
      num_tokens = n(),
      ratio = length(unique(token)) / n(),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert doc_id to character before combining data frames
  count_lang$doc_id <- as.character(count_lang$doc_id)
  
  # Add the information to type_token_words
  type_token_words <- bind_rows(type_token_words, count_lang)
}

# Remove rows with empty or null doc_id
type_token_words <- type_token_words %>%
  filter(!is.na(doc_id) & doc_id != "")

# ------------------------ Type-Token Ratio DPOS (TTR_p) ----------------------------
# Create an empty data frame to store the results
type_token_dpos <- tibble(
  doc_id = character(),
  num_p_types = integer(),
  ttr_p = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_p_types
  count_lang <- current_df %>%
    group_by(doc_id) %>%
    summarise(
      num_p_types = length(unique(upos)),
      ttr_p = length(unique(upos)) / n(),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert doc_id to character before combining data frames
  count_lang$doc_id <- as.character(count_lang$doc_id)
  
  # Add the information to type_token_dpos
  type_token_dpos <- bind_rows(type_token_dpos, count_lang)
}

# Remove rows with empty or null doc_id
type_token_dpos <- type_token_dpos %>%
  filter(!is.na(doc_id) & doc_id != "")
# ------------------------ Type-Token Ratio Dep (TTR_d) ----------------------------
# Create an empty data frame to store the results
type_token_dep <- tibble(
  doc_id = character(),
  num_d_types = integer(),
  ttr_d = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_d_types
  count_lang <- current_df %>%
    group_by(doc_id) %>%
    summarise(
      num_d_types = length(unique(dep_rel)),
      ttr_d = length(unique(dep_rel)) / n(),
      language = "english-l2", 
      column_name = paste0("line_", i)
    )
  
  # Convert doc_id to character before combining data frames
  count_lang$doc_id <- as.character(count_lang$doc_id)
  
  # Add the information to type_token_dep
  type_token_dep <- bind_rows(type_token_dep, count_lang)
}

# Remove rows with empty or null doc_id
type_token_dep <- type_token_dep %>%
  filter(!is.na(doc_id) & doc_id != "")

# ------------------------ Dependency Ratio (d-ratio) ----------------------------
# Create an empty data frame to store the results
dependency_ratio_passage <- tibble(
  doc_id = character(),
  language = character(),
  d_ratio = double(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Calculate n_heads and n_tokens per doc_id
  head_token_counts <- current_df %>%
    group_by(doc_id) %>%
    summarise(
      n_heads = length(unique(head_token_id)),
      n_tokens = n()
    )
  
  # Calculate d_ratio per doc_id
  head_token_counts <- head_token_counts %>%
    mutate(
      d_ratio = 1 - (n_heads / n_tokens),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert doc_id to character before combining data frames
  head_token_counts$doc_id <- as.character(head_token_counts$doc_id)
  
  # Add the information to dependency_ratio_passage
  dependency_ratio_passage <- bind_rows(dependency_ratio_passage, head_token_counts)
}

# Remove rows with empty or null doc_id
dependency_ratio_passage <- dependency_ratio_passage %>%
  filter(!is.na(doc_id) & doc_id != "")

# ------------------------ Mean Length of Utterance (MLU) ----------------------------
# Create an empty data frame to store the results
mlu_passage <- tibble(
  doc_id = character(),
  language = character(),
  mlu = double(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with dep_rel as punctuation
  current_df <- current_df[current_df$dep_rel != "PUNCT", ]
  
  # Ensure doc_id is character type
  current_df$doc_id <- as.character(current_df$doc_id)
  
  # Calculate mlu per doc_id
  mlu_per_passage <- current_df %>%
    group_by(doc_id) %>%
    summarise(
      mlu = mean(length(unique(token_id))),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Add information to mlu_passage
  mlu_passage <- bind_rows(mlu_passage, mlu_per_passage)
}

# Remove rows with empty or null doc_id
mlu_passage <- mlu_passage %>%
  filter(!is.na(doc_id) & doc_id != "")
# ------------------------ Embeddedness ----------------------------
# Create an empty data frame to store the results
embeddedness_passage <- tibble(
  doc_id = character(),
  language = character(),
  embeddedness = double(),
  num_complex_sentences = double(),
  num_simplex_sentences = double(),
  num_sentences = double(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Identify complex sentences
  current_df <- current_df %>%
    group_by(doc_id) %>% 
    mutate(is_complex = if_else(
      any(dep_rel %in% c("parataxis", "xcomp", "ccomp", "advcl", "acl:relcl", "acl", "conj", "cc", "mark")), 1, 0)
    ) %>% 
    ungroup()
  
  # Keep only the first row of each group (slice(1))
  df_complexity <- current_df %>%
    group_by(doc_id) %>% 
    slice(1) %>% 
    select(is_complex, doc_id) %>%  # Ensure the doc_id column is selected
    ungroup()
  
  # Ensure doc_id is character type
  df_complexity$doc_id <- as.character(df_complexity$doc_id)
  
  # Calculate Embeddedness per doc_id
  num_complex_sentences <- sum(df_complexity$is_complex)
  num_simplex_sentences <- nrow(df_complexity) - num_complex_sentences
  embeddedness <- 1 - (num_complex_sentences / num_simplex_sentences)
  
  # Add information to embeddedness_passage
  embeddedness_passage <- bind_rows(
    embeddedness_passage,
    tibble(
      doc_id = unique(df_complexity$doc_id),
      language = "english-l2",  
      embeddedness = embeddedness,
      num_complex_sentences = num_complex_sentences,
      num_simplex_sentences = num_simplex_sentences,
      num_sentences = nrow(df_complexity),
      column_name = paste0("line_", i)
    )
  )
}

# Remove rows with empty or null doc_id
embeddedness_passage <- embeddedness_passage %>%
  filter(!is.na(doc_id) & doc_id != "")

# ------------------------ Longest dependency path (LDP) ----------------------------
# Create an empty data frame to store the results
depth_data_passage <- tibble(
  column_name = character(),
  language = character(),
  doc_id = integer(),
  depth = double()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Create doc_sent_id
  current_df$doc_sent_id <- str_c(current_df$doc_id, current_df$sentence_id, sep = "_")
  
  # Initialize my_depth
  my_depth <- tibble(doc_sent_id = unique(current_df$doc_sent_id), depth = as.numeric(NA))
  
  # Calculate max depth for each sentence
  for (s in unique(current_df$doc_sent_id)) {
    tmp_depth <- current_df %>% filter(doc_sent_id == s) %>% droplevels()
    
    # Calculate max depth 
    x <- tmp_depth[!is.na(tmp_depth$head_token_id), ]
    x <- x[x$doc_id %in% min(x$doc_id), ]
    edges <- x[x$head_token_id != 0, c("token_id", "head_token_id")]
    
    # Get number of branches
    max_depth <- igraph::diameter(igraph::graph_from_data_frame(edges,
                                                                vertices = x[, c("token_id")],
                                                                directed = TRUE))
    
    my_depth[my_depth$doc_sent_id == s,]$depth <- max_depth
  }
  
  # Join with tmp_doc_id
  tmp_doc_id <- current_df %>% select(doc_id, doc_sent_id) %>% distinct(doc_sent_id, .keep_all = TRUE)
  my_depth <- left_join(my_depth, tmp_doc_id, by = "doc_sent_id")
  
  # Extract doc_id
  my_depth$doc_id <- as.numeric(substr(my_depth$doc_sent_id, 1 + regexpr(pattern = "_", text = my_depth$doc_sent_id), nchar(my_depth$doc_sent_id)))
  
  # Add information to depth_data
  depth_data_passage <- bind_rows(
    depth_data_passage,
    tibble(
      column_name = paste0("line_", i),
      language = "english-l2",  
      doc_id = my_depth$doc_id,
      depth = my_depth$depth
    )
  )
}

save(noun_to_verb_passage, type_token_words, type_token_dpos, 
     type_token_dep, dependency_ratio_passage, mlu_passage, 
     embeddedness_passage, depth_data_passage, file = "sentence_complexity_l2.rda",
     compress = "xz")


rm(list = ls())
load('passage_complexity_l2.rda')
depth_data_passage$sentence_id = as.character(depth_data_passage$sentence_id)

list_df = list(dependency_ratio_passage, 
               depth_data_passage, 
               embeddedness_passage,
               mlu_passage,
               noun_to_verb_passage,
               type_token_dep,
               type_token_dpos,
               type_token_words
)
all_sentence_l2 <- list_df %>% reduce(full_join, by = c("language", 
                                                        "column_name",
                                                        "sentence_id"))

save(all_sentence_l2, file = "passage_l2_summary.rda")
