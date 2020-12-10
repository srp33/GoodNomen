# The input argument is a vector of strings. These are terms (including synonyms)
# from a controlled terminology (ontology). This function creates a "clean" version
# of each term. The clean version is lowercase. "Stop words" are removed, as well
# as some punctuation or extra whitespace.
buildTermTibble <- function(terms) {
  termsDF <- tibble(term = terms) %>%
    unnest_tokens(word, term, drop=FALSE) %>%
    anti_join(stop_words) %>%
    dplyr::rename(originalTerm = term) %>%
    group_by(originalTerm) %>%
    summarize(cleanedTerm=paste(word, collapse=" ")) %>%
    ungroup() %>%
    group_by(cleanedTerm) %>%
    summarize(originalTerm = collapseTerms(originalTerm)) %>%
    ungroup()
  
  return(termsDF)
}

# When a cleaned term is the same for two original terms, 
# collapse them as |-separated values.
collapseTerms <- function(terms) {
  paste0(unique(terms), collapse = "|")
}

# Finds matches between standardized terms and synonyms
identifyMatches <- function(originalTerms) {
  # Start a timer
  startTime <- Sys.time()
  
  # The "withProgress" adds a progress bar. The functions "inc()" in this section increment the progress bar.
  withProgress(message = "Matching", {
    m <- 100
    incProgress(5/m, detail = "Cleaning terms")
    
    # Build a tibble with "clean" terms alongside the original terms.
    originalTerms <- buildTermTibble(originalTerms)
    incProgress(5/m, detail = "Mapping to standardized terms")
    
    # Use the stringdist package to map the test terms to the ontology terms (including synonyms).
    sdm <- stringdistmatrix(pull(values$totalTermList, cleanedTerm), pull(originalTerms, cleanedTerm), method = "jw", p = 0.1)
    incProgress(5/m, detail = "Organizing matches")
    
    # We get a matrix back. Convert it to a tibble and label columns descriptively.
    colnames(sdm) <- pull(originalTerms, originalTerm)
    sdm <- as_tibble(sdm)
    sdm <- bind_cols(pull(values$totalTermList, originalTerm), sdm, .name_repair = "minimal")
    colnames(sdm)[1] <- "OntologyTerm"
    incProgress(5/m, detail = "Tidying matches")
    
    # Check if there are any synonyms
    # If not don't worry about splitting the synonyms
    # Just tidy data and give top 10 matches
     if (all(is.na(values$synonyms))) {
       sdm <- pivot_longer(sdm, 2:ncol(sdm), names_to="TestTerm", values_to="Score") %>%
         group_by(TestTerm) %>%
         slice_min(n = 10, order_by = Score)
     }

    # Create a tidy version of the results. It includes steps for un-collapsing terms that were combined in previous steps and just making the results easier to work with.
    else {
      sdm <- pivot_longer(sdm, 2:ncol(sdm), names_to="TestTerm", values_to="Score") %>%
        group_by(TestTerm) %>%
        slice_min(n = 10, order_by = Score) %>%
        separate_rows(OntologyTerm, sep="\\|") %>%
        mutate(OntologyTerm = sapply(OntologyTerm, function(x) {
          index <- which(values$synonyms == x)
          incProgress(1/m, detail = "Collecting matches")
          values$preferred[[index[1]]] })) %>%
        separate_rows(TestTerm, sep="\\|") %>%
        group_by(TestTerm, OntologyTerm) %>%
        summarize(Score = min(Score)) %>%
        select(TestTerm, OntologyTerm, Score) %>%
        arrange(TestTerm, Score)
    }
    
    incProgress(5/m, detail = "Finishing")
    
    # Indicate how long the process took.
    duration <- Sys.time() - startTime
    print("#############################")
    print(duration)
    print("#############################")
    sdm <- sdm[!duplicated(sdm$TestTerm),]
    
  })
  return(sdm)
}