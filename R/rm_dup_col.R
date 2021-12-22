#' Note: This function is currently very specific and only used in walsh_curation.Rmd
replace.header <- function(x){   
  x <- x %>% select_if(~sum(!is.na(.)) > 0)
  names(x) <- x[1,]
  x <- x[2:(nrow(x)-1),]
  names(x)[which(str_detect(names(x), ".xlsx"))] <- "filename"
  return(x)
}

#' Note: This function is currently very specific and only used in walsh_curation.Rmd
#' Extract year from filenames within a data.frame
extract.year <- function(df){
  years_extract_ls <- str_extract_all(df$filename, "\\d+")
  years_extract = lapply(1:length(years_extract_ls), function(x){
    a = years_extract_ls[[x]][lengths(years_extract_ls[x])]
  })
  
  years_extract = as.numeric(unlist(years_extract))
  years_extract
}

#' Write multiple sheets to xlsx file
#' 
#' Write multiple sheets to xlsx file to help collaborators check or input 
#' needed data.
#' The first sheet will be the dataset to amend, followed by the codebook (metadata)
#' for that file and the controlled vocabularies for columns that require them.
#' @param dataset A data.frame that corresponds to data or metadata that will be checked or amended
#' @param db_filename A string that corresponds to the type of dataset (See names(db) and db$codebooks_all_db.csv for options)
#' @param datasetname A string for naming the file
#' @import openxlsx
write.multxlsxsheets <- function(dataset, db_filename, datasetname){
  codebook <- db$codebooks_all_db.csv %>% filter(book == db_filename)
  
  codebook_arrange <- codebook %>% arrange(factor(variable, levels = names(dataset)))
  # may want to add an warning if there are columns in the codebook that aren't present in the dataset
  
  cb_req <- (codebook_arrange %>% 
               filter(value_type == "categorical") %>% 
               filter(!is.na(values_defined_in)) %>% 
               select(values_defined_in) %>% 
               unique(.))$values_defined_in
  
  cb <- lapply(cb_req, function(x) {
    if (x == "master_cultivar_file"){
      db$master_cultivar_file_2020June8.csv
    } else {
      db[[paste0(x,".csv")]]
    }
  })
  
  list_of_datasets <- c(list(dataset, codebook_arrange), cb)
  
  datasetname_new <- paste0(datasetname, "_new")
  
  names(list_of_datasets) <- c(datasetname_new, paste0(datasetname,"_metadata"), cb_req)
  
  write.xlsx(list_of_datasets, file = paste0(knitroutputfolder, "/", datasetname_new, ".xlsx")) # add full path
  
}


#' Return a data.frame from a list based on the element name
#' @param data_list A named list
#' @param file_sheet A string that refers to a name of one element in the list
return.datasetfmlist <- function(data_list, file_sheet){
  
  idx <- which(names(data_list) == file_sheet)
  
  temp <- data_list[[idx]]
  return(temp)
  
}

#' Read in curation file and join with data
#' 
#' @param df A data.frame of the raw data that was used to output the curation file
#' @param col_sel A string of the name of the column that was curated
#' @param curation_file_path A string of the path to the curation files
readin_join_rename <- function(df, col_sel, curation_file_path){
  path <- paste0(curation_file_path, "/", 
                 substitute(df), "_", col_sel, "_unique_rename.csv")
  
  rename_file <- read.csv( path, stringsAsFactors = FALSE)
  
  df_rename <- left_join(df, rename_file) %>% 
    rename(., !!paste0(col_sel, "_temp") := controlled_vocab)
  return(df_rename)
  
}

#' Remove duplicate columns
#' 
#' Remove duplicate columns.  
#' If the columns are duplicates, except one column contains NAs, then
#' the column with NAs is removed
#' #' test_dup <- duplicated(as.list(df)) This method detects duplicates 
#' but does not take into account NAs
#' @param df A data.frame
#' @param concat_names A logical denoting how to rename the columns
#' The default is TRUE, which renames the column by concatenating the names of
#' all the duplicate columns
#' FALSE will retain the first 
rm.dup_col <- function(df, concat_names = TRUE){
  df <- as.data.frame(df) # cannot be a tibble otherwise identical() will fail if column names differ
  
  cols <- list(first = 1:ncol(df), second = 1:ncol(df))
  cols_compare <- cols %>% cross_df() %>% arrange(first) %>% filter(second > first)
  
  col_matches <- map2(cols_compare$first, cols_compare$second, function(x, y){
    df_2col <- data.frame(first = df[,x], second = df[,y])
    df_2col_rmna <- df_2col %>% drop_na()
    col_identical <- identical(df_2col$first, df_2col$second)
    
    # if after removing all NA, there are no rows, then they cannot match
    if (nrow(df_2col_rmna)>0){
      colna_identical <- identical(df_2col_rmna$first, df_2col_rmna$second)
    } else {
      colna_identical <- FALSE
    }
    
    # Test if column has NA 
    # Need to remove rows with NA in both because R doesn't detect these as matches
    df_2col_rmmatchingna <- df_2col %>% filter(!(is.na(first) & is.na(second)))
    cols_wna_rmmatchingna <- df_2col_rmmatchingna %>%
      summarise(across(everything(), ~ any(is.na(.))))
    
    return(data.frame(x,
                      y, 
                      col_identical, 
                      colna_identical, 
                      x_isna = cols_wna_rmmatchingna$first, 
                      y_isna = cols_wna_rmmatchingna$second))
  })
  cols_rmna <- bind_rows(col_matches) 
  c2 <- cols_rmna %>% filter(x!= y) %>% filter(colna_identical == TRUE)
  
  c3 <- 
    c2 %>% 
    mutate(coln_rm = ifelse(
      col_identical == TRUE & 
        (x_isna == TRUE & y_isna == TRUE), y,
      ifelse(col_identical == FALSE & (x_isna == TRUE & y_isna == TRUE), NA, 
             ifelse(col_identical == FALSE & (x_isna == TRUE & y_isna == FALSE), x, y)))) %>%
    mutate(coln_keep = ifelse(coln_rm == x, y, x))
  
  
  if (any(is.na(c3$coln_rm))){
    almost_dup <- unlist(c3 %>% filter(is.na(coln_rm)) %>% select(x,y))
    print(paste(paste(almost_dup, collapse = ", "), "- Both columns contain NAs, so both columns are retained"))
    c3 <- c3 %>% drop_na()
  }
  
  # Remove duplicate columns
  c4 <- c3 %>% 
    mutate(name_keep = names(df)[coln_keep]) %>% 
    mutate(name_rm = names(df)[coln_rm]) %>% 
    select(name_keep, name_rm) %>% 
    group_by(name_keep) %>%
    mutate(rename_keep_mult = paste(unique(name_rm), collapse="..")) %>%
    mutate(rename_keep = paste(name_keep, rename_keep_mult, sep = "..")) 
  
  names2rm <- unique(c4$name_rm)
  z_clean <- df %>% select(-all_of(names2rm))
  
  if (length(names2rm) > 0){ # rename.col() only works if there are columns remove in c4
    print(paste("Removing:", paste(names2rm, collapse = ", ")))
    
    if (concat_names){ # Rename columns based on original and the duplicate(s)
      rename_df <- c4 %>% 
        filter(! name_keep %in% c4$name_rm) %>%
        select(name_keep, rename_keep) %>%
        unique(.)
      
      # This is a bit hacky because rename.col() was developed for lists
      z_clean <- rename.col(list(z_clean), 
                            rename_df = rename_df, 
                            rename_col = rename_keep, 
                            old_col = name_keep,
                            rm_col = FALSE)[[1]]
    }
    
    
  } else {
    print("No columns removed")
  }
  
  return(z_clean)
}

#' Test whether a column has identical elements
#' 
#' Test whether a column has identical elements.  
#' The function will return TRUE if:
#' all values are equal,
#' all values are equal and there are NAs,
#' all values are NA.
#' https://stackoverflow.com/questions/31968623/how-to-check-whether-a-column-contains-only-identical-elements-in-r#:~:text=You%20can%20use%20the%20duplicated,column%20contains%20all%20identical%20values.&text=If%20x%20contains%20NAs%20this,mixed%20columns%20will%20return%20FALSE%20.
test.col_id_elements <- function(df, colname){
  
  dim(table(df[[colname]], useNA = "no")) <= 1
  
}


#' Full join data.frames to compare columns
#' add a suffix before the merge to track where the
#' column originated
#' @param suffix1 A string denoting the suffix to attach to the columns
#'   associated with the first data.frame.  If NA, no suffix attached
full_join_compare <- function(df1, df2, by, suffix1, suffix2){
  
  if (is.null(names(by))){ names(by) = by }
  
  join_names <- names(by)
  
  if (!is.na(suffix1)){ 
    names(df1) <- paste(names(df1), suffix1, sep = "_")
    join_names <- paste(join_names, suffix1, sep = "_")
  }
  
  if (!is.na(suffix2)){ 
    names(df2) <- paste(names(df2), suffix2, sep = "_")
    by <- paste(by, suffix2, sep = "_")
  }
  
  
  names(by) <- join_names
  
  test <- full_join(df1, df2, by = by) %>% 
    select(sort(names(.)))
  
}



