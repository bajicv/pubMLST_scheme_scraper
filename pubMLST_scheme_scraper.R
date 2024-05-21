################################################################################
# PubMLST webpage scraper (https://rest.pubmlst.org)
#
# Author: Vladimir Bajić
# Date: 2024-05-21
#
# Description:
# This script allows
#   - listing all of the available schemes on PubMLST
#   - finding the date and time of the last change on scheme of interest
#   - downloading schemes
#   - downloading schemes profiles
#
#
# Usage:
#
# To see help
#   Rscript --vanilla pubMLST_scheme_scraper.R --help
#
# To list available organisms on PubMLST
#   Rscript --vanilla pubMLST_scheme_scraper.R -f list_organisms
#
# To list available schemes for given organism
#   Rscript --vanilla pubMLST_scheme_scraper.R -f list_organism_schemes -o abaumannii
#
# To download scheme profiles for a given organism and scheme ID
#   Rscript --vanilla pubMLST_scheme_scraper.R -f download_scheme_profiles -o abaumannii -s 1
#
# To download scheme
#   Rscript --vanilla pubMLST_scheme_scraper.R -f download_scheme -o abaumannii -s 1
#
################################################################################



# Libraries --------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))
suppressMessages(library(knitr))
suppressMessages(library(optparse))
suppressMessages(library(jsonlite))

# Define base URL to PubMLST (API) ---------------------------------------------
base_url <- "https://rest.pubmlst.org"

# Functions --------------------------------------------------------------------

## Function to list all available organisms on pubMLST
list_organisms <- function() {
    organism_table <-
        base_url %>%
        fromJSON() %>%
        as_tibble() %>%
        filter(name != "test") %>%
        unnest(cols = c(databases), names_sep = "_") %>%
        filter(grepl("_seqdef", databases_name)) %>%
        filter(!grepl("requires registration", long_description)) %>%
        select(-long_description, -description, -name) %>%
        mutate(organism = str_remove(databases_description, " sequence/profile definitions")) %>%
        rename(seqdefID = databases_name) %>%
        mutate(organismID = str_remove(seqdefID, "pubmlst_")) %>%
        mutate(organismID = str_remove(organismID, "_seqdef")) %>%
        rename(description = databases_description) %>%
        rename(href = databases_href) %>%
        select(organism, organismID) %>%
        kable() %>%
        print()
}

## Function to list available schemes for given organismID formatted
list_organism_schemes <- function(organismID) {
    ### Print formatted table
    paste0("https://rest.pubmlst.org/db/pubmlst_", organismID, "_seqdef/schemes") %>%
        fromJSON() %>%
        .$schemes %>%
        as_tibble() %>%
        mutate(schemeID = basename(scheme)) %>%
        select(schemeID, description) %>%
        kable() %>%
        print()
}

## Function to list available schemes for given organismID formatted
list_organism_cgMLST_schemes <- function(organismID) {
    ### Print formatted table
    paste0("https://rest.pubmlst.org/db/pubmlst_", organismID, "_seqdef/schemes") %>%
        fromJSON() %>%
        .$schemes %>%
        as_tibble() %>%
        mutate(schemeID = basename(scheme)) %>%
        select(schemeID, description) %>%
        filter(grepl("cgMLST", description)) %>%
        kable() %>%
        print()
}

## Function to get raw scheme information
scheme_info_raw <- function(organismID, schemeID) {
    paste0("https://rest.pubmlst.org/db/pubmlst_", organismID, "_seqdef/schemes/", schemeID) %>%
        fromJSON()
}

## Function to print scheme information
print_scheme_info <- function(organismID, schemeID) {
    scheme_info_raw(organismID, schemeID) %>%
        .[(names(.) %in% c("description", "id", "last_added", "last_updated", "locus_count", "records"))] %>%
        as_tibble() %>%
        rename(schemeID = id) %>%
        relocate(schemeID) %>%
        kable() %>%
        print()
}

## Function to download schema PROFILES based on organismID and schemeID
download_scheme_profiles <- function(organismID, schemeID) {
    ### Save scheme information
    scheme_info <- scheme_info_raw(organismID, schemeID)

    ### Make scheme timestamp for the name of the scheme to be downloaded
    scheme_timestamp <- paste0("schemeID_", scheme_info$id, "_LastUpdated_", scheme_info$last_updated)

    ### Create path to where the scheme will be downloaded
    destfile_path <- paste0(organismID, "_profiles_", scheme_timestamp, ".tsv")
    cat("\nDownloading scheme profile as: ", destfile_path, "\n")

    download.file(
        scheme_info$profiles_csv,
        destfile = destfile_path,
        method = "auto"
    )
}

## Function to download fasta files of alleles included in schema
download_scheme <- function(organismID, schemeID) {
    ### Save scheme information
    scheme_info <- scheme_info_raw(organismID, schemeID)

    ### Extract download links
    scheme_table <-
        as_tibble_col(scheme_info$loci, column_name = "loci_link") %>%
        mutate(download_links = paste0(loci_link, "/alleles_fasta")) %>%
        mutate(locus_id = basename(loci_link))


    ### Make scheme timestamp for the name of the scheme to be downloaded
    scheme_timestamp <- paste0("schemeID_", scheme_info$id, "_LastUpdated_", scheme_info$last_updated)

    ### Create path to where the scheme will be downloaded
    destfile_path <- paste0(organismID, "_", scheme_timestamp)

    ### Check if dir already exists and if yes do not download
    if (dir.exists(destfile_path)) {
        stop("WARNING: ", destfile_path, " already exists. \nDownloading aborted to prevent overwriting.\n")
    }

    ### Create dir where fasta files will be stored
    dir.create(destfile_path)
    cat("\nDownloading fasta files in: ", destfile_path, "\n")

        ### Loop to download all alleles
    for (i in 1:nrow(scheme_table)) {
        cat("Downloading allele No.", i, "/", nrow(scheme_table), "\n\n")
        download.file(
            scheme_table$download_links[i],
            paste0(destfile_path, "/", scheme_table$locus_id[i], ".fasta", sep = ""),
            mode = "auto"
        )
    }
}

#-------------------------------------------------------------------------------

# Making option list -----------------------------------------------------------
option_list <- list(
    make_option(c("-f", "--function"),
        type = "character", metavar = "character",
        help = "Function to be performed \n\n\t\tPossible functions are:\n
            \t| list_organisms            |   to list all available organisms on PubMLST
            \t| list_organism_schemes     |   to list all available PubMLST schemes for a given organismID
            \t| print_scheme_info         |   to print scheme information based on organismID and schemeID
            \t| download_scheme_profiles  |   to download schema PROFILES based on organismID and schemeID
            \t| download_scheme           |   to download fasta files with loci and alleles included in the schema \n"
    ),
    make_option(c("-o", "--organismID"),
        type = "character", metavar = "character",
        help = "organismID on which to perform function\n"
    ),
    make_option(c("-s", "--schemeID"),
        type = "character", metavar = "character",
        help = "schemeID on which to perform function\n"
    )
)
# Parsing options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check the provided option and execute the corresponding code -----------------
if (is.null(opt$f)) {
    print_help(opt_parser)
} else if (opt$f == "list_organisms") {
    list_organisms()
} else if (opt$f == "list_organism_schemes") {
    if (is.null(opt$o)) {
        cat("\nPlease provide organismID using argument '-o' for which you want to see the list of available schemes\n\n")
    } else {
        list_organism_schemes(opt$o)
    }
} else if (opt$f == "download_scheme_profiles") {
    if (is.null(opt$o) | is.null(opt$s)) {
        cat("\nPlease provide organismID and schemeID using arguments '-o' and '-s'\n\n")
    } else {
        download_scheme_profiles(opt$o, opt$s)
    }
} else if (opt$f == "download_scheme") {
    if (is.null(opt$o) | is.null(opt$s)) {
        cat("\nPlease provide organismID and schemeID using arguments '-o' and '-s'.\n\n")
    } else {
        download_scheme(opt$o, opt$s)
    }
} else if (opt$f == "print_scheme_info") {
    if (is.null(opt$o) | is.null(opt$s)) {
        cat("\nPlease provide organismID and schemeID using arguments '-o' and '-s'.\n\n")
    } else {
        print_scheme_info(opt$o, opt$s)
    }
}
