################################################################################
# PubMLST.org webpage scraper
#
# Author: Vladimir Bajić
# Date: January 2024
#
# Description:
# This script allows
#   - listing all of the available schemes on cgMLST.org
#   - finding the date and time of the last change on scheme of interest
#   - downloading schemes from cgMLST.org
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
#
# TODO
# - give error message if scheme_id or orgnaism_id doesn't exist
# - change species_id to orgnaism_id in table
# - add organism_id in table output of listing available schemes
# - check if same scheme exists and if so do not download
#
################################################################################



# Libraries --------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))
library(knitr)
library(optparse)

# Functions --------------------------------------------------------------------

## Function to get list of seqdef for given organisms alphabetical page
list_seqdef_from_url <- function(url) {
    url %>%
        read_html() %>%
        html_nodes("a.collection-typing") %>%
        html_attr("href") %>%
        return()
}

## Function to get table of seqdef, organism_id, and organism name for one page
list_organism_info_from_url <- function(url) {
    ### Base url to pubmlst.org
    base_url <- "https://pubmlst.org"

    html <-
        url %>%
        read_html()

    urls <-
        html %>%
        html_nodes("a.collection-typing") %>%
        html_attr("href") %>%
        return()

    species_id <-
        urls %>%
        str_remove("_seqdef") %>%
        str_remove("/bigsdb\\?db=pubmlst_")

    species_names <-
        html %>%
        html_nodes("h3") %>%
        html_text(.)

    organism_table <-
        tibble(species_id, species_names, urls) %>%
        mutate(urls = paste(base_url, urls, sep = ""))

    return(organism_table)
}

## Function to list all available organisms on pubMLST
list_organisms <- function() {
    ### Base url to pubmlst.org
    base_url <- "https://pubmlst.org"

    ### make empty tibble which will be expanded in the loop
    organisms_tibble <- NULL

    ### To make a complete list of all organisms included in pubmslt.org (n=139)
    ### make a list of pages to be searched and include page 0 manually
    page_list <-
        paste(base_url, "/organisms", sep = "") %>%
        read_html() %>%
        html_nodes("a.page-link") %>%
        html_attr("href") %>%
        unique() %>%
        append("?title=&page=0", .) %>%
        paste(base_url, "/organisms", ., sep = "")

    for (i in 1:length(page_list)) {
        cat("Searching for organisms on pubmlst.org page:", i, "/", length(page_list), " \n")
        tmp_tibble <- list_organism_info_from_url(page_list[i])
        organisms_tibble <- bind_rows(organisms_tibble, tmp_tibble)
    }

    ### print in formatted way
    organisms_tibble %>%
        select(-urls) %>%
        kable() %>%
        print()
}

### Function to list available schemes for given organism_id unformatted
list_organism_schemes_raw <- function(organism_id) {
    ### Base url to pubmlst.org
    base_url <- "https://pubmlst.org"

    ### save html
    tmp_html <-
        paste(base_url, "/bigsdb?db=pubmlst_", organism_id, "_seqdef&page=schemes", sep = "") %>%
        read_html()

    ### extract urls for downloads
    download_urls <-
        tmp_html %>%
        html_nodes("table.resultstable a") %>% # Selecting all 'a' elements within the table
        html_attr("href") %>%
        tibble(Download = .) %>% # Convert to tibble
        filter(grepl("downloadProfiles", Download, ignore.case = TRUE))

    ### extract table from html
    tmp_table <-
        tmp_html %>%
        html_table() %>%
        .[[1]] %>%
        mutate(Download = download_urls$Download) %>%
        mutate(scheme_id = sub(".*scheme_id\\=", "", Download)) %>%
        select(scheme_id, Name, `Last updated`, Profiles, `Curator(s)`, Description, Download)

    return(tmp_table)
}

## Function to list available schemes for given organism_id formatted
list_organism_schemes <- function(organism_id) {
    ### print formatted table
    list_organism_schemes_raw(organism_id) %>%
        select(scheme_id, Name, `Last updated`, Profiles) %>%
        kable() %>%
        print()
}

## Function to download schema PROFILES based on organism_id and scheme_id
download_scheme_profiles <- function(oid, sid) {
    ### Base url to pubmlst.org
    base_url <- "https://pubmlst.org"

    ### gather scheme information
    sinfo <-
        list_organism_schemes_raw(oid) %>%
        filter(`scheme_id` == sid)

    ### make scheme timestamp for the name of the scheme to be downloaded
    scheme_timestamp <- paste0("sid", sinfo$scheme_id, "_LastUpdated_", sinfo$`Last updated`)

    ### create path to where the scheme will be downloaded
    destfile_path <- paste0(oid, "_profiles_", scheme_timestamp, ".txt")

    download.file(
        paste0(base_url, "/bigsdb?db=pubmlst_", oid, "_seqdef&amp;page=downloadProfiles&amp;scheme_id=", sid, sep = ""),
        destfile = destfile_path,
        method = "auto"
    )
}

## Function to download fasta files of alleles included in schema
download_scheme <- function(oid, sid) {
    ### Base url to pubmlst.org
    base_url <- "https://pubmlst.org"

    ### save html
    tmp_html <-
        paste(base_url, "/bigsdb?db=pubmlst_", oid, "_seqdef&page=downloadAlleles&scheme_id=", sid, sep = "") %>%
        read_html()

    ### extract download links
    download_links <-
        tmp_html %>%
        html_nodes("table") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        tibble(Download = .) %>% # Convert to tibble
        filter(grepl("downloadAlleles", Download, ignore.case = TRUE)) %>%
        mutate(Download = paste0(base_url, Download, sep = ""))

    ### store table with information about scheme and alleles
    scheme_table <-
        paste0(base_url, "/bigsdb?db=pubmlst_", oid, "_seqdef&amp;page=downloadAlleles&scheme_id=", sid, sep = "") %>%
        read_html() %>%
        html_table() %>%
        .[[1]] %>%
        mutate(Download = download_links$Download) %>%
        mutate(locus_id = sub(".*locus\\=", "", Download))

    ### gather scheme information
    sinfo <-
        list_organism_schemes_raw(oid) %>%
        filter(`scheme_id` == sid)

    ### make scheme timestamp for the name of the scheme to be downloaded
    scheme_timestamp <- paste0("schemeID_", sinfo$scheme_id, "_LastUpdated_", sinfo$`Last updated`)

    ### create path to where the scheme will be downloaded
    destfile_path <- paste0(oid, "_", scheme_timestamp)

    ### create dir where fasta files will be stored
    dir.create(destfile_path)

    ### loop to download all alleles
    for (i in seq_along(scheme_table$Download)) {
        cat("Downloading allele No.", i, "/", length(scheme_table$Download), "\n")
        download.file(
            scheme_table$Download[i],
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
            \t| list_organisms            |   to list all available organisms on pubMLST
            \t| list_organism_schemes     |   to list all available PubMLST schemes for a given organism_id
            \t| download_scheme_profiles  |   to download schema PROFILES based on organism_id and scheme_id
            \t| download_scheme           |   to download fasta files with loci and alleles included in schema \n"
    ),
    make_option(c("-o", "--organismID"),
        type = "character", metavar = "character",
        help = "organism ID on which to perform function\n"
    ),
    make_option(c("-s", "--schemeID"),
        type = "character", metavar = "character",
        help = "scheme ID on which to perform function\n"
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
        cat("\nPlease provide organism_ID using argument '-o' for which you want to see the list of available schemes\n\n")
    } else {
        list_organism_schemes(opt$o)
    }
} else if (opt$f == "download_scheme_profiles") {
    if (is.null(opt$o) | is.null(opt$s)) {
        cat("\nPlease provide organism_ID and scheme_ID using arguments '-o' and '-s'\n\n")
    } else {
        download_scheme_profiles(opt$o, opt$s)
    }
} else if (opt$f == "download_scheme") {
    if (is.null(opt$o) | is.null(opt$s)) {
        cat("\nPlease provide organism_ID and scheme_ID using arguments '-o' and '-s'.\n\n")
    } else {
        download_scheme(opt$o, opt$s)
    }
}
