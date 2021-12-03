#' @title Get gnomAD API URL
#' @description
#' Returns the gnomAD API URL as a character. Checks if the option
#' 'gnomadr.apiurl' has been set to allow for hypothetical parameterization
#' in the future
#' @return Character with gnomAD API URL
#' @export

apiUrl <- function() {
  getOption("gnomadr.apiurl", "https://gnomad.broadinstitute.org/api")
}

qfailmessage <-
  c("Query failed. This is likely due to the size of the query. At this time ",
    "the gnomAD API has very low query limits. This package is designed to ",
    "accomodate large queries in the event gnomAD increases query limits in ",
    "the future.")

#' @name gnomad-ids
#' @title gnomAD IDs and associated helper functions
#' @title Validate/associate reference genome build ID and gnomAD data set ID
#' @description
#' Description here.

NULL

#' @rdname gnomad-ids
#' @importFrom glue glue
#' @export

ReferenceGenomeIds <- c("GRCh38", "GRCh37")
attr(ReferenceGenomeIds, "defDatasetOpt") <-
  glue("gnomadr.{ReferenceGenomeIds}.dataset")
attr(ReferenceGenomeIds, "defDataset") <- c("gnomad_r3", "gnomad_r2_1")
attr(ReferenceGenomeIds, "liftoverIdName") <-
  c("liftover_variant_id", "source_variant_id")

#' @rdname gnomad-ids
#' @export

DatasetIds <- c("gnomad_r3",
                "gnomad_r3_controls_and_biobanks",
                "gnomad_r3_non_cancer",
                "gnomad_r3_non_neuro",
                "gnomad_r3_non_topmed",
                "gnomad_r3_non_v2",
                "gnomad_r2_1",
                "gnomad_r2_1_controls",
                "gnomad_r2_1_non_neuro",
                "gnomad_r2_1_non_cancer",
                "gnomad_r2_1_non_topmed",
                "exac")
attr(DatasetIds, "associatedGenome") <- rep(ReferenceGenomeIds, each = 6)

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

validGenomes <- function(genomes) all(genomes %in% ReferenceGenomeIds)

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

validDatasets <- function(datasets) all(datasets %in% DatasetIds)

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

compatibleGenomeDataset <- function(datasets, genomes) {
  stopifnot(validDatasets(datasets))
  stopifnot(validGenomes(genomes))
  identical(genomes, getGenomes(datasets))
}

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

getGenomes <- function(datasets) {
  stopifnot(validDatasets(datasets))
  attr(DatasetIds, "associatedGenome")[match(datasets, DatasetIds)]
}

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

getDatasets <- function(genomes) {
  stopifnot(validGenomes(genomes))
  rgi <- match(genomes, ReferenceGenomeIds)
  dsopt <- attr(ReferenceGenomeIds, "defDatasetOpt")[rgi]
  dsdef <- attr(ReferenceGenomeIds, "defDataset")[rgi]
  ds <- mapply(getOption, x = dsopt, default = dsdef, USE.NAMES = FALSE)
  stopifnot(compatibleGenomeDataset(datasets = ds, genomes = genomes))
  ds
}

#' @rdname gnomad-ids
#' @inheritParams pkgParams
#' @export

getLiftoverIdName <- function(genomes) {
  stopifnot(validGenomes(genomes))
  attr(ReferenceGenomeIds, "liftoverIdName")[match(genomes, ReferenceGenomeIds)]
}

#' @importFrom glue glue glue_collapse
#' @import ghql

.makeAndEvalQuery <- function(qfmt, maxTries = 3) {
  gmCon <- GraphqlClient$new(url = apiUrl())
  qryBody <- glue_collapse(glue(qfmt), sep = "\n")
  qry <- Query$new()$query('q', glue('query {{ {qryBody} }}'))
  tries <- 1
  repeat {
    if (tries > maxTries) break
    tryres <- try(gmCon$exec(qry$q), silent = TRUE)
    if (!is(tryres, 'try-error')) break
    Sys.sleep(2*tries)
    tries <- tries + 1
  }
  tryres
}
