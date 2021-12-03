#' @title Get variants from region
#' @inheritParams pkgParams
#' @description
#' Returns the variant_id, ref, alt, rsids fields for all variants in the given
#' region(s).
#'
#' @return List of data.frames for each region given
#'
#' @examples
#' \dontrun{
#' getVariantsFromRegion(genomes = 'GRCh37',
#'                       chroms = c('1'),
#'                       starts = c(89388944))
#' }
#'
#' @import ghql
#' @importFrom glue glue glue_collapse
#' @importFrom jsonlite fromJSON
#' @export

getVariantsFromRegion <- function(genomes, chroms, starts, stops = starts) {
  stopifnot(validGenomes(genomes))
  gmCon <- GraphqlClient$new(url = apiUrl())
  datasets <- getDatasets(genomes)
  qfmt <-
  '
    {genomes}_{chroms}_{starts}_{stops}:
    region(chrom: "{chroms}",
           start: {starts},
           stop: {stops},
           reference_genome: {genomes}) {{
      variants(dataset: {datasets}) {{
        variant_id
        ref
        alt
        rsids
      }}
    }}
  '
  rsp <- .makeAndEvalQuery(qfmt, environment())
  if (is(rsp, 'try-error')) return(rsp)
  res <- lapply(fromJSON(rsp)$data, "[[", 1)
  res
}

