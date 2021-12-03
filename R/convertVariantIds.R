#' @title Get variant IDs for alternate reference genome
#' @inheritParams pkgParams
#' @param genomes The genome associated with the given variant_id
#' @description
#' Returns the variant_id(s) for the alternate reference genome
#'
#' @return variant_id(s)
#'
#' @examples
#' \dontrun{
#' convertVariantIds(varids = '1-89388944-A-C', genomes = "GRCh37")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @export

convertVariantIds <- function(varids, genomes) {
  stopifnot(validGenomes(genomes))
  datasets <- getDatasets(genomes)
  idnames <- getLiftoverIdName(genomes)
  qfmt <-
  '
    {genomes}_{gsub("-", "_", varids)}:
    liftover({idnames}: "{varids}", reference_genome: {genomes}) {{
      source {{
        variant_id
        reference_genome
      }}
      liftover {{
        variant_id
        reference_genome
      }}
    }}
  '
  rsp <- .makeAndEvalQuery(qfmt, environment())
  if (is(rsp, 'try-error')) return(rsp)
  res <- fromJSON(rsp, flatten = TRUE)$data
  res
}

