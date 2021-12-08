#' @title Get variant population data
#' @inheritParams pkgParams
#' @param genomes The genome(s) associated with the given variant_id
#' @description
#' Returns the variant_id(s) for the alternate reference genome
#'
#' @return List of population data for the given variant_id(s)
#'
#' @examples
#' \dontrun{
#' getVariantPopData(varids = '1-88923261-A-C', genomes = "GRCh38")
#' }
#'
#' @import ghql
#' @importFrom glue glue glue_collapse
#' @importFrom jsonlite fromJSON
#' @export

getVariantPopData <- function(varids, genomes) {
  stopifnot(validGenomes(genomes))
  gmCon <- GraphqlClient$new(url = apiUrl())
  datasets <- getDatasets(genomes)
  qfmt <-
  '
    {genomes}_{gsub("-", "_", varids)}:
    variant(variantId: "{varids}", dataset: {datasets}) {{
      variantId
      chrom
      pos
      genome {{
        populations {{
          id
          ac
          an
          homozygote_count
          hemizygote_count
        }}
      }}
    }}
  '
  rsp <- .makeAndEvalQuery(qfmt, environment())
  if (is(rsp, 'failedQuery')) return(rsp)
  resLst <- fromJSON(rsp, flatten = TRUE)$data
  procPopData <- function(x) {
    cbind(varid = x$variantId,
          chrom = x$chrom,
          pos = x$pos,
          x$genome$populations)
  }
  res <- lapply(resLst, procPopData)
  res
}

