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
#' @import ghql
#' @importFrom glue glue glue_collapse
#' @importFrom jsonlite fromJSON
#' @export

convertVariantIds <- function(varids, genomes) {
  stopifnot(validGenomes(genomes))
  gmCon <- GraphqlClient$new(url = apiUrl())
  datasets <- getDatasets(genomes)
  idnames <- getLiftoverIdName(genomes)
  tmp <-
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
  qryBody <- glue_collapse(glue(tmp), sep = "\n")
  qry <- Query$new()$query('convertIds',
                           glue('query convertIds {{ {qryBody} }}'))
  tryres <- try(jsn <- gmCon$exec(qry$convertIds), silent = TRUE)
  if (is(tryres, 'try-error')) stop(qfailmessage)
  res <- fromJSON(jsn, flatten = TRUE)$data
  res
}

