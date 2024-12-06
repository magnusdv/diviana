readGenemapper = function(file, format = NULL) {
  line1 = readLines(file, n = 1) |> strsplit("\t")
  line1 = line1[[1]]
  if(!"Marker" %in% line1)
    stop2("Unsupported file format. Expected the first line to contain 'Marker'\n:", line1)
  if(sum(line1 == "Marker") > 1)
    stop2("Unsupported file format (multiple markers per line)")

  x0 = read.table(file, header = TRUE, sep = "\t", colClasses = "character",
                 check.names = FALSE)
  x = x0[c("Sample Name", "Marker", "Allele 1", "Allele 2")]
  names(x) = c("Sample", "Marker", "A1", "A2")

  # All IDs
  ids = unique.default(x$Sample)

  # All markers
  markers = unique.default(x$Marker)

  # Simple format? All markers in same order for everyone
  ok1 = identical(x$Marker, rep(markers, length(ids)))
  ok2 = identical(x$Sample, rep(ids, each = length(markers)))
  ok = ok1 && ok2

  if(!ok) {
    stop2("Something unexpected in the file: Markers not consistently repeated.")
  }

  # Add homozygous alleles
  x$A2[x$A2 == ""] = x$A1[x$A2 == ""]

  # Remove "-NB"
  x$A1 = sub("-NB", "", x$A1, fixed = TRUE)
  x$A2 = sub("-NB", "", x$A2, fixed = TRUE)

  # Genotype matrix
  g = paste(x$A1, x$A2, sep = "/")
  gmat = matrix(g, nrow = length(ids), ncol = length(markers),
             dimnames = list(ids, markers), byrow = TRUE)

  # Convert to data frame (?)
  as.data.frame(gmat)
}


readGenoFromTxt = function(file) {
  x = read.table(file, header = TRUE, sep = "\t", colClasses = "character",
                 check.names = FALSE, row.names = NULL)

  # Use sample ID as rownames
  idcol = grep("sample", tolower(names(x)))
  if(length(idcol) == 0)
    stop2("No column with 'sample' in the name")
  rownames(x) = trimws(x[[idcol]])
  x = x[, -idcol, drop = FALSE]

  # Remove cols with all NA
  x = x[, !apply(is.na(x) | x == "", 2, all), drop = FALSE]

  x
}
