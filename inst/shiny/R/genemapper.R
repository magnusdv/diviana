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

  # Convert sample ID columns to row names
  trycols = list()
  for(a in c("Family", "Sample", "Relationship")) {
    i = grep(a, names(x), ignore.case = TRUE)
    trycols[[a]] = if(length(i) > 0) i else 0
  }
  trycols = unlist(trycols)
  if(trycols[2] > 0) {
    idcols = trycols[trycols > 0]
  } else {
    idcols = grep("sample", names(x), ignore.case = TRUE)
  }

  # If no luck, use first column
  if(length(idcols) == 0)
    idcols = 1

  args = lapply(idcols, function(i) trimws(x[[i]]))
  rownames(x) = do.call(paste, c(args, list(sep = "_")))
  x = x[, -idcols, drop = FALSE]

  # Check if there are 2 columns per marker
  splitcols = grep("[-_. ][12]$", names(x))
  if(length(splitcols) > 0 && length(splitcols) %% 2 == 0) {
    odd = seq_along(splitcols) %% 2 == 1
    col1 = splitcols[odd]
    col2 = splitcols[!odd]
    nms0 = substr(names(x)[splitcols], 1, nchar(names(x)[splitcols]) - 2)
    if(!all.equal(nms0[col1], nms0[col2])) {
      stop2("Genotypes seems to be split in the wrong order")
    }
    nms = nms0[odd]

    # Merge
    for(i in seq_along(nms)) {
      x[[nms[i]]] = paste(x[[col1[i]]], x[[col2[i]]], sep = "/")
    }
     # Remove split columns
    x = x[, -splitcols, drop = FALSE]
  }

  # Remove cols with all NA
  x = x[, !apply(is.na(x) | x == "", 2, all), drop = FALSE]

  x
}
