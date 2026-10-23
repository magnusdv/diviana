readGenemapper = function(file, format = NULL) {
  line1 = readLines(file, n = 1) |> strsplit("\t")
  line1 = line1[[1]]
  if("Marker" %notin% line1)
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
  x = read.delim(file, colClasses = "character", check.names = FALSE,
                 row.names = NULL, strip.white = TRUE, fill = TRUE)

  # Remove unnamed columns
  x = x[nzchar(names(x))]
  nm = names(x)

  # Identify sample ID columns
  sample = grep("sample|^id", nm, ignore.case = TRUE)
  idcols = if(length(sample))
    c(grep("Family", nm, ignore.case = TRUE), sample[1L],
      grep("Relationship", nm, ignore.case = TRUE),
      grep("Kinship", nm, ignore.case = TRUE))
  else 1L

  rownames(x) = do.call(paste, c(lapply(x[idcols], trimws), list(sep = "_")))
  x = x[-idcols]

  # Combine paired allele columns
  nm = names(x)
  cols = grep("[-_. ][12]$", nm)
  if(length(cols) > 0L && length(cols) %% 2L == 0L) {
    odd = seq.int(1L, length(cols), by = 2L)
    markers = substr(nm[cols], 1L, nchar(nm[cols]) - 2L)
    if(!identical(markers[odd], markers[odd + 1L]))
      stop2("Genotypes seem to be split in the wrong order")

    x[cols] = lapply(x[cols], \(v) replace(v, is.na(v) | v == "", "0"))
    geno = Map(\(a, b) paste(a, b, sep = "/"),
               x[cols[odd]], x[cols[odd + 1L]])
    names(geno) = markers[odd]
    x[names(geno)] = geno
    x = x[-cols]
  }

  # Remove completely empty columns
  x[vapply(x, \(v) any(!is.na(v) & nzchar(v)), logical(1))]
}
