# Detect delimiter and table layout from the header, irrespective of extension.
detectTextFormat = function(file) {
  first = readLines(file, n = 1L, warn = FALSE)
  if(!length(first) || !nzchar(trimws(first)))
    stop2("Empty text file")

  header = sub("^\ufeff", "", first[1])
  sep = if(grepl("\t", header, fixed = TRUE)) "\t"
    else if(grepl(",", header, fixed = TRUE)) ","
    else
    stop2("Text file must have tab- or comma-separated column headers")

  fields = count.fields(file, sep = sep, quote = "\"", comment.char = "")
  if(length(fields) < 2L)
    stop2("Text file contains no data rows")
  if(anyNA(fields) || any(fields[-1L] > fields[1L]))
    stop2("Malformed text file: too many columns in a data row")

  cols = names(read.table(file, header = TRUE, sep = sep, nrows = 0,
                          check.names = FALSE, fill = TRUE, quote = "\"", comment.char = ""))
  # Ignore unnamed trailing columns.
  cols = cols[seq_len(max(c(0L, which(nzchar(trimws(cols))))))]

  if(length(cols) < 2L || any(!nzchar(trimws(cols))) || anyDuplicated(cols))
    stop2("Invalid text file header (missing or duplicate columns)")

  hdr = tolower(trimws(cols))
  isGM = "marker" %in% hdr && any(c("sample name", "allele 1", "allele 2") %in% hdr)
  isAM = length(hdr) >= 3L && startsWith(hdr[1], "family") && grepl("^\\[?(rel|kin)", hdr[2])

  type = if(isGM) "gm" else if(isAM) "am" else "geno"

  list(type = type, sep = sep)
}

readTextImport = function(file, flavour = c("AM", "PM")) {
  flavour = match.arg(flavour)
  fmt = detectTextFormat(file)
  if(fmt$type == "am") {
    if(flavour != "AM")
      stop2("Family/relationship files can only be imported as AM data")

    x = read.table(file, header = TRUE, sep = fmt$sep, colClasses = "character",
                   check.names = FALSE, fill = TRUE, quote = "\"", comment.char = "")
    if(!nrow(x)) stop2("No samples found in AM file")

    b = dvir::buildAM(x, mpFormat = "M")
    if(is.null(b$geno) || !nrow(b$geno))
      stop2("No genotypes found in AM file")

    return(list(data = b$geno, built = list(source = x, result = b), type = "am"))
  }
  data = if(fmt$type == "gm") readGenemapper(file, sep = fmt$sep) else
    readGenoFromTxt(file, sep = fmt$sep)
  list(data = data, built = NULL, type = fmt$type)
}

readGenemapper = function(file, format = NULL, sep = "\t") {
  x0 = read.table(file, header = TRUE, sep = sep, colClasses = "character",
                  check.names = FALSE, fill = TRUE, quote = "\"", comment.char = "")
  needed = c("Sample Name", "Marker", "Allele 1", "Allele 2")
  if(!all(needed %in% names(x0)))
    stop2("GeneMapper file requires columns: ", toString(needed))
  if(sum(names(x0) == "Marker") > 1L)
    stop2("Unsupported GeneMapper format (multiple markers per line)")
  if(!nrow(x0)) stop2("No samples found in GeneMapper file")

  x = x0[needed]
  names(x) = c("Sample", "Marker", "A1", "A2")
  if(anyNA(x$Sample) || anyNA(x$Marker) || any(!nzchar(x$Sample)) || any(!nzchar(x$Marker)))
    stop2("GeneMapper file has empty sample or marker names")

  ids = unique.default(x$Sample)
  markers = unique.default(x$Marker)
  if(!identical(x$Marker, rep(markers, length(ids))) ||
     !identical(x$Sample, rep(ids, each = length(markers))))
    stop2("GeneMapper markers are not consistently repeated for each sample")

  # Fill homozygotes and strip the GeneMapper -NB suffix.
  x$A1[is.na(x$A1)] = ""
  x$A2[is.na(x$A2)] = ""
  x$A2[x$A2 == ""] = x$A1[x$A2 == ""]
  x$A1 = sub("-NB", "", x$A1, fixed = TRUE)
  x$A2 = sub("-NB", "", x$A2, fixed = TRUE)

  geno = paste(x$A1, x$A2, sep = "/")
  geno[x$A1 == "" & x$A2 == ""] = ""
  gmat = matrix(geno, nrow = length(ids), ncol = length(markers),
                dimnames = list(ids, markers), byrow = TRUE)
  as.data.frame(gmat)
}

readGenoFromTxt = function(file, sep = "\t") {
  x = read.table(file, header = TRUE, sep = sep, colClasses = "character",
                 check.names = FALSE, row.names = NULL, strip.white = TRUE,
                 fill = TRUE, quote = "\"", comment.char = "")
  if(!nrow(x)) stop2("No samples found in genotype table")

  # Remove unnamed columns and identify sample IDs.
  x = x[nzchar(names(x))]
  nm = names(x)
  sample = grep("sample|^id", nm, ignore.case = TRUE)
  idcols = if(length(sample))
    c(grep("Family", nm, ignore.case = TRUE), sample[1L],
      grep("Relationship", nm, ignore.case = TRUE),
      grep("Kinship", nm, ignore.case = TRUE))
  else 1L
  if(!length(nm) || any(idcols > length(nm))) stop2("Missing sample ID column")

  if(anyNA(x[idcols])) stop2("Missing sample IDs in genotype table")
  ids = do.call(paste, c(lapply(x[idcols], trimws), list(sep = "_")))
  if(anyDuplicated(ids) || any(!nzchar(ids))) stop2("Empty or duplicated sample IDs")
  rownames(x) = ids
  x = x[-idcols]

  # Combine adjacent pairs of allele columns.
  nm = names(x)
  cols = grep("[-_. ][12]$", nm)
  if(length(cols) %% 2L) stop2("Unpaired allele columns in genotype table")
  if(length(cols)) {
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

  x = x[vapply(x, \(v) any(!is.na(v) & nzchar(v)), logical(1))]
  if(!length(setdiff(names(x), c("Sex", "Alias", "Fam"))))
    stop2("No genotype columns found in text file")
  x
}
