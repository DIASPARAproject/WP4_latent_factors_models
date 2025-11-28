custom_functions <- c(
  "f.classify.R",
  "f.extract.para.R",
  "f.lambda.plot.R",
  "f.factor.plot.R",
  "f.comb.factors.R",
  "f.comb.traceplot.R",
  "f.traceplot.R",
  "f.extract.pos.long.data.R",
  "f.extract.pos.R"
)

cat("Loading custom functions:\n")
for (func_file in custom_functions) {
  func_path <- file.path(path_functions, func_file)
  if (file.exists(func_path)) {
    source(func_path)
    cat("  ✓ Loaded:", func_file, "\n")
  } else {
    warning("Custom function not found: ", func_path)
  }
}