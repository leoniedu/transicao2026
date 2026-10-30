# Step 5: Duncan-Davis bounds (seção units) and lphom / tslphom / nslphom fitted per zona eleitoral (seção units),
# aggregated to UF and national by summing transition counts.
source(here::here("R", "functions.R"))

t1 <- read_proc("secao_t1")
t2 <- read_proc("secao_t2")
only_uf <- Sys.getenv("ONLY_UF")   # e.g. ONLY_UF=RR for a test run
if (nzchar(only_uf)) { t1 <- t1[uf %in% strsplit(only_uf, ",")[[1]]]; t2 <- t2[secao_id %in% t1$secao_id] }
n_cores <- 8L

run_spec <- function(spec_name) {
  s <- make_spec(t1, t2, specs[[spec_name]])
  zona <- paste(t1$uf, t1$nr_zona, sep = "_")

  # Bounds: national, UF, zona.
  b <- rbind(bounds_by(s, "BR")[, geo := "BR"], bounds_by(s, s$uf)[, geo := "UF"], bounds_by(s, zona)[, geo := "zona"])
  b[, spec := spec_name]

  idx <- split(seq_along(zona), zona)
  t0 <- Sys.time()
  fits <- parallel::mclapply(idx, function(i) fit_zona(s$X[i, , drop = FALSE], s$Y[i, , drop = FALSE], s$cd_municipio[i]),
                             mc.cores = n_cores)
  cat(sprintf("spec %s: %d zonas fitted in %s\n", spec_name, length(idx), format(Sys.time() - t0)))

  fail <- rbindlist(lapply(names(fits), function(z) {
    f <- fits[[z]]
    if (inherits(f, "try-error")) return(data.table(zona = z, method = "all", erro = as.character(f)))
    rbindlist(lapply(names(f$zona), function(m) if (is.character(f$zona[[m]])) data.table(zona = z, method = m, erro = f$zona[[m]])))
  }))
  if (nrow(fail)) { cat("failed fits:\n"); print(fail) }

  ok <- names(fits)[!vapply(fits, inherits, logical(1), "try-error")]
  zl <- rbindlist(lapply(ok, function(z) rbindlist(lapply(names(fits[[z]]$zona), function(m) {
    M <- fits[[z]]$zona[[m]]
    if (is.character(M)) return(NULL)
    d <- as.data.table(as.table(M)); setnames(d, c("row", "col", "votos")); d[, `:=`(zona = z, method = m)]
  }))))
  ml <- rbindlist(lapply(ok, function(z) rbindlist(lapply(names(fits[[z]]$muni), function(m)
    rbindlist(lapply(names(fits[[z]]$muni[[m]]), function(mu) {
      d <- as.data.table(as.table(fits[[z]]$muni[[m]][[mu]])); setnames(d, c("row", "col", "votos"))
      d[, `:=`(zona = z, cd_municipio = as.integer(mu), method = m)]
    }))))))
  zl[, `:=`(uf = sub("_.*", "", zona), spec = spec_name)]
  ml[, `:=`(uf = sub("_.*", "", zona), spec = spec_name)]

  # Each zona's counts reproduce both margins of that zona.
  for (m in unique(zl$method)) {
    chk_r <- merge(zl[method == m, .(fit = sum(votos)), by = .(zona, row)],
                   data.table(zona = zona, s$X)[, lapply(.SD, sum), by = zona] |>
                     melt(id.vars = "zona", variable.name = "row", value.name = "obs"), by = c("zona", "row"))
    chk_c <- merge(zl[method == m, .(fit = sum(votos)), by = .(zona, col)],
                   data.table(zona = zona, s$Y)[, lapply(.SD, sum), by = zona] |>
                     melt(id.vars = "zona", variable.name = "col", value.name = "obs"), by = c("zona", "col"))
    cat(sprintf("  %s: max |row margin error| %.3g, max |col margin error| %.3g votes\n", m,
                chk_r[, max(abs(fit - obs))], chk_c[, max(abs(fit - obs))]))
  }
  list(bounds = b, zona = zl, muni = ml, fail = fail)
}

res <- lapply(names(specs), run_spec)
bounds <- rbindlist(lapply(res, `[[`, "bounds"))
zona_est <- rbindlist(lapply(res, `[[`, "zona"))
muni_est <- rbindlist(lapply(res, `[[`, "muni"))
write_proc(bounds, "bounds")
write_proc(zona_est, "lp_zona")
write_proc(muni_est, "lp_municipio")

# Aggregate zona counts to UF and national; row-standardise.
agg <- function(d, by) {
  a <- d[, .(votos = sum(votos)), by = c(by, "method", "spec", "row", "col")]
  a[, p := votos / sum(votos), by = c(by, "method", "spec", "row")][]
}
nat <- agg(zona_est, character())[, geo := "BR"]
uf <- agg(zona_est, "uf")[, geo := uf]
est <- rbind(nat, uf, fill = TRUE)
for (g in unique(est$geo)) for (m in unique(est$method)) for (sp in unique(est$spec)) {
  fwrite(est[geo == g & method == m & spec == sp, .(row, col, p, votos)],
         out_table(sprintf("%s_%s_%s.csv", m, sp, g)))
}
write_proc(est, "lp_agregado")
fwrite(bounds[geo != "zona"], out_table("bounds_BR_UF.csv"))

cat("\nnational row-standardised transitions (%), seção units within zona:\n")
for (sp in names(specs)) for (m in lp_methods) {
  cat(sprintf("\n-- spec %s, %s\n", sp, m))
  print(round(100 * xtabs(p ~ row + col, nat[spec == sp & method == m]), 1))
}
cat("\nnational Duncan-Davis bounds (%):\n")
print(bounds[geo == "BR", .(spec, row, col, lo = round(100 * p_lo, 1), hi = round(100 * p_hi, 1))])
