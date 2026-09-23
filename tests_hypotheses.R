# Construction du tableau de contingence
tableau <- matrix(c(110, 884,   # Exposés : malades, non malades
                    281, 4219), # Non exposés : malades, non malades
                  nrow = 2, byrow = TRUE)

rownames(tableau) <- c("Exposé", "Non exposé")
colnames(tableau) <- c("Malade", "Non malade")

tableau
test <- chisq.test(tableau, correct = FALSE)
print(test)

# Effectifs théoriques (vérification des conditions d'application)
test$expected

qchisq(0.95, df = 1)

# Odds ratio
OR <- (110 * 4219) / (884 * 281)
OR

# Risque relatif (RR)
risque_expose <- 110 / (110 + 884)       # = 110/994
risque_non_expose <- 281 / (281 + 4219)  # = 281/4500

RR <- risque_expose / risque_non_expose
RR


##################""


# ============================================
# Étude cas-témoins : exposition vapeurs de diesel et cancer du poumon
# ============================================

# --- 1. Construction du tableau de contingence ---
a <- 200   # Exposé / Malade
b <- 373   # Exposé / Non malade
c <- 842   # Non exposé / Malade (1042 - 200)
d <- 1991  # Non exposé / Non malade (2364 - 373)

tableau2 <- matrix(c(a, b,
                    c, d),
                  nrow = 2, byrow = TRUE)

rownames(tableau2) <- c("Expose", "Non expose")
colnames(tableau2) <- c("Malade", "Non malade")

print(tableau2)

# --- 2. Test du chi2 ---
test <- chisq.test(tableau2, correct = FALSE)
print(test)

cat("\nEffectifs théoriques :\n")
print(test$expected)

# --- 3. Odds ratio et IC 95% ---
OR <- (a * d) / (b * c)
OR

ln_OR <- log(OR)
se_ln_OR <- sqrt(1/a + 1/b + 1/c + 1/d)

IC_inf <- exp(ln_OR - qnorm(0.975) * se_ln_OR)
IC_sup <- exp(ln_OR + qnorm(0.975) * se_ln_OR)

# --- 4. Résultats finaux ---
cat("\n=== RÉSULTATS ===\n")
cat("OR =", round(OR, 3), "\n")
cat("IC 95% OR = [", round(IC_inf, 3), ";", round(IC_sup, 3), "]\n")
cat("Chi2 =", round(test$statistic, 3), "\n")
cat("ddl =", test$parameter, "\n")
cat("p-value =", test$p.value, "\n")


a <- 200; b <- 373; c <- 842; d <- 1991

# Risque relatif
risque_expose <- a / (a + b)
risque_non_expose <- c / (c + d)
RR <- risque_expose / risque_non_expose
RR
# Variance de ln(RR)
var_lnRR <- b/(a*(a+b)) + d/(c*(c+d))
var_lnRR
se_lnRR <- sqrt(var_lnRR)

# IC 95%
IC_inf <- RR * exp(-1.96 * se_lnRR)
IC_sup <- RR * exp(1.96 * se_lnRR)

cat("RR =", round(RR, 3), "\n")
cat("IC 95% RR = [", round(IC_inf, 3), ";", round(IC_sup, 3), "]\n")
