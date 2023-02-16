# 1º a)

enem <- read.csv("./data/ENEM-AL-2019.csv", header = TRUE, dec = ".")

maceio <- subset(enem, enem$NO_MUNICIPIO_RESIDENCIA == "Maceió")

par(mfrow = c(1, 2))

a_cn <- c(maceio$NU_NOTA_CN)
a_ch <- c(maceio$NU_NOTA_CH)
a_lc <- c(maceio$NU_NOTA_LC)
a_mt <- c(maceio$NU_NOTA_MT)
a_red <- c(maceio$NU_NOTA_REDACAO)
a_geral <- c(maceio$NU_NOTA_ENEM)

boxplot(a_cn, a_ch, a_lc, a_mt, a_red, a_geral,
        main = "Notas do ENEM - Maceió",
        names = c("CN", "CH", "LC", "MT", "RED", "GERAL"),
        col = c("red", "yellow", "blue", "green", "purple", "cyan")
)


# Análise:
# Pela forma de como é mostrada pelo boxplot, as notas de matemática e redação
# aparentam ser mais fáceis de se alcançar uma nota alta, em comparação com as
# notas de linguagens e ch e cn.

# 1º b)

arapiraca <- subset(enem, enem$NO_MUNICIPIO_RESIDENCIA == "Arapiraca")

nota_enem <- c(arapiraca$NU_NOTA_ENEM)
classes <- c(
        "297 - 460",
        "460 - 515",
        "515 - 583",
        "583 - 769"
)

freq <- table(cut(nota_enem, breaks = 4, labels = classes))

freq_acum <- cumsum(freq)

freq_rel <- prop.table(freq)
freq_rel_acum <- cumsum(freq_rel)

freq_rel <- round(freq_rel * 100, 2)
freq_rel_acum <- round(freq_rel_acum * 100, 2)

tabela <- cbind(freq, freq_acum, freq_rel, freq_rel_acum)
print(tabela)

hist(nota_enem,
        breaks = c(297, 461, 515, 583, 800), labels = classes,
        xlim = c(300, 800),
        main = "Notas do ENEM - Arapiraca",
        xlab = "Notas",
        ylab = "Frequência",
        col = c("#74f71c", "#fffb00", "#ff7c02", "#f80f0f"),
)

# Análise:
# A partir da tabela, é possível perceber que a maior parte dos alunos de arapiraca tiraram notas entre 460 e 515, e a menor parte entre as extremidades (297 e 769).

# 1º c)

