# 1º a)

enem <- read.csv("./data/ENEM-AL-2019.csv", header = TRUE, dec = ".")

maceio <- subset(enem, enem$NO_MUNICIPIO_RESIDENCIA == "Maceió")

a_cn <- c(maceio$NU_NOTA_CN)
a_ch <- c(maceio$NU_NOTA_CH)
a_lc <- c(maceio$NU_NOTA_LC)
a_mt <- c(maceio$NU_NOTA_MT)
a_red <- c(maceio$NU_NOTA_REDACAO)
a_geral <- c(maceio$NU_NOTA_ENEM)

boxplot(a_cn, a_ch, a_lc, a_mt, a_red, a_geral,
        main = "Notas do ENEM - Maceió",
        names = c("CN", "CH", "LC", "MT", "RED", "GERAL"),
        col = c(
                "#ADD8E6",
                "#87CEFA",
                "#00BFFF",
                "#1E90FF",
                "#6495ED",
                "#4682B4"
        )
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
# print(tabela)

hist(nota_enem,
        breaks = c(297, 461, 515, 583, 800), labels = classes,
        xlim = c(300, 800),
        main = "Notas do ENEM - Arapiraca",
        xlab = "Notas",
        ylab = "Frequência",
        col = c("#faddc3", "#ffba6c", "#FFA500", "#FF8C00")
)

# Análise:
# A partir da tabela, é possível perceber que a maior parte dos alunos de arapiraca tiraram notas entre 460 e 515, e a menor parte entre as extremidades (297 e 769).

# 1º c)

marechal <- subset(enem, enem$NO_MUNICIPIO_RESIDENCIA == "Marechal Deodoro")

cor_raca <- marechal$TP_COR_RACA
escolaridade <- marechal$TP_ESCOLA

dados <- with(marechal, table(cor_raca, escolaridade))

nomes_cor_raca <- c(
        "Não consta",
        "Branca",
        "Preta",
        "Parda",
        "Amarela",
        "Indígena"
)

nomes_escolaridade <- c("Pública", "Privada")

barplot(t(dados),
        main = "Escolaridade por cor/raça - Marechal Deodoro",
        xlab = "Cor/Raça",
        ylab = "Escolaridade",
        ylim = c(0, 100),
        col = c("#ec82b7", "#FFDAB9"),
        legend = nomes_escolaridade,
        names = nomes_cor_raca,
        beside = TRUE
)

# Análise:
# A partir do gráfico, é possível perceber que a maior parte dos alunos de Marechal Deodoro são pardos e que a maior parte são de escola pública.

# 1º d)