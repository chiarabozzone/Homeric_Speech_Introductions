setwd("/Users/rpsandell/Documents/Dropbox/Homeric Speech Introductions Data")
library(stringi)

iliados = scan("IliadosEdit.txt", what="raw", sep="\n", fileEncoding = "UTF-8", encoding="UTF-8")
iliados = stri_trans_nfc(iliados)

odyssey = scan("OdusseiasEdit.txt", what="raw", sep="\n", fileEncoding = "UTF-8", encoding="UTF-8")
odyssey = stri_trans_nfc(odyssey)

proseeipe.iliad = c()

for(line in iliados){
  line = stri_trans_nfc(line)
  if(regexpr("προσέειπε", line) !=  -1){
    proseeipe.iliad = append(proseeipe.iliad, line)
  }
}
proseeipe.iliad.table = sort(table(proseeipe.iliad), decreasing=F)
write.table(as.data.frame(proseeipe.iliad.table), file="IliadProseeipeData.txt", row.names=T, col.names=F, quote=F, sep="\t")
# 30 tokens occur more than once
# Therefore, Baayen P = 45/75 = .6
  # More specifically, the Type 1 Construction (proseeipe beginning in the 2nd foot) occurs 61 times, 42 types, 29 HL: Baayen P = 0.4754098
    # Type 1.1 is aute + proseeipe (41) -- I count 42 tokens, 24 types, 12 HL: Baayen P = 0.2857143
    # Type 1.2 is proteros + proseeipe (10) -- right, 10 tokens, 9 types, 8 HL: Baayen P = .8
    # Type 1.3 is Obj.NP + proseeipe (5) -- right, right 5 tokens, 5 types, 5 HL: Baayen P = 1
    # Type 1.4 Other? (5) -- I count 4 tokens, 4 types, 4 HL
  # The Type 2 Construction (proseeipe at the end of the line) occurs 14 times
    # Type 2.1: ameibomenos proseeipe (2) -- right, 2 tokens
      # Type 2.1.1 = 2.2: ton d'aut' Subj.NP ameibomenos proseeipe -- shouldn't this be a subtype of the above? (1) -- right, 1 token
    # Type 2.3: Obj.NP proseeipe (4) -- I find only 5 tokens
    # Type 2.4 Other? (7) -- I find 6 tokens, 


prosephee.iliad = c()
for(line in iliados){
  line = stri_trans_nfc(line)
  if(regexpr("προσέφη ", line) !=  -1){
    prosephee.iliad = append(prosephee.iliad, line)
  }
}
prosephee.iliad.table = sort(table(prosephee.iliad), decreasing=F)
write.table(as.data.frame(prosephee.iliad.table), file="IliadProsepheeData.txt", row.names=T, col.names=F, quote=F, sep="\t")


proseeuda.iliad = c()

for(line in iliados){
  line = stri_trans_nfc(line)
  if(regexpr("προσηύδα", line) !=  -1){
    proseeuda.iliad = append(proseeuda.iliad, line)
  }
}
proseeuda.iliad.table = sort(table(proseeuda.iliad), decreasing=F)
write.table(as.data.frame(proseeuda.iliad.table), file="IliadProseeudaData.txt", row.names=T, col.names=F, quote=F, sep="\t")

## ODYSSEY


proseeipe.odyssey = c()
for(line in odyssey){
  line = stri_trans_nfc(line)
  if(regexpr("προσέειπε", line) !=  -1){
    proseeipe.odyssey = append(proseeipe.odyssey, line)
  }
}

proseeipe.odyssey.table = sort(table(proseeipe.odyssey), decreasing=F)
write.table(as.data.frame(proseeipe.odyssey.table), file="OdysseyProseeipeData.txt", row.names=T, col.names=F, quote=F, sep="\t")


prosephee.odyssey = c()
for(line in odyssey){
  line = stri_trans_nfc(line)
  if(regexpr("προσέφη ", line) !=  -1){
    prosephee.odyssey = append(prosephee.odyssey, line)
  }
}
prosephee.odyssey.table = sort(table(prosephee.odyssey), decreasing=F)
write.table(as.data.frame(prosephee.odyssey.table), file="OdysseyProsepheeData.txt", row.names=T, col.names=F, quote=F, sep="\t")


proseeuda.odyssey = c()

for(line in odyssey){
  line = stri_trans_nfc(line)
  if(regexpr("προσηύδα", line) !=  -1){
    proseeuda.odyssey = append(proseeuda.odyssey, line)
  }
}
proseeuda.odyssey.table = sort(table(proseeuda.odyssey), decreasing=F)
write.table(as.data.frame(proseeuda.odyssey.table), file="OdysseyProseeudaData.txt", row.names=T, col.names=F, quote=F, sep="\t")

### SIGNIFICANCE TESTING

# Iliad line tokens: 15668
# Iliad line types: 14500
# Iliad line hapaxes: 13663

# Odyssey line tokens: 12779
# Odyssey line types: 10457
# Odyssey line hapaxes: 9746



iliad.tokens <- 15668
iliad.types <- 14500
iliad.hapaxes <- 13663

odyssey.tokens <- 12779
odyssey.types <- 10457
odyssey.hapaxes <- 9746

proseeipe.1.1.types <- chisq.test(matrix(c(24, iliad.types-24, 19, odyssey.types - 19), byrow=T, nrow=2))
proseeipe.1.1.hapaxes <- chisq.test(matrix(c(14, iliad.hapaxes-14, 7, odyssey.hapaxes - 7), byrow=T, nrow=2))

proseeipe.1.2.types <- fisher.test(matrix(c(9, iliad.types-9, 1, odyssey.types - 1), byrow=T, nrow=2))
proseeipe.1.2.hapaxes <- fisher.test(matrix(c(8, iliad.hapaxes-8, 1, odyssey.hapaxes - 1), byrow=T, nrow=2))

proseeipe.1.3.types <- fisher.test(matrix(c(5, iliad.types-5, 2, odyssey.types - 2), byrow=T, nrow=2))
proseeipe.1.3.hapaxes <- fisher.test(matrix(c(5, iliad.hapaxes-5, 2, odyssey.hapaxes - 2), byrow=T, nrow=2))

proseeipe.1.4.types <- fisher.test(matrix(c(4, iliad.types-4, 1, odyssey.types - 1), byrow=T, nrow=2))
proseeipe.1.4.hapaxes <- fisher.test(matrix(c(4, iliad.hapaxes-4, 1, odyssey.hapaxes - 1), byrow=T, nrow=2))

proseeipe.Type1.types <- chisq.test(matrix(c(42, iliad.types-42, 23, odyssey.types - 23), byrow=T, nrow=2))
proseeipe.Type1.hapaxes <- chisq.test(matrix(c(31, iliad.hapaxes-31, 11, odyssey.hapaxes - 11), byrow=T, nrow=2))

proseeipe.2.1.types <- chisq.test(matrix(c(3, iliad.types-3, 15, odyssey.types - 15), byrow=T, nrow=2))
proseeipe.2.1.hapaxes <- chisq.test(matrix(c(3, iliad.hapaxes-3, 13, odyssey.hapaxes - 13), byrow=T, nrow=2))

proseeipe.2.2.types <- fisher.test(matrix(c(1, iliad.types-1, 4, odyssey.types - 4), byrow=T, nrow=2))
proseeipe.2.2.hapaxes <- fisher.test(matrix(c(1, iliad.hapaxes-1, 4, odyssey.hapaxes - 4), byrow=T, nrow=2))

proseeipe.2.4.types <- fisher.test(matrix(c(5, iliad.types-5, 0, odyssey.types - 0), byrow=T, nrow=2))
proseeipe.2.4.hapaxes <- fisher.test(matrix(c(5, iliad.hapaxes-5, 0, odyssey.hapaxes - 0), byrow=T, nrow=2))

proseeipe.Type2.types <- chisq.test(matrix(c(14, iliad.types-14, 23, odyssey.types - 23), byrow=T, nrow=2))
proseeipe.Type2.hapaxes <- chisq.test(matrix(c(14, iliad.hapaxes-14, 21, odyssey.hapaxes - 21), byrow=T, nrow=2))

proseeipe.3.1.types <- fisher.test(matrix(c(0, iliad.types-0, 4, odyssey.types - 4), byrow=T, nrow=2))
proseeipe.3.1.hapaxes <- fisher.test(matrix(c(0, iliad.hapaxes-0, 4, odyssey.hapaxes - 4), byrow=T, nrow=2))

proseeipe.3.2.types <- fisher.test(matrix(c(0, iliad.types-0, 1, odyssey.types - 1), byrow=T, nrow=2))
proseeipe.3.2.hapaxes <- fisher.test(matrix(c(0, iliad.hapaxes-0, 1, odyssey.hapaxes - 1), byrow=T, nrow=2))

proseeipe.Type3.types <- fisher.test(matrix(c(0, iliad.types-0, 5, odyssey.types - 5), byrow=T, nrow=2))
proseeipe.Type3.hapaxes <- fisher.test(matrix(c(0, iliad.hapaxes-0, 5, odyssey.hapaxes - 5), byrow=T, nrow=2))

proseeipe.overall.types <- chisq.test(matrix(c(56, iliad.types-56, 51, odyssey.types - 51), byrow=T, nrow=2))
proseeipe.overall.hapaxes <- chisq.test(matrix(c(45, iliad.hapaxes-45, 37, odyssey.hapaxes - 37), byrow=T, nrow=2))

### Still need to make proseephe and proseeuda corrections in the giant table

proseephe.overall.types <- chisq.test(matrix(c(70, iliad.types-70, 38, odyssey.types - 38), byrow=T, nrow=2))
proseephe.overall.hapaxes <- chisq.test(matrix(c(49, iliad.hapaxes-49, 27, odyssey.hapaxes - 27), byrow=T, nrow=2))

proseeuda.overall.types <- chisq.test(matrix(c(54, iliad.types-54, 36, odyssey.types - 36), byrow=T, nrow=2))
proseeuda.overall.hapaxes <- chisq.test(matrix(c(41, iliad.hapaxes-41, 26, odyssey.hapaxes - 26), byrow=T, nrow=2))

