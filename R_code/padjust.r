p=X111$p_value
adjusted=p.adjust(p,"BH",388)
write.table(adjusted,file="111.txt")