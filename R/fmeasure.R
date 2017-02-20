fmeasure <- function(predict, actual_labels) {
    precision <- sum(predict & actual_labels) / sum(predict)
    recall <- sum(predict & actual_labels) / sum(actual_labels)
    fmeasure <- 2 * precision * recall / (precision + recall)

#    cat('precision:  ')
#    cat(precision * 100)
#    cat('%')
#    cat('\n')
    
#    cat('recall:     ')
#    cat(recall * 100)
#    cat('%')
#    cat('\n')
    
#    cat('f-measure:  ')
#    cat(fmeasure * 100)
#    cat('%')
#    cat('\n')

    return(list(precision=precision, recall=recall, fmeasure=fmeasure))
}
