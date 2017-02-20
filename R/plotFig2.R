plotConnections <- function(g, vdf)
{
    suppressMessages(library(Matrix))
    suppressMessages(library(lattice))

    vcol <- rainbow(4)

    (tab <- table(vdf$type))
    cumtab <- cumsum(c(0,tab))
    print(image(Matrix(g[]),lwd=0,aspect="fill",sub="",col.regions="grey70",xlab="neurons",ylab="neurons"))
    trellis.focus("panel", 1, 1, highlight=FALSE)
    for (i in 1:4) {
        lrect(xleft=0+cumtab[i],ybottom=0,xright=cumtab[i+1],ytop=cumtab[5],col=vcol[i],alpha=0.3)
        lrect(xleft=0,ybottom=0+cumtab[i],xright=cumtab[5],ytop=cumtab[i+1],col=vcol[i],alpha=0.3)
    }
    ltext(tab[1]/2,tab[1]/2,"KC → KC",srt=0,cex=2)
    ltext(cumtab[3]+tab[3]/2,cumtab[3]+tab[3]/2,"MBON \n↓ \nMBON",srt=0,cex=0.8)
    ltext(cumtab[2]+tab[2]/2,cumtab[2]/2,"KC → MBIN",srt=90,cex=1.2)
    ltext(cumtab[3]+tab[3]/2,cumtab[2]/2,"KC → MBON",srt=90,cex=1.2)
    ltext(tab[1]/2,cumtab[2]+tab[2]/2,"MBIN → KC",srt=0,cex=1.2)
    ltext(cumtab[3]+tab[3]/2,cumtab[2]+tab[2]/2,"MBIN \n↓ \nMBON",srt=0,cex=0.7)
    ltext(cumtab[2]+tab[2]/2,cumtab[3]+tab[3]/2,"MBON \n↓ \nMBIN",srt=0,cex=0.8)
    ltext(tab[1]/2,cumtab[4]+tab[4]/2,"PN → KC",srt=0,cex=2)
    trellis.unfocus()
}
