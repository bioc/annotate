pmid2MIAME = function (pmid) 
{
    require("annotate")
    require("XML")
    x = pubmed(pmid)
    rr = xmlRoot(x)
    top = xmlChildren(rr)
    pmart = top[["PubmedArticle"]]
    cit = xmlChildren(pmart)[["MedlineCitation"]]
    art = cit[["Article"]]
    cart = xmlChildren(art)
    title = xmlValue(cart[["ArticleTitle"]])
    abst = xmlValue(cart[["Abstract"]])
    aff = xmlValue(cart[["Affiliation"]])
    an = cart[["AuthorList"]]
    last = xmlValue(xmlChildren(an[[1]])[["LastName"]])
    ini = xmlValue(xmlChildren(an[[1]])[["Initials"]])
    new("MIAME", name=paste(last,ini,collapse=", "), lab = aff, title = title, abstract = abst, pubMedIds = pmid)
}
