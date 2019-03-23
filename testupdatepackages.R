
## modify this to have the script put the packages and fake repository file somewhere else.
## be sure to put the srcmanifest.csv file in that directory
bdir = "./"
chooseCRANmirror(ind=1L)

safecreate = function(dir, recursive = FALSE) if(!file.exists(dir)) dir.create(dir, recursive = recursive)

pkgrootdir = function(bdir, bigrepo = FALSE) {
    if(bigrepo)
        file.path(bdir, "pkgfiles", "bigrepo")
    else
        file.path(bdir, "pkgfiles")
}

makepkgdirs = function(bdir, bigrepo = FALSE) {
    ages = c("old", "new")
    typs = c("source", "mac.binary", "win.binary")

    bpkgdir = pkgrootdir(bdir, bigrepo)
    res = outer(ages, typs, function(a, ty) {
        mapply(function(ag, typ) data.frame(age = ag, type = typ, path = file.path(bpkgdir, typ, ag), stringsAsFactors = FALSE), SIMPLIFY=FALSE, ag = a, typ = ty)
    })
    dim(res) = NULL
    dirsdf = do.call(rbind.data.frame, res)
    
    sapply(dirsdf$path, safecreate, recursive = TRUE)
    dirsdf
}
getpackagefiles = function(bdir) {
    dirsdf = makepkgdirs(bdir)

## 3 versions each of XML and ggplot2 packages for each type, 18 package files total

    xmlurls = data.frame(url = c( "https://cran.r-project.org/src/contrib/Archive/XML/XML_3.96-0.1.tar.gz",
                                 "https://cran.r-project.org/src/contrib/Archive/XML/XML_3.98-1.6.tar.gz",
                                 "https://cran.r-project.org/src/contrib/Archive/XML/XML_3.98-1.17.tar.gz",
                                 "https://cran.r-project.org/bin/windows/contrib/3.1/XML_3.98-1.4.zip",
                                 "https://cran.r-project.org/bin/windows/contrib/3.3/XML_3.98-1.11.zip",
                                 "https://cran.r-project.org/bin/windows/contrib/3.5/XML_3.98-1.19.zip",
                                 "https://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/XML_3.98-1.4.tgz",
                                 "https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/XML_3.98-1.9.tgz",
                                 "https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.5/XML_3.98-1.19.tgz"),
                         age = rep(c("old", "new", "new"), times = 3),
                         type = rep(c("source", "win.binary", "mac.binary"), each = 3),
                         stringsAsFactors = FALSE)

    
    ggurls = data.frame(url = c("https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.8.3.tar.gz",
                                "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_1.0.0.tar.gz",
                                "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.0.0.tar.gz",
                                "https://cran.r-project.org/bin/windows/contrib/3.1/ggplot2_2.1.0.zip",
                                "https://cran.r-project.org/bin/windows/contrib/3.2/ggplot2_2.2.1.zip",
                                "https://cran.r-project.org/bin/windows/contrib/3.5/ggplot2_3.1.0.zip",
                                "https://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/ggplot2_2.1.0.tgz",
                                "https://cran.r-project.org/bin/macosx/mavericks/contrib/3.2/ggplot2_2.2.1.tgz",
                                "https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.5/ggplot2_3.1.0.tgz"),
                        age = rep(c("old", "new", "new"), times = 3),
                        type = rep(c("source", "win.binary", "mac.binary"), each = 3),
                        stringsAsFactors = FALSE)


    df = rbind.data.frame(xmlurls, ggurls)
    

    ##download files

    apply(df, 1, function(rw) {
    # rw is named character vector, such is life :(
        destdir = dirsdf$path[dirsdf$age == rw["age"] & dirsdf$type == rw["type"]]
        url = rw["url"]
        destfile = file.path(destdir, basename(url))
        if(!file.exists(destfile))
            download.file(url, destfile = destfile)
    })
}
  

winarchivelink = "https://cran.r-project.org/bin/windows/contrib/3.3/"
wincurlink = "https://cran.r-project.org/bin/windows/contrib/3.5/"

macarchivelink = "https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/"
maccurlink = "https://cran.r-project.org/bin/macosx/el-capitan/contrib/r-release/"


pkgurlsfromavl = function(avl, type) {
    ext = switch(type, source = ".tar.gz", mac.binary = ".tgz", win.binary = ".zip")
    ret = paste0(avl[,"Repository"], "/", avl[,"Package"], "_", avl[,"Version"], ext)
    names(ret) = avl[,"Package"]
    ret
}

dlifmissing = function(url, desdir) {
    desfile = file.path(desdir, basename(url))
    if(!file.exists(desfile))
        download.file(url, destfile = desfile)
    desfile
}

getbigrepofiles = function(bdir) {
    
    dirsdf = makepkgdirs(bdir, bigrepo = TRUE)
    manfile = file.path(bdir, "srcmanifest.csv")
    if(!file.exists(manfile)) { ## should never happen, I'm shipping srcmanifest.csv with the code
        library(switchr)
        man = cranPkgVersManifest("tidyverse", "1.1.1")
        write.csv(manifest_df(man), file = manfile)
    } else
        mandf = read.csv(manfile, stringsAsFactors = FALSE)
    
    urls = mandf$url
    pkgnames = mandf$name

    oldsrcdir = dirsdf[dirsdf$age == "old" & dirsdf$type == "source", "path"]
    newsrcdir = dirsdf[dirsdf$age == "new" & dirsdf$type == "source", "path"]
    lapply(urls, dlifmissing, desdir = oldsrcdir)
    
    
    avl = available.packages(type = "source")
    avl = as.data.frame(avl[pkgnames,], stringsAsFactors = FALSE)

    cururls = pkgurlsfromavl(avl, type = "source")
    lapply(cururls, dlifmissing,  desdir = newsrcdir)

    oldwinavl = available.packages(contriburl = winarchivelink)[pkgnames,]
    oldwinurls = pkgurlsfromavl(oldwinavl, type = "win.binary")
    oldwindir = dirsdf[dirsdf$age == "old" & dirsdf$type == "win.binary", "path"]
    newwindir = dirsdf[dirsdf$age == "new" & dirsdf$type == "win.binary", "path"]
    lapply(oldwinurls, dlifmissing, desdir = oldwindir)

    newwinavl = available.packages(contriburl = wincurlink)[pkgnames,]
    newwinurls = pkgurlsfromavl(newwinavl, type = "win.binary")
    lapply(newwinurls, dlifmissing, desdir = newwindir)


    oldmacdir = dirsdf[dirsdf$age == "old" & dirsdf$type == "mac.binary", "path"]
    newmacdir = dirsdf[dirsdf$age == "new" & dirsdf$type == "mac.binary", "path"]

    oldmacavl = available.packages(contriburl = macarchivelink)[pkgnames,]
    oldmacurls = pkgurlsfromavl(oldmacavl, type = "mac.binary")
    lapply(oldmacurls, dlifmissing, desdir = oldmacdir)

    newmacavl = available.packages(contriburl = maccurlink)[pkgnames,]
    newmacurls = pkgurlsfromavl(newmacavl, type = "mac.binary")
    lapply(newmacurls, dlifmissing, desdir = newmacdir)

    
    
}


##throughout, pf stands for "package file"

test_update_packages = function(type = "source",
                                bdir = file.path(tempdir(), "update_packages_testing"),
                                verbose.level = 0,
                                bigrepo = FALSE,
                                maxspeedup = FALSE) {
    library(tools) ## gross, I know
    ## download the package files if we don't have them already, will not
    ## download ones that we already have
    if(!bigrepo) 
        getpackagefiles(bdir = bdir)
    else
        getbigrepofiles(bdir = bdir)
    
    pfrootdir = pkgrootdir(bdir, bigrepo)
    stopifnot(file.exists(pfrootdir))
    pkpat = tools:::.get_pkg_file_pattern(type)
    oldpfs = list.files(file.path(pfrootdir, type, "old"), pattern = pkpat, full.names = TRUE)
    newpfs = list.files(file.path(pfrootdir, type, "new"), pattern = pkpat, full.names = TRUE)
    if(maxspeedup)
        newpfs = newpfs[which(basename(oldpfs) != basename(newpfs))[1]]
    repdir = normalizePath(file.path(bdir, "repodir", type))
    ## don't use any leftovers from previous tests
    if(file.exists(repdir))
        stopifnot(unlink(repdir, recursive = TRUE, force=TRUE) == 0)
    safecreate(repdir, recursive = TRUE)

    cat("checking for errors when dir is completely empty (expected warning suppressed) .....")
    ## make sure update_PACKAGES doesn't crap out on empty dir
    ## suppress warnings cause it is going to be called a bunch of times by
    ## the testing harness
    suppressWarnings(tools::update_PACKAGES(repdir, type = type, strict = FALSE))
    if(type != "win.binary") {
        unlink(list.files(repdir, pattern = "PACKAGES.*", full.names = TRUE))
        suppressWarnings(tools::update_PACKAGES(repdir, type = type, strict = TRUE))
    }
    cat("PASS\n")
    #put 'old' files in there
    file.copy(oldpfs, repdir)

    ## initial write_PACKAEGS call
    write_PACKAGES(repdir, type = type)

    indfile = file.path(repdir, "PACKAGES")
    wpresold = read.dcf(indfile)

    cat("*** Testing update_PACKAGES when dir contents haven't changed since write_PACKAGES\n")
    ## test update_PACKAGES with no new versions
    tools::update_PACKAGES(repdir, verbose.level = verbose.level, type = type, strict = FALSE)
    upresold = read.dcf(indfile)
    cat("\tchecking for identical result (type: ", type,", strict mode OFF)..........", sep="")
    stopifnot(identical(upresold, wpresold))
    cat("PASS\n")
    if(type != "win.binary") {
        
        tools::update_PACKAGES(repdir, strict = TRUE, verbose.level = verbose.level, type = type)
        upresold = read.dcf(indfile)
        cat("\tchecking for identical result (type: ", type,", strict mode ON)..........", sep="")
        stopifnot(identical(wpresold,upresold))
        cat("PASS\n")
    }
    allindfiles = list.files(repdir, pattern = "PACKAGES", full.names = TRUE)
    stopifnot(length(allindfiles) == 3) #don't get to theunlink if something is wrong
    indfilebackup = file.path(tempdir(), basename(allindfiles))
    if(any(file.exists(indfilebackup)))
        unlink(indfilebackup)
    file.copy(allindfiles, indfilebackup)
    ## put newer files in there
    file.copy(newpfs, repdir)

    cat("*** Testing for errors calling update_PACKAGES when all previous tarballs are missing (2-4 different configs)\n")
    ## tests for file being missing for out-of-date entry
    unlink(file.path(repdir, basename(oldpfs)))
    if(type != "win.binary") {
  
        cat("\tchecking (strict: TRUE, latestOnly:FALSE).......")
        tools::update_PACKAGES(repdir, type = type, strict = TRUE, verbose.level = verbose.level,
                               latestOnly = FALSE)
        cat("PASS\n")
        file.copy(indfilebackup, allindfiles, overwrite = TRUE)
        if(verbose.level > 0) cat("\n\n")
        cat("\tchecking (strict: TRUE, latestOnly:TRUE).......")
        tools::update_PACKAGES(repdir, type = type, strict = TRUE, verbose.level = verbose.level,
                    latestOnly = TRUE)
        cat("PASS\n")
    }
    file.copy(indfilebackup, allindfiles, overwrite = TRUE)
    if(verbose.level > 0) cat("\n\n")
    cat("\tchecking (strict: FALSE, latestOnly:FALSE).......")
    tools::update_PACKAGES(repdir, type = type, strict = FALSE, verbose.level = verbose.level,
                    latestOnly = FALSE)
    cat("PASS\n")
    file.copy(indfilebackup, allindfiles, overwrite = TRUE)
    if(verbose.level > 0) cat("\n\n")
    cat("\tchecking (strict: FALSE, latestOnly:TRUE).......")
    tools::update_PACKAGES(repdir, type = type, strict = FALSE, verbose.level = verbose.level,
                    latestOnly = TRUE)
    cat("PASS\n\n")
    file.copy(oldpfs, repdir)
    file.copy(indfilebackup, allindfiles, overwrite = TRUE)
    if(verbose.level > 0) cat("\n\n") 
    do_one_timing = function(strict, latestOnly, func) {
        chfunc = func
        if(is.character(func))
            func = get(func,envir = as.environment("package:tools"),  mode = "function")
        ## clear latest PACKAGES files, so update_PACKAGES will find enw things
        cpres = file.copy(from = indfilebackup, to = repdir, overwrite = TRUE)
        stopifnot(all(cpres))
        if(verbose.level > 0) cat("\n\n")
        if("strict" %in% names(formals(func))) { ## update_PACKAGES
            res = system.time(func(repdir, type = type,
                verbose.level = verbose.level, strict = strict, latestOnly  = latestOnly))
        } else { ## func is write_PACKAGES
            res = system.time(func(repdir, type = type,
                verbose = verbose.level > 0, latestOnly  = latestOnly))
        }
        res = as.data.frame(as.list(res)[1:3])
        res$func = chfunc
        res$strict = strict
        res$latestOnly = latestOnly
        content = as.data.frame(read.dcf(indfile), stringsAsFactors = FALSE)
        
        res$result = I(list(content))
        
        res
    }

    strvals = c(TRUE, FALSE)
    lavals = c(TRUE, FALSE)
    funs = c("write_PACKAGES", "update_PACKAGES")
    ctrldf = data.frame(func = rep(funs, each = 4),
                        strict = strvals, #natural recycle takes care of it
                        latestOnly = rep(rep(lavals, each = 2), 2),
                        stringsAsFactors = FALSE)
    ctrldf = ctrldf[!(ctrldf$strict & ctrldf$func == "write_PACKAGES"),]
    if(type == "win.binary")
        ctrldf = ctrldf[!ctrldf$strict,]
    
    ctrldf = rbind(ctrldf, ctrldf, ctrldf)
    
    resrows = mapply(do_one_timing, strict = ctrldf$strict, latestOnly = ctrldf$latestOnly, func = ctrldf$func, SIMPLIFY = FALSE)
    resdf = do.call(rbind.data.frame,c(make.row.names = FALSE, resrows))
    resdf = resdf[order(resdf$func, resdf$strict, resdf$latestOnly),]
    resdf$type = type
    resdf
}


check_print_one = function(df) {
    type = df$type[1]
    strict = df$strict[1]
    latestOnly = df$latestOnly[1]
    wpdf = df[df$func == "write_PACKAGES",]
    wpout = wpdf$content[[1]]
    wptiming = mean(wpdf$elapsed)
    cat("\ntype: ", type, " (latestOnly: ", latestOnly, ")\n")
    if(type != "win.binary") {
        cat("\tChecking for identical result with strict mode ON...............")
       
   
        updstrictdf = df[df$func == "update_PACKAGES" & df$strict,]
        stricttiming = mean(updstrictdf$elapsed)
        if(all(sapply(updstrictdf$content, function(x) identical(x, wpout)))) {
            cat("PASS\n")
            strpass = TRUE
        } else {
            cat("FAIL\n")
            strpass = FALSE
        }
    } else {
        strpass = TRUE
    }
    
    cat("\tChecking for identical result with strict mode OFF............")
    updfastdf = df[df$func == "update_PACKAGES" & !df$strict,]
    fasttiming = mean(updfastdf$elapsed)
    if(all(sapply(updfastdf$content, function(x) identical(x, wpout)))) {
        cat("PASS\n")
        fastpass = TRUE
    } else {
        cat("FAIL\n")
        fastpass = FALSE
    }

    cat("\tEstimating Speedups against write_PACKAGES\n")
    ## cat("\t\tStrict mode OFF (update_P, write_P): ", wptiming/fasttiming, "X (", fasttiming, ", ", wptiming, ")\n", sep="")
    cat(sprintf("\t\tStrict mode OFF (update_P, write_P): %.2fX\n", wptiming/fasttiming))
    if(type != "win.binary") 
        cat(sprintf("\t\tStrict mode ON (update_P, write_P): %.2fX\n", wptiming/stricttiming))
    
    strpass && fastpass

}

check_upd_results = function(resdf) {
    cat("\nTESTING update_PACKAGES performance and results against write_PACKAGES\n\n")
    lst = split(resdf, list(resdf$type, resdf$latestOnly))
    res = sapply(lst, check_print_one)

    if(!all(res))
        stop("At least one configuration had non-identical output for update_PACKAGES and write_PACKAGES")
    ## else
    ##     cat("FULL PASS (all tests).\n\n")
    res
}

do_it_all = function(bdir, bigrepo = TRUE) {
    cat("***************************************************************************************\n",
        "*** BEGIN TESTING update_PACKAGES                                                   ***\n",
        "***                                                                                 ***\n",
        "***************************************************************************************\n\n\n",
        sep="")
        
        
    if(bigrepo)
        cat("*** TESTING 'MODERATE' UPDATE (58 packages, 39 updated) ***\n\n")
    else
        cat("*** TESTING SMALL FOOTPRINT (2 packages, 2 new versions each) ***\n\n")

   
    
    reslst = lapply(c("source", "win.binary", "mac.binary"),
                    test_update_packages, bdir = bdir, verbose.level = 0, bigrepo = bigrepo)
    
    resdf = do.call(rbind.data.frame, reslst)

    res = check_upd_results(resdf)

    cat("*** PASSED ", sum(res), " OF ", length(res), "(", sum(res)/length(res)*100, "%) IDENTICAL RESULT TESTS ***\n\n\n", sep="") 
    if(bigrepo) {
        cat("*** TESTING INCREMENTAL UPDATE (58 packages, 1 updated) ***\n\n")
        reslst2 = lapply(c("source", "win.binary", "mac.binary"),
                         test_update_packages, bdir = bdir, verbose.level = 0, bigrepo = TRUE, maxspeedup=TRUE)
        
        resdf2 = do.call(rbind.data.frame, reslst2)
        
        res2 = check_upd_results(resdf2)
        cat("*** PASSED ", sum(res2), " OF ", length(res2), "(", sum(res2)/length(res2)*100, "%) IDENTICAL RESULT TESTS ***\n\n\n", sep="") 
        res = c(res, res2)
    }

    cat("***************************************************************************************\n",
        "*** TESTING COMPLETE                                                                ***\n",
        "*** ", if(all(res)) "FULL PASS" else {if(any(res)) sprintf("PARTIAL PASS (%d/%d)", sum(res), length(res)) else "FAIL"},"         ***\n",
        "***************************************************************************************\n\n\n",
        sep="")
}
                    

do_it_all(bdir = bdir)
