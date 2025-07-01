#' @importFrom utils packageDescription
#' @importFrom pillar style_subtle
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://github.com/YuLab-SMU/aplot/issues", "\n\n")

    citation <- paste0("If you use ", pkgname,
                       " in published research, please cite the paper:\n\n",
                       aplot_citations())
    
    packageStartupMessage(paste0(strwrap(pillar::style_subtle(paste0(msg, citation, suppressmsg(pkgname)))), collapse="\n"))
}

aplot_citations <- function(){
    paste("Shuangbin Xu, Qianwen Wang, Shaodi Wen, Junrui Li, Nan He, Ming Li, Thomas Hackl, Rui Wang,
          Dongqiang Zeng, Shixiang Wang, Shensuo Li, Chunhui Gao, Lang Zhou, Shaoguo Tao, Zijing Xie,
          Lin Deng, and Guangchuang Yu.",
          "aplot: Simplifying the creation of complex graphs to visualize associations across diverse data types.",
          "The Innovation. 2025, 6(9):100958. doi: 10.1016/j.xinn.2025.100958\n\n",
          "Export the citation to BibTex by citation('aplot')\n\n"
    )    
    
}

suppressmsg <- function(pkgname){
    paste0("This message can be suppressed by:\n",
        "suppressPackageStartupMessages(library(", pkgname ,"))"
    )
}
