# This script defines some generally useful functions for calls from my other R codes.

suppressMessages(require(stringr));

# This function reads in a matrix (can be 1-row or 1-col) from a file in a "usual" way.
read_matrix = function(dir,sep=' ',header=F){
  dat = as.matrix(read.table(dir,colClasses="character",stringsAsFactors=F,sep=sep,header=header));
  for(i in 1:ncol(dat)){
    dat[,i] = str_replace_all(dat[,i],' ','');
    return(dat);
  }
}

# This function reads in a characteristic vector (please make sure it is 1-dimensional) from a file in a "usual" way.
read_vector = function(dir,sep=' ',header=F){
  dat = as.vector(as.matrix(read.table(dir,colClasses="character",stringsAsFactors=F,sep=sep,header=header)));
  dat = str_replace_all(dat,' ','');
  return(dat);
}

# This function makes an empty matrix with the same dimensions, column names and row names as the input matrix
sim_mat = function(mat){
    out = matrix(NA,nrow(mat),ncol(mat));
    rownames(out) = rownames(mat);
    colnames(out) = colnames(mat);
    return(out);
}


# Multiple plot function
# Courtesy and use example: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
