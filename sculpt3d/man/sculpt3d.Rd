\name{sculpt3d}
\Rdversion{1.1}
\alias{sculpt3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The sculpt3d GUI for interactive 3d plot brushing and editing
}
\description{
Provides a simple toolbar GUI for interacting with 3D rgl plots.  The data to be plotted is provided to sculpt3d(), which calls relevant rgl plot3d() functions by proxy. A subset of the plot3d() arguments is accommodated.  See below for the list of acceptable parameters.
}
\usage{
sculpt3d(x, y = NULL, z = NULL, col = 'black', labels = NULL, radius = NULL, type = 'p', alpha = NULL, callback = NULL, size = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x, y, z}{coordinates. Any reasonable way of defining the
  coordinates is acceptable.  See the function \code{\link[grDevices]{xyz.coords}}
  for details.}
  \item{col}{
	the color to be used for plotted items.
}
  \item{labels}{
	the labels to be used for plotted items.
}
  \item{radius}{
	the radius to be used if \code{type='s'}: see Details below.
}
  \item{type}{
	the type of plot to generate.  Currently supported types include \code{'p'} for points, and \code{'s'} for spheres.
}
  \item{alpha}{
	the alpha transparency to use.
}
  \item{size}{
	the size for plotted points
}
  \item{callback}{
	the callback function to use.
}

}
\details{
	The above arguments are passed on to \code{\link[rgl]{plot3d}} for plotting.  However, rgl currently does not have a method of determining which data points are currently being represented in a given rgl device.  Therefore, it is necessary to first pass this data to \code{sculpt3d} to generate both the toolbar and the rgl plot.

Through the course of interaction with the toolbar, the data points become filtered by the \code{\link[rgl]{select3d}} function.  This type of filtering is not easily applicable for certain rgl plot methods (such as lines, meshes, quads, etc.), so only plots comprised of individual datapoints such as points and spheres are allowed for \code{sculpt3d}.
	
	The toolbar provides the following functionality:
\enumerate{
\item \bold{Selecting data points} : Clicking the \strong{Select3d} button will activate the \code{\link[rgl]{select3d}} mode in rgl.  Once clicked, it is possible to select data points in the current rgl plot.  These points will be colored by the current color indicated by the \strong{Color} button.
\item \bold{Changing color of selected data points} : Clicking the \strong{Color} button will change the highlight color of the selection mode.
\item \bold{Labeling data points} : This attaches text labels to each datapoint in the plot.  By default these are a simple enumeration of the datapoints, but it's possible to override this by passing a label argument.
\item \bold{Cropping data points} : Once data points are selected with \strong{Select3d}, it is possible to crop or delete them with the \strong{Crop} button.  The data is not actually deleted, it is just filtered from the current plot view.
\item \bold{Deleting data points} : The \strong{Delete} works similarly to the Crop Data Points button, except it deletes the currently selected points.
\item \bold{Resetting plot} : The \strong{Reset} button will reset the plot back to its original state.
\item \bold{Changing the mouse mode} : The three combo box controls at the bottom can change the mouseMode settings in \code{\link[rgl]{par3d}}.  See Warning section below.
}
	During interaction with the toolbar and plot, it is possible to retrieve a logic vector of the selected or visible items by calling \code{\link{sculpt3d.selected}} or \code{\link{sculpt3d.current}} resepectively.  Other rgl commands are still available, such as the \code{\link[rgl]{rgl.snapshot}} for saving plots as images.  
	
	If the rgl plot window is closed, the toolbar will throw an error and close the next time a button is clicked.

}

\section{Warning}{
It is strongly recommended to not call additional rgl plot functions on sculpt3d's current rgl device.  This can cause confuse the plotting, selection, and editing routines, and can even crash the toolbar.

The toolbar itself is generated from a GTK builder file, and is created as a window in GTK+'s toolbar window-hint mode.  This allows the toolbar to float on top of the OpenGL window on some platforms, and remain immediately active without the need to switch between applications in the window manager.  However, this has the unfortunate side effect of obscuring the combo box drop down list on some platforms.  Currently, the workaround is to select the combo box, and change the values with the up/down arrows until it shows the correct value.

In addition, the OSX version will throw several "invalid context" errors when sculpt3d is initialized for the first time.  This is a known issue with the RGTK2 bundle, and these errors do not affect the performance of the interface.
}

\value{
\code{sculpt3d} is called for the side effect of drawing the RGL plot and generating the GUI.  No value is returned.
}


\author{
Justin Donaldson jdonaldson@gmail.com
}




%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[rgl]{select3d}, \link[rgl]{plot3d}}

}
\examples{\dontrun{
	#run this directly with demo(sculpt3d)
	x <- sort(rnorm(1000))
	y <- rnorm(1000)
	z <- rnorm(1000) + atan2(x,y)
	sculpt3d(x, y, z, labels=1:1000, col=rainbow(1000), type='s', radius=runif(1000)/5, alpha = .5)
  }}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }

