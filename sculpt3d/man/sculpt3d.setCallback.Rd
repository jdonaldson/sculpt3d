\name{sculpt3d.setCallback}
\Rdversion{1.1}
\alias{sculpt3d.setCallback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set a callback function for the GUI
}
\description{
Sets a callback function to be called after select/crop/delete commands.  
The callback function could be used to call/update additional plots using selection/current points in the current sculpt3d rgl plot.  

To disable the callback, simply pass NULL.
}
\usage{
sculpt3d.setCallback(callback = NULL)
}
\arguments{
\item{callback}{Callback function. The function should accept arguments of 'current' and 'selected' as logical vectors, and 'selected_color' as the selection color:\cr
\code{sculpt3d.setCallback(f = (current, selected, selected_color){...}) }}

}

%- maybe also 'usage' for other objects documented here.

\value{
Called for the side effect of calling the corresponding callback function passed as an argument.
}

\author{
Justin Donaldson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sculpt3d}},
}
\examples{
	\dontrun{
		x <- sort(rnorm(1000))
		y <- rnorm(1000)
		z <- rnorm(1000) + atan2(x,y)
		sculpt3d(x, y, z, labels=1:1000, col=rainbow(1000), type='s', radius=runif(1000)/5, alpha = .5)
		f = function(current, selected, selected_color) {
				plot(x[current],y[current],type='n') # set plot dimensions
				points(x[current & !selected],z[current & !selected]) # current points
				points(x[selected],z[selected], col=selected_color)   # selected points
		}
		sculpt3d.setCallback(f)
		# now select/crop/delete using the toolbar to see the auxiliary 2d plot update itself
	  }
	
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
