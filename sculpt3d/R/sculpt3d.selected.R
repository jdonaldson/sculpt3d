sculpt3d.selected <-
function(){
	if (!is.null(.local$current)) .local$selected & .local$current
	else NULL
}

