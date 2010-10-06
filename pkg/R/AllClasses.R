#setClass("stream",
#	representation(
#		name		= "character"
#	),
#
#	prototype(
#		name		="randomevents"
#	)
#)

setClass("clusterer",
	#contains("stream"),
	representation(
		algorithm	= "character"
		
	),

	prototype(
		algorithm	= "denstream"
	)
)
