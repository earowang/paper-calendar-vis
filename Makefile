render:
	Rscript -e 'rmarkdown::render("main.Rmd")'

open:
	open main.pdf

response:
	Rscript -e 'rmarkdown::render("reviews/response.Rmd")'
